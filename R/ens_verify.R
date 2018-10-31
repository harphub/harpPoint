#' Compute all verification scores for an ensemble.
#'
#' @param .fcst A \code{harp_fcst} object with tables that have a column for
#'   observations, or a single forecast table.
#' @param parameter The name of the column for the observed data.
#' @param thresholds A numeric vector of thresholds for which to compute the
#'   threshold based scores. Set to NULL (the default) to only compute summary
#'   scores.
#' @param groupings The groups for which to compute the scores. See
#'   \link[dplyr]{group_by} for more information of how grouping works.
#'
#' @return A list containting two data frames: \code{ens_summary_scores} and
#'   \code{ens_threshold_scores}.
#' @export
#'
#' @examples
ens_verify <- function(.fcst, parameter, thresholds = NULL, groupings = "leadtime") {
  UseMethod("ens_verify")
}

#' @export
ens_verify.default <- function(.fcst, parameter, thresholds = NULL, groupings = "leadtime") {

  col_names <- colnames(.fcst)
  parameter <- rlang::enquo(parameter)
  chr_param <- rlang::quo_name(parameter)
  groupings <- rlang::syms(groupings)
  crps_out  <- rlang::sym("crps_output")

  if (length(grep(chr_param, col_names)) < 1) {
    stop(paste("No column found for", chr_param), call. = FALSE)
  }

  .fcst <- ens_mean_and_var(.fcst, mean_name = "ens_mean", var_name = "ens_var")

  ens_summary_scores <- .fcst %>%
    dplyr::group_by(!!! groupings) %>%
    tidyr::nest(.key = "grouped_fcst") %>%
    dplyr::transmute(
      !!! groupings,
      num_cases    = purrr::map_int(grouped_fcst, nrow),
      mean_bias    = purrr::map_dbl(grouped_fcst, ~ mean(.x$ens_mean - .x[[chr_param]])),
      rmse         = purrr::map_dbl(grouped_fcst, ~ sqrt(mean((.x$ens_mean - .x[[chr_param]]) ^ 2))),
      stde         = purrr::map_dbl(grouped_fcst, ~ sd(.x$ens_mean - .x[[chr_param]])),
      spread       = purrr::map_dbl(grouped_fcst, ~ sqrt(mean(.x$ens_var))),
      rank_count   = purrr::map(grouped_fcst, harp_rank_hist, !! parameter),
      !! crps_out := purrr::map(grouped_fcst, harp_crps, !! parameter)
    ) %>%
    sweep_crps(crps_out, FALSE) %>%
    sweep_rank_histogram()

  if (is.numeric(thresholds)) {

    join_cols  <- c("SID", "fcdate", "leadtime", "validdate", "threshold")
    meta_cols  <- rlang::quos(c(SID, fcdate, leadtime, validdate))
    thresh_col <- rlang::sym("threshold")

    .fcst <- ens_probabilities(.fcst, !! parameter, thresholds)

    fcst_thresh <- .fcst %>%
      dplyr::select(!!! meta_cols, dplyr::contains("fcst_prob")) %>%
      tidyr::gather(dplyr::contains("fcst_prob"), key = "threshold", value = "fcst_prob") %>%
      dplyr::mutate(!! thresh_col := readr::parse_number(!! thresh_col))

    obs_thresh <- .fcst %>%
      dplyr::select(!!! meta_cols, dplyr::contains("obs_prob")) %>%
      tidyr::gather(dplyr::contains("obs_prob"), key = "threshold", value = "obs_prob") %>%
      dplyr::mutate(!! thresh_col := readr::parse_number(!! thresh_col))

    .fcst <- dplyr::inner_join(
      fcst_thresh,
      obs_thresh,
      by = join_cols
    )

    ens_threshold_scores <- .fcst %>%
      dplyr::group_by(!!! groupings, !! thresh_col) %>%
      tidyr::nest(.key = "grouped_fcst") %>%
      dplyr::transmute(
        !!! groupings,
        !! thresh_col,
        brier_output = purrr::map(
          .data$grouped_fcst,
          ~ verification::brier(.x$obs_prob, .x$fcst_prob)
        ),
        economic_value = purrr::map(
          .data$grouped_fcst,
          ~ harp_ecoval(.x$obs_prob, .x$fcst_prob)
        ),
        roc_output = purrr::map(
          .data$grouped_fcst,
          ~ harp_roc(.x$obs_prob, .x$fcst_prob)
        ),
        climatology = purrr::map_dbl(
          .data$grouped_fcst,
          ~ sum(.x$obs_prob) / nrow(.x)
        ),
        total_num_cases = purrr::map_int(
          .data$grouped_fcst,
          ~ sum(as.integer(.x$obs_prob) | as.integer(ceiling(.x$fcst_prob)))
        ),
        observed_num_cases = purrr::map_int(
          .data$grouped_fcst,
          ~ sum(as.integer(.x$obs_prob))
        ),
        forecast_num_cases = purrr::map_int(
          .data$grouped_fcst,
          ~ sum(as.integer(ceiling(.x$fcst_prob)))
        )
      ) %>%
      sweep_brier_output() %>%
      sweep_roc()

  } else {

    ens_threshold_scores <- tibble::tibble(
      leadtime                = integer(),
      threshold               = numeric(),
      economic_value          = list(),
      brier_score             = numeric(),
      brier_skill_score       = numeric(),
      brier_score_reliability = numeric(),
      brier_score_resolution  = numeric(),
      brier_score_uncertainty = numeric(),
      reliability             = list(),
      roc                     = list(),
      roc_area                = numeric()
    )

  }

  list(ens_summary_scores = ens_summary_scores, ens_threshold_scores = ens_threshold_scores)

}

#' @export
ens_verify.harp_fcst <- function (.fcst, parameter, thresholds = NULL, groupings = "leadtime") {
  parameter   <- rlang::enquo(parameter)
  list_result <- purrr::map(.fcst, ens_verify, !! parameter, thresholds, groupings)
  list(
    ens_summary_scores   = dplyr::bind_rows(
      purrr::map(list_result, "ens_summary_scores"),
      .id = "mname"
    ),
    ens_threshold_scores = dplyr::bind_rows(
      purrr::map(list_result, "ens_threshold_scores"),
      .id = "mname"
    )
  ) %>% add_attributes(.fcst, !! parameter)

}

# Internal function to clean up the output from the brier function.
sweep_brier_output <- function(ens_threshold_df) {

  brier_output_col <- rlang::quo(brier_output)

  brier_df <- ens_threshold_df %>%
    dplyr::mutate(
      brier_score             = purrr::map_dbl(!! brier_output_col, "bs"),
      brier_skill_score       = purrr::map_dbl(!! brier_output_col, "ss"),
      brier_score_reliability = purrr::map_dbl(!! brier_output_col, "bs.reliability"),
      brier_score_resolution  = purrr::map_dbl(!! brier_output_col, "bs.resol"),
      brier_score_uncertainty = purrr::map_dbl(!! brier_output_col, "bs.uncert"),
      forecast_probability    = purrr::map(!! brier_output_col, "y.i"),
      observed_frequency      = purrr::map(!! brier_output_col, "obar.i"),
      proportion_occurred     = purrr::map(!! brier_output_col, "prob.y")
    ) %>%
    dplyr::select(-!! brier_output_col) %>%
    tidyr::unnest(
      .data$forecast_probability,
      .data$observed_frequency,
      .data$proportion_occurred) %>%
    tidyr::nest(
      .data$forecast_probability,
      .data$observed_frequency,
      .data$proportion_occurred,
      .key = "reliability"
    )

  dplyr::inner_join(
    dplyr::select(ens_threshold_df, -!! brier_output_col),
    brier_df,
    by = c(
      "leadtime",
      "threshold",
      "climatology",
      "total_num_cases",
      "observed_num_cases",
      "forecast_num_cases"
    )
  )
}

# Internal function to add forecast attributes to a verification output
add_attributes <- function(.verif, .fcst, parameter) {
  parameter <- rlang::enquo(parameter)

  dates     <- unlist(purrr::map(.fcst, "fcdate"))
  SIDs      <- unlist(purrr::map(.fcst, "SID"))

  attr(.verif, "parameter")    <- rlang::quo_name(parameter)
  attr(.verif, "start_date")   <- harpIO::unixtime_to_str_datetime(min(dates), YMDh)
  attr(.verif, "end_date")     <- harpIO::unixtime_to_str_datetime(max(dates), YMDh)
  attr(.verif, "num_stations") <- length(unique(SIDs))

  .verif
}
