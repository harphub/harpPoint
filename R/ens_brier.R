#' Brier score and its decomposition for an ensemble.
#'
#' @param .fcst A \code{harp_fcst} object with tables that have a column for
#'   observations, or a single forecast table.
#' @param parameter The name of the column for the observed data.
#' @param thresholds A numeric vector of thresholds for which to compute the
#'   Brier Score.
#' @param groupings The groups for which to compute the ensemble mean and
#'   spread. See \link[dplyr]{group_by} for more information of how grouping
#'   works.
#' @param climatology The climatology to use for the Brier Skill Score. Can be
#'   "sample" for the sample climatology (the default), a named list with
#'   elements eps_model and member to use a member of an eps model in the
#'   harp_fcst object for the climatology, or a data frame with columns for
#'   threshold and climatology and also optionally leadtime.
#'
#' @return A data frame with data grouped for the \code{groupings} column(s) and
#'   columns for \code{brier_score}, \code{brier_skill_score} and the
#'   deomposition of the brier score - \code{brier_score_reliability},
#'   \code{brier_score_resolution} and \code{brier_score_uncertainty}.
#' @export
#'
#' @examples
ens_brier <- function(.fcst, parameter, thresholds, groupings = "leadtime", climatology = "sample") {
  UseMethod("ens_brier")
}

#' @export
ens_brier.default <- function(.fcst, parameter, thresholds, groupings = "leadtime", climatology = "sample") {

  groupings  <- rlang::syms(union("threshold", groupings))
  parameter  <- rlang::enquo(parameter)
  meta_cols  <- rlang::syms(c("SID", "fcdate", "leadtime", "validdate"))
  join_cols  <- c("SID", "fcdate", "leadtime", "validdate", "threshold")


  .fcst   <- ens_probabilities(.fcst, !! parameter, thresholds)

  fcst_thresh <- .fcst %>%
    dplyr::select(!!! meta_cols, dplyr::contains("fcst_prob")) %>%
    tidyr::gather(dplyr::contains("fcst_prob"), key = "threshold", value = "fcst_prob") %>%
    dplyr::mutate(threshold = readr::parse_number(.data$threshold))

  obs_thresh <- .fcst %>%
    dplyr::select(!!! meta_cols, dplyr::contains("obs_prob")) %>%
    tidyr::gather(dplyr::contains("obs_prob"), key = "threshold", value = "obs_prob") %>%
    dplyr::mutate(threshold = readr::parse_number(.data$threshold))

  .fcst <- dplyr::inner_join(fcst_thresh, obs_thresh, by = join_cols)

  if (inherits(climatology, "data.frame")) {
    if (all(c("leadtime", "threshold") %in% names(climatology))) {
      join_cols <- c("leadtime", "threshold")
    } else {
      join_cols <- "threshold"
    }
    .fcst <- dplyr::inner_join(.fcst, climatology, by = join_cols) %>%
      dplyr::rename(bss_ref_climatology = .data$climatology)
  }

  brier_function <- function(df) {
    if (is.element("climatology", names(df))) {
      verification::brier(df$obs_prob, df$fcst_prob, baseline = unique(df$climatology))
    } else {
      verification::brier(df$obs_prob, df$fcst_prob)
    }
  }

  .fcst %>%
    dplyr::group_by(!!! groupings) %>%
    tidyr::nest(.key = "grouped_fcst") %>%
    dplyr::transmute(
      !!! groupings,
      brier_output = purrr::map(
        .data$grouped_fcst,
        brier_function
      ),
      sample_climatology = purrr::map_dbl(
        .data$grouped_fcst,
        ~ sum(.x$obs_prob) / nrow(.x)
      ),
      bss_ref_climatology = purrr::map_dbl(
        .data$grouped_fcst,
        ~ mean(.x$bss_ref_climatology)
      ),
      num_cases_for_threshold_total = purrr::map_int(
        .data$grouped_fcst,
        ~ sum(as.integer(.x$obs_prob) | as.integer(ceiling(.x$fcst_prob)))
      ),
      num_cases_for_threshold_observed = purrr::map_int(
        .data$grouped_fcst,
        ~ sum(as.integer(.x$obs_prob))
      ),
      num_cases_for_threshold_forecast = purrr::map_int(
        .data$grouped_fcst,
        ~ sum(as.integer(ceiling(.x$fcst_prob)))
      )
    ) %>%
    sweep_brier()
}

#' @export
ens_brier.harp_fcst <- function(.fcst, parameter, thresholds, groupings = "leadtime", climatology = "sample") {
  parameter   <- rlang::enquo(parameter)
  climatology <- get_climatology(.fcst, !! parameter, thresholds, climatology)
  list(
    ens_summary_scores = NULL,
    ens_threshold_scores = purrr::map(.fcst, ens_brier, !! parameter, thresholds, groupings, climatology) %>%
    dplyr::bind_rows(.id = "mname")
  ) %>%
    add_attributes(.fcst, !! parameter)
}

sweep_brier <- function(brier_df) {
  brier_col <- rlang::quo(brier_output)
  brier_df %>%
    dplyr::mutate(
      brier_score             = purrr::map_dbl(!! brier_col, "bs"),
      brier_skill_score       = purrr::map_dbl(!! brier_col, "ss"),
      brier_score_reliability = purrr::map_dbl(!! brier_col, "bs.reliability"),
      brier_score_resolution  = purrr::map_dbl(!! brier_col, "bs.resol"),
      brier_score_uncertainty = purrr::map_dbl(!! brier_col, "bs.uncert")
    ) %>%
    dplyr::select(-!! brier_col)
}
