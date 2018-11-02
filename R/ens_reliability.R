#' Reliability for an ensemble.
#'
#' @param .fcst A \code{harp_fcst} object with tables that have a column for
#'   observations, or a single forecast table.
#' @param parameter The name of the column for the observed data.
#' @param thresholds A numeric vector of thresholds for which to compute the
#'   reliability.
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
#'   a nested column for reliability. The column can be unnested with
#'   \link[tidyr]{unnest}.
#' @export
#'
#' @examples
ens_reliability <- function(.fcst, parameter, thresholds, groupings = "leadtime", climatology = "sample") {
  UseMethod("ens_reliability")
}

#' @export
ens_reliability.default <- function(.fcst, parameter, thresholds, groupings = "leadtime", climatology = "sample") {

  groupings  <- rlang::syms(groupings)
  parameter  <- rlang::enquo(parameter)
  meta_cols  <- rlang::quos(c(SID, fcdate, leadtime, validdate))
  thresh_col <- rlang::sym("threshold")
  join_cols  <- c("SID", "fcdate", "leadtime", "validdate", "threshold")


  .fcst   <- ens_probabilities(.fcst, !! parameter, thresholds)

  fcst_thresh <- .fcst %>%
    dplyr::select(!!! meta_cols, dplyr::contains("fcst_prob")) %>%
    tidyr::gather(dplyr::contains("fcst_prob"), key = "threshold", value = "fcst_prob") %>%
    dplyr::mutate(!! thresh_col := readr::parse_number(!! thresh_col))

  obs_thresh <- .fcst %>%
    dplyr::select(!!! meta_cols, dplyr::contains("obs_prob")) %>%
    tidyr::gather(dplyr::contains("obs_prob"), key = "threshold", value = "obs_prob") %>%
    dplyr::mutate(!! thresh_col := readr::parse_number(!! thresh_col))

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
    dplyr::group_by(!!! groupings, !! thresh_col) %>%
    tidyr::nest(.key = "grouped_fcst") %>%
    dplyr::transmute(
      !!! groupings,
      !! thresh_col,
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
        ~ unique(.x$bss_ref_climatology)
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
    sweep_reliability()
}

#' @export
ens_reliability.harp_fcst <- function(.fcst, parameter, thresholds, groupings = "leadtime", climatology = "sample") {
  parameter   <- rlang::enquo(parameter)
  climatology <- get_climatology(.fcst, !! parameter, thresholds, climatology)
  purrr::map(.fcst, ens_reliability, !! parameter, thresholds, groupings, climatology) %>%
    dplyr::bind_rows(.id = "mname") %>%
    add_attributes(.fcst, !! parameter)
}

sweep_reliability <- function(brier_df) {
  brier_col <- rlang::quo(brier_output)
  brier_df %>%
    dplyr::mutate(
      forecast_probability = purrr::map(!! brier_col, "y.i"),
      observed_frequency   = purrr::map(!! brier_col, "obar.i"),
      proportion_occurred  = purrr::map(!! brier_col, "prob.y")
    ) %>%
    dplyr::select(-!! brier_col) %>%
    tidyr::unnest() %>%
    tidyr::nest(
      .data$forecast_probability,
      .data$observed_frequency,
      .data$proportion_occurred,
      .key = "reliability"
    )
}
