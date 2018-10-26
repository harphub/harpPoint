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
#'
#' @return A data frame with data grouped for the \code{groupings} column(s) and
#'   columns for \code{brier_score}, \code{brier_skill_score} and the
#'   deomposition of the brier score - \code{brier_score_reliability},
#'   \code{brier_score_resolution} and \code{brier_score_uncertainty}.
#' @export
#'
#' @examples
ens_reliability <- function(.fcst, parameter, thresholds, groupings = "leadtime") {
  UseMethod("ens_reliability")
}

#' @export
ens_reliability.default <- function(.fcst, parameter, thresholds, groupings = "leadtime") {

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

  .fcst <- dplyr::inner_join(
    fcst_thresh,
    obs_thresh,
    by = join_cols
  )

  .fcst %>%
    dplyr::group_by(!!! groupings, !! thresh_col) %>%
    tidyr::nest(.key = "grouped_fcst") %>%
    dplyr::transmute(
      !!! groupings,
      !! thresh_col,
      brier_output = purrr::map(
        .data$grouped_fcst,
        ~ verification::brier(.x$obs_prob, .x$fcst_prob, show = FALSE)
      )
    ) %>%
    sweep_reliability()
}

#' @export
ens_reliability.harp_fcst <- function(.fcst, parameter, thresholds, groupings = "leadtime") {
  parameter <- rlang::enquo(parameter)
  purrr::map(.fcst, ens_reliability, !! parameter, thresholds, groupings) %>%
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
    tidyr::nest(.data$forecast_probability, .data$observed_frequency, .key = "reliability")
}
