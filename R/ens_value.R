#' Economic value for an ensemble.
#'
#' @param .fcst A \code{harp_fcst} object with tables that have a column for
#'   observations, or a single forecast table.
#' @param parameter The name of the column for the observed data.
#' @param thresholds A numeric vector of thresholds for which to compute the
#'   economic value.
#' @param groupings The groups for which to compute the economic value. See
#'   \link[dplyr]{group_by} for more information of how grouping works.
#'
#' @return A data frame with data grouped for the \code{groupings} column(s) and
#'   a nested column for the economic value with each row containing a data
#'   frame with columns: \code{cl} for cost loss, and \code{value} for the
#'   economic value. Use \link[tidyr]{unnest} to unnest to the nested column.
#' @export
#'
#' @examples
ens_value <- function(.fcst, parameter, thresholds, groupings = "leadtime") {
  UseMethod("ens_value")
}

#' @export
ens_value.default <- function(.fcst, parameter, thresholds, groupings = "leadtime") {

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

  .fcst <- dplyr::inner_join(
    fcst_thresh,
    obs_thresh,
    by = join_cols
  )

  .fcst %>%
    dplyr::group_by(!!! groupings) %>%
    tidyr::nest(.key = "grouped_fcst") %>%
    dplyr::transmute(
      !!! groupings,
      economic_value = purrr::map(
        .data$grouped_fcst,
        ~ harp_ecoval(.x$obs_prob, .x$fcst_prob)
      )
    )

}

#' @export
ens_value.harp_fcst <- function(.fcst, parameter, thresholds, groupings = "leadtime") {
  parameter <- rlang::enquo(parameter)
  list(
    ens_summary_scores = NULL,
    ens_threshold_scores = purrr::map(.fcst, ens_value, !! parameter, thresholds, groupings) %>%
    dplyr::bind_rows(.id = "mname")
  ) %>%
    add_attributes(.fcst, !! parameter)
}

