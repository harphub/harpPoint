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

  if (!is.null(groupings)) {
    groupings  <- rlang::syms(union("threshold", groupings))
  }
  parameter  <- rlang::enquo(parameter)

  if (!inherits(.fcst, "harp_ens_probs")) {
    .fcst   <- ens_probabilities(.fcst, thresholds, !! parameter)
  }

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

