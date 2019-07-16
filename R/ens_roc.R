#' Relative Operating Characteristics for an ensemble.
#'
#' @param .fcst A \code{harp_fcst} object with tables that have a column for
#'   observations, or a single forecast table.
#' @param parameter The name of the column for the observed data.
#' @param thresholds A numeric vector of thresholds for which to compute the
#'   ROC.
#' @param groupings The groups for which to compute the ROC. See
#'   \link[dplyr]{group_by} for more information of how grouping works.
#'
#' @return A data frame with data grouped for the \code{groupings} column(s) and
#'   a nested column for the ROC with each row containing a data frame with
#'   columns: \code{prob} for the forecast probability bin, \code{HR} for the
#'   hit rate and \code{FAR} for the false alarm rate. Use \link[tidyr]{unnest}
#'   to unnest to the nested column.
#' @export
#'
#' @examples
ens_roc <- function(.fcst, parameter, thresholds, groupings = "leadtime") {
  UseMethod("ens_roc")
}

#' @export
ens_roc.default <- function(.fcst, parameter, thresholds, groupings = "leadtime") {

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
      roc_output = purrr::map(
        .data$grouped_fcst,
        ~ harp_roc(.x$obs_prob, .x$fcst_prob)
      )
    ) %>%
    sweep_roc()

}

#' @export
ens_roc.harp_fcst <- function(.fcst, parameter, thresholds, groupings = "leadtime") {
  parameter <- rlang::enquo(parameter)
  list(
    ens_summary_scores = NULL,
    ens_threshold_scores = purrr::map(.fcst, ens_roc, !! parameter, thresholds, groupings) %>%
    dplyr::bind_rows(.id = "mname")
  )%>%
    add_attributes(.fcst, !! parameter)
}

sweep_roc <- function(roc_df) {
  roc_col <- rlang::quo(roc_output)
  roc_df %>%
    dplyr::mutate(
      roc      = purrr::map(!! roc_col, "roc_data"),
      roc_area = purrr::map_dbl(!! roc_col, "roc_area")
    ) %>%
    dplyr::select(- !! roc_col)
}
