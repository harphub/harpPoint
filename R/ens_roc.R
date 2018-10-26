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
  purrr::map(.fcst, ens_roc, !! parameter, thresholds, groupings) %>%
    dplyr::bind_rows(.id = "mname") %>%
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
