#' Rank histogram for an ensemble.
#'
#' The rank histogram is computed as columns in a \code{harp_fcst} object.
#' Typically the scores are aggregated over lead time, but other grouping
#' variables cam be chosen.
#'
#' @param .fcst A \code{harp_fcst} object with tables that have a column for
#'   observations, or a single forecast table.
#' @param parameter The name of the column for the observed data.
#' @param groupings The groups for which to compute the ensemble mean and
#'   spread. See \link[dplyr]{group_by} for more information of how grouping
#'   works.
#'
#' @return An object of the same format as the inputs but with data grouped for
#'   the \code{groupings} column(s) and columns for \code{rank} and
#'   \code{rank_count} that are nested together in a column with the name
#'   \code{rank_histogram}.
#' @export
#'
#' @examples
ens_rank_histogram <- function(.fcst, parameter, groupings = "leadtime") {
  UseMethod("ens_rank_histogram")
}

#' @export
ens_rank_histogram.default <- function(.fcst, parameter, groupings = "leadtime") {

  col_names  <- colnames(.fcst)
  parameter  <- rlang::enquo(parameter)
  chr_param  <- rlang::quo_name(parameter)
  groupings  <- rlang::syms(groupings)
  if (length(grep(chr_param, col_names)) < 1) {
    stop(paste("No column found for", chr_param), call. = FALSE)
  }

  .fcst %>%
    dplyr::group_by(!!! groupings) %>%
    tidyr::nest(.key = "grouped_fcst") %>%
    dplyr::transmute(
      !!! groupings,
      rank_count = purrr::map(grouped_fcst, harp_rank_hist, !! parameter)
    ) %>%
    sweep_rank_histogram(groupings)

}

#' @export
ens_rank_histogram.harp_fcst <- function(.fcst, parameter, groupings = "leadtime") {
  parameter = rlang::enquo(parameter)
  purrr::map(.fcst, ens_rank_histogram, !! parameter, groupings) %>%
    dplyr::bind_rows(.id = "mname")
}

# Internal function to return nicely formatted column for ens_rank_histogram.
sweep_rank_histogram <- function(rank_hist_df) {
  nest_cols <- rlang::syms(c("rank", "rank_count"))
  rank_hist_df %>%
    dplyr::mutate(
      rank = purrr::map(rank_count, ~ seq(1, length(.x)))
    ) %>%
    tidyr::unnest() %>%
    tidyr::nest(!!! nest_cols, .key = "rank_histogram")
}
