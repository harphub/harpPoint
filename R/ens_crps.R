#' Continuous Rank Probability Score (CRPS) for an ensemble.
#'
#' The CRPS and its decomposition are computed as columns in a \code{harp_fcst}
#' object. Typically the scores are aggregated over lead time, but other grouping
#' variables cam be chosen.
#'
#' @param .fcst A \code{harp_fcst} object with tables that have a column for
#'   observations, or a single forecast table.
#' @param parameter The name of the column for the observed data.
#' @param groupings The groups for which to compute the ensemble mean and
#'   spread. See \link[dplyr]{group_by} for more information of how grouping
#'   works.
#' @param keep_full_ouput Logical. Whether to keep the full output to computing
#' CRPS for ungrouped data.
#'
#' @return An object of the same format as the inputs but with data grouped for
#'   the \code{groupings} column(s) and columns for \code{crps}, \code{crps_pot}
#'   and \code{crps_rel}.
#' @export
#'
#' @examples
ens_crps <- function(.fcst, parameter, groupings = "leadtime", keep_full_output = FALSE) {
  UseMethod("ens_crps")
}

#' @export
ens_crps.default <- function(.fcst, parameter, groupings = "leadtime", keep_full_output = FALSE) {

  col_names   <- colnames(.fcst)
  parameter   <- rlang::enquo(parameter)
  chr_param   <- rlang::quo_name(parameter)
  groupings   <- rlang::syms(groupings)
  crps_output <- rlang::sym("crps_output")
  if (length(grep(chr_param, col_names)) < 1) {
    stop(paste("No column found for", chr_param), call. = FALSE)
  }

  .fcst %>%
    dplyr::group_by(!!! groupings) %>%
    tidyr::nest(.key = "grouped_fcst") %>%
    dplyr::transmute(
      !!! groupings,
      !! crps_output := purrr::map(grouped_fcst, harp_crps, !! parameter)
    ) %>%
    sweep_crps(crps_output, keep_full_output)
}

#' @export
ens_crps.harp_fcst <- function(.fcst, parameter, groupings = "leadtime", keep_full_output = FALSE) {
  parameter <- rlang::enquo(parameter)
  purrr::map(.fcst, ens_crps, !! parameter, groupings, keep_full_output) %>%
    dplyr::bind_rows(.id = "mname") %>%
    add_attributes(.fcst, !! parameter)
}

# Internal function to extract scores from the list output and add as columns to a data frame.
sweep_crps <- function(crps_df, crps_col, keep_full_output) {
  crps_col <- rlang::sym(crps_col)
  crps_df  <- crps_df %>%
    dplyr::mutate(
      crps             = purrr::map_dbl(!! crps_col, "CRPS"),
      cprs_potential   = purrr::map_dbl(!! crps_col, "CRPSpot"),
      crps_reliability = purrr::map_dbl(!! crps_col, "Reli")
    )
  if (!keep_full_output) {
    crps_df <- dplyr::select(crps_df, - !! crps_col)
  }
  crps_df
}
