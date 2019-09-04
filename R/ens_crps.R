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
#' @param num_ref_members The number of members for which to compute the fair
#'   CRPS.
#' @param keep_full_ouput Logical. Whether to keep the full output to computing
#' CRPS for ungrouped data.
#'
#' @return An object of the same format as the inputs but with data grouped for
#'   the \code{groupings} column(s) and columns for \code{crps}, \code{crps_pot}
#'   and \code{crps_rel}.
#' @export
#'
#' @examples
ens_crps <- function(.fcst, parameter, groupings = "leadtime", num_ref_members = NA, keep_full_output = FALSE, show_progress = FALSE) {
  UseMethod("ens_crps")
}

#' @export
ens_crps.default <- function(.fcst, parameter, groupings = "leadtime", num_ref_members = NA, keep_full_output = FALSE, show_progress = FALSE) {

  if (!is.list(groupings)) {
    groupings <- list(groupings)
  }

  col_names   <- colnames(.fcst)
  parameter   <- rlang::enquo(parameter)
  chr_param   <- rlang::quo_name(parameter)
  crps_output <- rlang::sym("crps_output")
  if (length(grep(chr_param, col_names)) < 1) {
    stop(paste("No column found for", chr_param), call. = FALSE)
  }

  crps_function <- function(df, parameter, show_progress) {
    parameter <- rlang::enquo(parameter)
    res       <- harp_crps(df, !! parameter)
    if (show_progress) {
      crps_progress$tick()
    }
    res
  }

  fair_crps <- function(df, parameter, R_new, show_progress) {
    parameter <- rlang::enquo(parameter)
    obs       <- dplyr::pull(df, !!parameter)
    fcst      <- as.matrix(
      df %>%
      dplyr::select(dplyr::contains("_mbr")) %>%
      dplyr::select_if(~sum(!is.na(.)) > 0)
    )
    res       <- mean(SpecsVerification::EnsCrps(fcst, obs, R.new = R_new))
    if (show_progress) {
      fair_crps_progress$tick()
    }
    res
  }

  if (show_progress) {
    progress_total <- sum(
      sapply(
        groupings,
        function(x) length(dplyr::group_rows(dplyr::group_by(.fcst, !!! rlang::syms(intersect(x, names(.fcst))))))
      )
    )
    crps_progress      <- progress::progress_bar$new(format = "  CRPS [:bar] :percent eta: :eta", total = progress_total)
    fair_crps_progress <- progress::progress_bar$new(format = "  Fair CRPS [:bar] :percent eta: :eta", total = progress_total)
  }

  compute_crps <- function(compute_group, fcst_df) {

    grouped_fcst <- group_without_threshold(fcst_df, compute_group) %>%
      tidyr::nest(.key = "grouped_fcst") %>%
      dplyr::mutate(
        num_cases       = purrr::map_int(.data$grouped_fcst, nrow),
        !! crps_output := purrr::map(.data$grouped_fcst, crps_function, !! parameter, show_progress)
      )

    if (!is.na(num_ref_members)) {
      grouped_fcst <- dplyr::mutate(
        grouped_fcst,
        fair_crps       = purrr::map_dbl(.data$grouped_fcst, fair_crps, !! parameter, num_ref_members, show_progress)
      )
    }

    grouped_fcst %>%
      dplyr::select(-.data$grouped_fcst) %>%
      sweep_crps(crps_output, keep_full_output)
  }

  purrr::map_dfr(groupings, compute_crps, .fcst) %>%
    fill_group_na(groupings)
}

#' @export
ens_crps.harp_fcst <- function(.fcst, parameter, groupings = "leadtime", num_ref_members = NA, keep_full_output = FALSE, show_progress = FALSE) {
  parameter <- rlang::enquo(parameter)
  list(
    ens_summary_scores = purrr::map(.fcst, ens_crps, !! parameter, groupings, num_ref_members, keep_full_output, show_progress) %>%
    dplyr::bind_rows(.id = "mname"),
    ens_threshold_scores = NULL
  ) %>%
    add_attributes(.fcst, !! parameter)
}

# Internal function to extract scores from the list output and add as columns to a data frame.
sweep_crps <- function(crps_df, crps_col, keep_full_output) {
  crps_col <- rlang::sym(crps_col)
  crps_df  <- crps_df %>%
    dplyr::mutate(
      crps             = purrr::map_dbl(!! crps_col, "CRPS"),
      crps_potential   = purrr::map_dbl(!! crps_col, "CRPSpot"),
      crps_reliability = purrr::map_dbl(!! crps_col, "Reli")
    )
  if (!keep_full_output) {
    crps_df <- dplyr::select(crps_df, - !! crps_col)
  }
  crps_df
}
