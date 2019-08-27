#' Compute probabilities of threshold exceedence for ensemble forecasts
#'
#' @param .fcst A \code{harp_fcst} object with tables that have a column for
#'   observations, or a single forecast table.
#' @param parameter The name of the column for the observed data.
#' @param thresholds A numeric vector of thresholds for which to compute
#'   probabilities.
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
ens_probabilities <- function(.fcst, thresholds, parameter = NULL) {
  UseMethod("ens_probabilities")
}

#' @export
ens_probabilities.default <- function(.fcst, thresholds, parameter = NULL) {

  parameter  <- rlang::enquo(parameter)
  if (rlang::quo_is_null(parameter)) {
    chr_param         <- NA
    obs_probabilities <- FALSE
  } else {
    chr_param  <- rlang::quo_name(parameter)
    col_names  <- colnames(.fcst)
    if (length(grep(chr_param, col_names)) < 1) {
      stop(paste("No column found for", chr_param), call. = FALSE)
    }
    obs_probabilities <- TRUE
  }

  meta_cols  <- rlang::syms(c("SID", "fcdate", "leadtime", "validdate"))

  ens_probs <- dplyr::bind_cols(
    .fcst,
    harp_probs(.fcst, thresholds, chr_param, obs_prob = obs_probabilities)
  )


  fcst_thresh <- ens_probs %>%
    dplyr::select(!!! meta_cols, dplyr::contains("fcst_prob")) %>%
    tidyr::gather(dplyr::contains("fcst_prob"), key = "threshold", value = "fcst_prob") %>%
    dplyr::mutate(threshold = readr::parse_number(.data$threshold))

  if (obs_probabilities) {

    join_cols  <- c("SID", "fcdate", "leadtime", "validdate", "threshold")

    obs_thresh <- ens_probs %>%
      dplyr::select(!!! meta_cols, dplyr::contains("obs_prob")) %>%
      tidyr::gather(dplyr::contains("obs_prob"), key = "threshold", value = "obs_prob") %>%
      dplyr::mutate(threshold = readr::parse_number(.data$threshold))

    ens_probs <- dplyr::inner_join(fcst_thresh, obs_thresh, by = join_cols)

  } else {

    ens_probs <- fcst_thresh

  }

  num_members <- length(grep("_mbr[[:digit:]]+", colnames(.fcst)))

  class(ens_probs)               <- c("harp_ens_probs", class(ens_probs))
  attr(ens_probs, "num_members") <- num_members

  ens_probs

}

#' @export
ens_probabilities.harp_fcst <- function(.fcst, thresholds, parameter = NULL) {
  parameter <- rlang::enquo(parameter)
  purrr::map(.fcst, ens_probabilities, thresholds, !! parameter) %>%
    new_harp_fcst()
}

