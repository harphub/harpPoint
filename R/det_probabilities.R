#' Compute binary probabilities for deterministic forecasts
#'
#' @param .fcst A \code{harp_list} object with `harp_det_point_df` deta frames,
#'   or a `harp_det_point_df` data frame.
#' @param parameter The name of the column for the observed data.
#' @param thresholds A numeric vector of thresholds for which to compute
#'   probabilities.
#' @param obs_probabilities A logical indicating whether or not to compute the
#'   binary probabilities for the observations.
#'
#' @return An object of the same class as `.fcst` with columns for threshold,
#' fcst_prob and optionally obs_prob instead of the raw forecast column.
det_probabilities <- function(.fcst, parameter, thresholds, obs_probabilities = TRUE) {
  UseMethod("det_probabilities")
}

#' @export
det_probabilities.harp_det_point_df <- function(.fcst, parameter, thresholds, obs_probabilities = TRUE) {

  parameter  <- rlang::enquo(parameter)
  chr_param  <- rlang::quo_name(parameter)
  col_names  <- colnames(.fcst)
  if (length(grep(chr_param, col_names)) < 1) {
    stop(paste("No column found for", chr_param), call. = FALSE)
  }

  dplyr::bind_cols(
    .fcst,
    harp_probs(.fcst, thresholds, chr_param, obs_prob = obs_probabilities, fcst_type = "det")
  )

}

#' @export
det_probabilities.harp_list <- function(.fcst, parameter, thresholds, obs_probabilities = TRUE) {
  parameter   <- rlang::enquo(parameter)
  if (!inherits(try(rlang::eval_tidy(parameter), silent = TRUE), "try-error")) {
    if (is.character(rlang::eval_tidy(parameter))) {
      parameter <- rlang::eval_tidy(parameter)
      parameter <- rlang::ensym(parameter)
    }
  }
  purrr::map(.fcst, det_probabilities, !! parameter, thresholds, obs_probabilities) %>%
    new_harp_fcst()
}

