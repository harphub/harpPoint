#' Compute probabilities of threshold exceedence for ensemble forecasts
#'
#' @inheritParams ens_verify
#' @return A \code{harp_list} object with each data frame having columns for threshold,
#' fcst_prob and obs_prob instead of the columns for each member forecast.
#' @export
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

  meta_cols     <- grep("_mbr[[:digit:]]+", colnames(.fcst), invert = TRUE, value = TRUE) %>%
    .[!. %in% chr_param]
  meta_cols_sym <- rlang::syms(meta_cols)

  ens_probs <- dplyr::bind_cols(
    .fcst,
    harp_probs(.fcst, thresholds, chr_param, obs_prob = obs_probabilities)
  )


  fcst_thresh <- ens_probs %>%
    dplyr::select(!!! meta_cols_sym, dplyr::contains("fcst_prob")) %>%
    tidyr::gather(dplyr::contains("fcst_prob"), key = "threshold", value = "fcst_prob") %>%
    dplyr::mutate(threshold = readr::parse_number(.data$threshold))

  if (obs_probabilities) {

    join_cols  <- c(meta_cols, "threshold")

    obs_thresh <- ens_probs %>%
      dplyr::select(!!! meta_cols_sym, dplyr::contains("obs_prob")) %>%
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
ens_probabilities.harp_list <- function(.fcst, thresholds, parameter = NULL) {

  parameter   <- rlang::enquo(parameter)
#  if (!inherits(try(rlang::eval_tidy(parameter), silent = TRUE), "try-error")) {
#    if (is.character(rlang::eval_tidy(parameter))) {
#      parameter <- rlang::eval_tidy(parameter)
#      parameter <- rlang::ensym(parameter)
#    }
#  }

  purrr::map(.fcst, ens_probabilities, thresholds, !! parameter) %>%
    lapply(function(x) {
      x <- harpCore::as_harp_df(x)
      class(x) <- c("harp_ens_probs", class(x))
      x
    }) %>%
    harpCore::as_harp_list()
}

