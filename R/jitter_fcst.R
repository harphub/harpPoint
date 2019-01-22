#' Jitter a forecast to account for observation errors.
#'
#' To account for observation errors in ensemble verification the forecast
#' values can be perturbed by sampling from a specified error distribution.
#' Jittering the forecast is only likely to have an effect on the ensemble
#' spread and rank histograms.
#'
#' Note that the function is applied rowwise so for sampling functions n should
#' always be 1.
#'
#' @param .fcst An object of class 'harp_fcst'.
#' @param jitter_function The function to be applied to the forecast values.
#' @param fcst_suffix The suffix that identifies forecast values in the
#'   harp_fcst object. This will normally be "_mbr".
#'
#' @return An object of class harp_fcst with jittered forecast values.
#' @export
#'
#' @examples
#' \donttest{
#' jitter_fcst(fcst, function(x) x + rnorm(1, ))
#' }
jitter_fcst <- function(.fcst, jitter_function, fcst_suffix = "_mbr") {
  if (!is.function(jitter_function)) {
    stop("'jitter_function' must be a function", call. = FALSE)
  }
  UseMethod("jitter_fcst")
}

#' @export
jitter_fcst.default <- function(.fcst, jitter_function, fcst_suffix = "_mbr") {
  dplyr::mutate_at(.fcst, dplyr::vars(dplyr::contains(fcst_suffix)), ~ purrr::map_dbl(., jitter_function))
}

#' @export
jitter_fcst.harp_fcst <- function(.fcst, jitter_function, fcst_suffix = "_mbr") {
  purrr::map(.fcst, jitter_fcst, jitter_function, fcst_suffix) %>%
    new_harp_fcst()
}
