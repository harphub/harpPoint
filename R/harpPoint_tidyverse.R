# tidyverse methods for harp_fcst objects

#' @export
filter.harp_fcst <- function(.fcst, ...) {
  out <- purrr::map(.fcst, dplyr::filter, ...)
  class(out) <- class(.fcst)
  out
}

