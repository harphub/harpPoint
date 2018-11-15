#' @export
get_validdate <- function(.fcst, max_min) {
  UseMethod("get_validdate")
}

#' @export
get_validdate.default <- function(.fcst, max_min) {
  .fcst[["validdate"]] %>%
    max_min() %>%
    harpIO::unixtime_to_str_datetime(harpIO::YMDhm)
}

#' @export
get_validdate.harp_fcst <- function(.fcst, max_min) {
  purrr::map(.fcst, get_validdate, max_min) %>%
    unlist() %>%
    as.numeric() %>%
    max_min() %>%
    as.character()
}

#' Return the first valid date in a forecast
#'
#' The function is intended to be used to find the first date for which to fetch
#' observations against which to verify.
#'
#' @param .fcst A \code{harp_fcst} object or a table containing a column named
#'   validdate with the data in unix time format.
#'
#' @return The first valid time in YYYYMMDDhhmm format for the input object.
#' @export
#'
#' @examples
first_validdate <- function(.fcst) {
  get_validdate(.fcst, min)
}

#' Return the last valid date in a forecast
#'
#' The function is intended to be used to find the last date for which to fetch
#' observations against which to verify.
#'
#' @param .fcst A \code{harp_fcst} object or a table containing a column named
#'   validdate with the data in unix time format.
#'
#' @return The first valid time in YYYYMMDDhhmm format for the input object.
#' @export
#'
#' @examples
last_validdate  <- function(.fcst) {
  get_validdate(.fcst, max)
}

