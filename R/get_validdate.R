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
get_validdate.harp_list <- function(.fcst, max_min) {
  purrr::map(.fcst, get_validdate, max_min) %>%
    unlist() %>%
    as.numeric() %>%
    max_min() %>%
    as.character()
}

#' Return the first valid date in a forecast
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' As of version 0.1.0 reading data is no longer done with a start date and
#' end date, but rather a vector of date-time strings making these functions
#' obsolete. \code{\link[harpCore]{unique_valid_dttm}} is now the more
#' appropriate function to use.
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
first_validdate <- function(.fcst) {
  lifecycle::deprecate_stop(
    "0.0.1",
    "first_validdate()",
    "unique_valid_dttm()"
  )
  get_validdate(.fcst, min)
}


#' @rdname first_validdate
#' @export
#'
last_validdate  <- function(.fcst) {
  lifecycle::deprecate_stop(
    "0.0.1",
    "last_validdate()",
    "unique_valid_dttm()"
  )
  get_validdate(.fcst, max)
}

