#' Expand date columns into year, month, day, hour, minute.
#'
#' @param .fcst A harp_fcst object or a data frame with a column with the string
#'   'date' in its name.
#' @param date_col The unquoted name of the date column that you wish to expand.
#'
#' @return An object of the same type as '.fcst' with extra columns for year,
#'   month, day, hour, minute. The extra colun names are prefixed with
#'   'date_col' with the string 'date' removed.
#' @export
#'
#' @examples
expand_date <- function(.fcst, date_col) {
  date_col_quo  <- rlang::enquo(date_col)
  date_col_name <- rlang::quo_name(date_col_quo)
  if (!grepl("date", date_col_name)) {
    stop("The column name for 'date_col' must contain the string 'date'", call. = FALSE)
  }
  if (!any(purrr::map_lgl(dplyr::pull(.fcst, !! date_col_quo), is.numeric))) {
    stop("'date_col' must be numeric. It is assumed to be in unix epoch format", call. = FALSE)
  }

  prefix        <- gsub("date", "", date_col_name)
  year_col      <- rlang::sym(paste0(prefix, "_year"))
  month_col     <- rlang::sym(paste0(prefix, "_month"))
  day_col       <- rlang::sym(paste0(prefix, "_day"))
  hour_col      <- rlang::sym(paste0(prefix, "_hour"))
  minute_col    <- rlang::sym(paste0(prefix, "_minute"))

  dplyr::mutate(
    .fcst,
    !! year_col   := harpIO::unix2datetime(!!date_col_quo) %>% lubridate::year(),
    !! month_col  := harpIO::unix2datetime(!!date_col_quo) %>% lubridate::month(),
    !! day_col    := harpIO::unix2datetime(!!date_col_quo) %>% lubridate::day(),
    !! hour_col   := harpIO::unix2datetime(!!date_col_quo) %>% lubridate::hour(),
    !! minute_col := harpIO::unix2datetime(!!date_col_quo) %>% lubridate::minute()
  )
}
