#' Title
#'
#' @param .fcst A harp3 format data frame of forecasts
#'
#' @return The input data frame with only the common stations and forecast dates
#'   for each experiment selected.
#' @export
#'
#' @examples
common_cases <- function(.fcst) {

  join_columns    <- c("SID", "fcdate", "leadtime")
  select_columns  <- rlang::syms(join_columns)
  sites_and_dates <- join_models(
    dplyr::select(.fcst, !!! select_columns),
    by = join_columns,
    name = "common_cases"
  )

  join_to_fcst(.fcst, sites_and_dates$common_cases, by = join_columns)

}
