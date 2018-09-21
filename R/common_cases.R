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

  if (!is.element("dataframe_format", names(attributes(.fcst)))) {
    stop("Input forecast data frame does not have a dataframe_format attribute")
  }

  spread_df <- function(df) {
    dataframe_format <- attr(df, "dataframe_format")
    if (dataframe_format == "long") {
      spread_members(df)
    } else {
      df
    }
  }

  sites_and_dates <- .fcst %>%
    spread_df() %>%
    split(.$mname) %>%
    purrr::map(~ dplyr::filter(.x, leadtime == .x$leadtime[1])) %>%
    purrr::map(~ dplyr::select(.x, SID, fcdate))

  .fcst %>% dplyr::inner_join(Reduce(dplyr::inner_join, sites_and_dates))

}
