#' Filter to common cases
#'
#' For a fair comparison of models, the verification should only be done for
#' dates and locations that are common to all models. \code{common_cases} takes
#' a harp_fcst object as input and then identifies and filters to only those
#' cases that are common to all of the forecast models in the harp_fcst object.
#'
#' @param .fcst A harp_fcst object
#'
#' @return The input data frame with only the common stations and forecast dates
#'   for each forecast model selected.
#' @export
#'
#' @examples
common_cases <- function(.fcst) {

  join_columns    <- c("SID", "fcdate", "leadtime")
  select_columns  <- rlang::syms(join_columns)
  sites_and_dates <- join_models(
    dplyr::select(.fcst, !!! select_columns),
    by         = join_columns,
    name       = "common_cases"
  )

  suppressMessages(
    suppressWarnings(
      join_to_fcst(.fcst, sites_and_dates$common_cases, by = join_columns, force_join = TRUE)
    )
  )

}
