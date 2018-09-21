#' Convert EPS forecast data from long format data frame to wide format data
#' frame.
#'
#' @param .fcst An EPS forecast data frame in long format.
#'
#' @return An EPS data frame in wide format.
#' @export
#'
#' @examples
spread_members <- function(.fcst) {

  if (!is.element("dataframe_format", names(attributes(.fcst)))) {
    stop("The dataframe_format attribute must be present and equal to 'long'")
  }

  if (attr(.fcst, "dataframe_format") != "long") {
    stop("Input data frame must be long format")
  }

  required_colnames <- c("member", "forecast")
  if (intersect(colnames(.fcst), required_colnames) != required_colnames) {
    stop("Input data frame must include column names: member, forecast.")
  }

  .fcst <- tidyr::spread(.fcst, key = "member", value = "forecast")
  attr(.fcst, "dataframe_format") <- "wide"

  .fcst
}
