#' Convert EPS forecast data from long format data frame to wide format data
#' frame.
#'
#' @param FCST An EPS forecast data frame in long format.
#'
#' @return An EPS data frame in wide format.
#' @export
#'
#' @examples
spread_members <- function(FCST) {

  if (!is.element("dataframe_format", names(attributes(FCST)))) {
    stop("The dataframe_format attribute must be present and equal to 'long'")
  }

  if (attr(FCST, "dataframe_format") != "long") {
    stop("Input data frame must be long format")
  }

  required_colnames <- c("member", "forecast")
  if (intersect(colnames(FCST), required_colnames) != required_colnames) {
    stop("Input data frame must include column names: member, forecast.")
  }

  FCST <- tidyr::spread(FCST, key = "member", value = "forecast")
  attr(FCST, "dataframe_format") <- "wide"

  FCST
}
