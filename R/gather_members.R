#' Convert EPS forecast data from wide format data frame to long format data
#' frame.
#'
#' @param .fcst An EPS forecast data frame in wide format.
#' @param member_prefix Prefix for column names that contain forcasts for a
#'   single member. The default is "mbr". Note that the column name only has to
#'   contain \code{member_prefix}.
#'
#' @return An EPS data frame in long format.
#' @export
#'
#' @examples
gather_members <- function(.fcst, member_prefix = "mbr") {

  if (!is.element("dataframe_format", names(attributes(.fcst)))) {
    stop("The dataframe_format attribute must be present and equal to 'wide'")
  }

  if (attr(.fcst, "dataframe_format") != "wide") {
    stop("Input data frame must be long format")
  }

  required_colnames <- member_prefix
  if (ncol(dplyr::select(.fcst, dplyr::contains(member_prefix))) < 1) {
    stop(
      paste0("Input data frame must include columns with names containing: ", member_prefix)
    )
  }

  .fcst <- tidyr::gather(
    .fcst,
    dplyr::contains(member_prefix),
    key   = "member",
    value = "forecast"
  )

  attr(.fcst, "dataframe_format") <- "long"

  .fcst
}
