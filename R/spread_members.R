#' Convert EPS forecast data from long format data frame to wide format data
#' frame.
#'
#' @param .fcst An EPS forecast data frame in long format.
#'
#' @return An EPS data frame in wide format.
#' @export
#'
#' @examples
spread_members <- function(.fcst, ...) {
  UseMethod("spread_members")
}

#' @export
spread_members.default <- function(.fcst, model_name) {

  class_in <- class(.fcst)

  required_colnames <- c("member", "forecast")
  if (all(intersect(colnames(.fcst), required_colnames) != required_colnames)) {
    stop("Input data frame must include column names: member, forecast.")
  }

  if (is.element("sub_model", colnames(.fcst))) {
    .fcst <- dplyr::select(.fcst, -.data[["sub_model"]])
  }

  .fcst <- dplyr::mutate(.fcst, member = paste(model_name, .data$member, sep = "_"))
  .fcst <- tidyr::spread(.fcst, key = "member", value = "forecast")

  structure(.fcst, class = class_in)
}

#' @export
spread_members.harp_fcst <- function(.fcst, ...) {
  new_harp_fcst(purrr::map2(.fcst, names(.fcst), spread_members))
}
