#' Pull station IDs from forecast or observations
#'
#' @param .fcst A harp_fcst object or data frame
#'
#' @return A vector of station IDs
#' @export
pull_stations <- function(.fcst) {
  UseMethod("pull_stations")
}

#' @export
pull_stations.default <- function(.fcst) {
  if (!is.element("SID", colnames(.fcst))) {
    return(integer())
  }
  unique(.fcst[["SID"]])
}

#' @export
pull_stations.harp_fcst <- function(.fcst) {
  sort(unique(unlist(lapply(.fcst, pull_stations))))
}
