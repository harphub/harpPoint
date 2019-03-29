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
gather_members <- function(.fcst, member_prefix = "_mbr") {
  UseMethod("gather_members")
}

#' @export
gather_members.default <- function(.fcst, member_prefix = "_mbr") {

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

  .fcst <- .fcst %>% dplyr::mutate(
    member = stringr::str_extract(.data$member, paste0(gsub("_", "", member_prefix), "[[:graph:]]+"))
  )

  .fcst
}

#' @export
gather_members.harp_fcst <- function(.fcst, member_prefix = "_mbr") {
  purrr::map(.fcst, gather_members, member_prefix) %>%
    new_harp_fcst()
}

