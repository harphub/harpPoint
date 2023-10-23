#' Convert EPS forecast data from wide format data frame to long format data
#' frame.
#'
#' @description
#'
#' `r lifecycle::badge("deprecated")`
#'
#' \code{\link[harpCore]{pivot_members}} is now the preferred method for
#' transforming between long and wide data frames since it supports classes
#' that were introduced in version 0.1.0.
#'
#' @param .fcst An EPS forecast data frame in wide format.
#' @param member_regex Regular expression for column names that contain forecasts for a
#'   single member.
#'
#' @return An EPS data frame in long format.
#' @export
#'
gather_members <- function(.fcst, member_regex = "_mbr[[:digit:]]+$|_mbr[[:digit:]]+_lag[[:graph:]]*$") {
  lifecycle::deprecate_warn(
    "0.1.0",
    "gather_members()",
    "pivot_members()"
  )
  UseMethod("gather_members")
}

#' @export
gather_members.default <- function(.fcst, member_regex = "_mbr[[:digit:]]+$|_mbr[[:digit:]]+_lag[[:graph:]]*$") {

  class_in <- class(.fcst)

  required_colnames <- member_regex
  if (ncol(dplyr::select(.fcst, dplyr::matches(member_regex))) < 1) {
    stop(
      "Input data frame must include columns with names containing regex: \"_mbr[[:digit:]]{3}\""
    )
  }

  .fcst <- tidyr::gather(
    .fcst,
    dplyr::matches(member_regex),
    key   = "member",
    value = "forecast"
  ) %>%
    dplyr::group_by(.data$member) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      sub_model = gsub(member_regex, "", .data$member),
      member    = gsub(paste0(.data$sub_model, "_"), "", .data$member)
    ) %>%
    tidyr::unnest(.data$data) %>%
    dplyr::ungroup()

  #.fcst <- .fcst %>% dplyr::mutate(
  #  member = stringr::str_extract(.data$member, paste0(gsub("_", "", member_prefix), "[[:graph:]]+"))
  #)

  structure(.fcst, class = class_in)
}

#' @export
gather_members.harp_fcst <- function(.fcst, member_regex = "_mbr[[:digit:]]+$|_mbr[[:digit:]]+_lag[[:graph:]]*$") {
  purrr::map(.fcst, gather_members, member_regex) %>%
    new_harp_fcst()
}


split_member_name <- function(mbr, mbr_regex) {
  regex_starts  <- regexpr(mbr_regex, mbr)
  mbr_start     <- regex_starts + 1
  sub_model_end <- regex_starts  - 1
  mapply_func   <- function(x, start, end) substr(x, start, end)
  list(
    sub_model = mapply(mapply_func, mbr, 1, sub_model_end, USE.NAMES = FALSE),
    member    = mapply(mapply_func, mbr, mbr_start, nchar(mbr), USE.NAMES = FALSE)
  )
}
