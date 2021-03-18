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
gather_members <- function(.fcst, member_regex = "_mbr[[:digit:]]+$|_mbr[[:digit:]]+_lag[[:graph:]]*$") {
  UseMethod("gather_members")
}

#' @export
gather_members.default <- function(.fcst, member_regex = "_mbr[[:digit:]]+$|_mbr[[:digit:]]+_lag[[:graph:]]*$") {

  class_in <- class(.fcst)

  required_colnames <- member_regex
  if (ncol(dplyr::select(.fcst, dplyr::matches(member_regex))) < 1) {
    stop(
      paste0("Input data frame must include columns with names containing: ", member_prefix)
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
