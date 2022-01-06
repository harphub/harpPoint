#' Select members from a harp_fcst object
#'
#' @param .fcst A harp_fcst object
#' @param members The members to select. Can be a numeric vector, or a named
#'   list to select members from spcific forecast models in the harp_fcst
#'   object.
#'
#' @return A harp_fcst object.
#' @export
#'
#' @examples
select_members <- function(.fcst, members, include_lagged_members = TRUE) {

  stopifnot(inherits(.fcst, "harp_fcst"))

  if (is.list(members)) {

    if (is.null(names(members))) {

      if (length(members) == 1 && length(.fcst) > 1) {
        message("Members only supplied for one forecast model. Recycling members for all forecast models.")
        members <- rep(members, length(.fcst))
      } else if (length(members) != length(.fcst)) {
        stop(
          paste(
            "Members supplied for", length(members), "forecast models",
            "when there are ", length(.fcst), "forecast models."
          ),
          call. = FALSE
        )
      } else {
        warning("No forecast model names supplied for members. Assuming they are in the correct order.", immediate. = TRUE, call. = FALSE)
        names(members) <- names(.fcst)
      }

    } else {
      bad_names <- setdiff(names(members), names(.fcst))
      if (length(bad_names) > 0) {
        stop(paste(bad_names, collapse = ", "), " not found in .fcst", call. = FALSE)
      }
    }

  } else {

    if (length(.fcst) > 1) {
      message("Members only supplied for one forecast model. Recycling members for all forecast models.")
    }
    members <- lapply(seq_along(.fcst), function(x) members)
    names(members) <- names(.fcst)
  }

  .fcst[names(members)] <- purrr::map2(.fcst[names(members)], members, member_select, include_lagged_members)
  .fcst

}

member_select <- function(df, members, lag_inc) {
  suffix    <- ifelse(lag_inc, "", "$")
  meta_cols <- grep("_mbr[[:digit:]]", colnames(df), invert = TRUE)
  data_cols <- lapply(
    members,
    function(x) {
      grep(
        paste0("_mbr", formatC(x, width = 3, flag = "0"), suffix),
        colnames(df)
      )
    }
  )
  data_cols <- unlist(data_cols[sapply(data_cols, length) != 0])
  dplyr::select_at(df, c(meta_cols, data_cols))
}
