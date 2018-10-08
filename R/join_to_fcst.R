#' Combine data with point forecasts.
#'
#' Works in a similar way to the \link[dplyr]{join} family of functions, excpet
#' that the join type is specified as an argument.
#'
#' @param .fcst A \code{harp_fcst} object.
#' @param .join A data frame to join to each table in the \code{harp_fcst}
#'   object.
#' @param join_type How to join the data frame. Acceptable values are: "inner",
#'   "left", "right", "full", "semi", "anti". See \code{\link[dplyr]{join}} for
#'   more details.
#' @param by Which columns to join by.
#'
#' @return The input forecast data fram with an "obs" column added.
#' @export
#'
#' @examples
#'
join_to_fcst <- function(
  .fcst,
  .join,
  join_type = c("inner", "left", "right", "full", "semi", "anti"),
  ...
) {

  UseMethod("join_to_fcst")

}

#' @export
join_to_fcst.default <- function(
  .fcst,
  .join,
  join_type = c("inner", "left", "right", "full", "semi", "anti"),
  ...
) {

  valid_joins <- c("inner", "left", "right", "full", "semi", "anti")
  if (length(intersect(join_type, valid_joins)) < 1) {
    stop(
      paste(
        "Invalid join_type: ", join_type[1], ".\n",
        "Must be one of: 'inner', 'left', 'right', 'full', 'semi', 'anti'"
      )
    )
  }

  join_func <- get(paste0(join_type[1], "_join"), envir = asNamespace("dplyr"))

  join_func(.fcst, .join, ...)

}

#' @export
join_to_fcst.harp_fcst <- function(
  .fcst,
  .join,
  join_type = c("inner", "left", "right", "full", "semi", "anti"),
  ...
) {

  new_harp_fcst(purrr::map(.fcst, join_to_fcst, .join, join_type, ...))

}
