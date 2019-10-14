#' Combine data with point forecasts.
#'
#' Works in a similar way to the \link[dplyr]{join} family of functions, excpet
#' that the join type is specified as an argument.
#'
#' This function would typically be used to add a column of observations to the
#' forecast data. If units columns are found in the data, the function will
#' check if the units are the same for both .fcst and .join. If they are not, an
#' error will be thrown and the join will not be done. This behaviour can be
#' overridden with the force_join argument.
#'
#'
#' @param .fcst A \code{harp_fcst} object.
#' @param .join A data frame to join to each table in the \code{harp_fcst}
#'   object.
#' @param join_type How to join the data frame. Acceptable values are: "inner",
#'   "left", "right", "full", "semi", "anti". See \code{\link[dplyr]{join}} for
#'   more details.
#' @param by Which columns to join by - if set to NULL a natural join will be
#'   done, using all variables with common names across .fcst and .join.
#' @param force_join Set to TRUE to force the join to happen even if the units
#'   in .fcst and .join are not compatible.
#' @param ... Other arguments for \link[dplyr]{join}.
#'
#' @return The input forecast data frame with column(s) added from \code{.join}.
#' @export
#'
#' @examples
#'
join_to_fcst <- function(
  .fcst,
  .join,
  join_type  = c("inner", "left", "right", "full", "semi", "anti"),
  by         = NULL,
  force_join = FALSE,
  ...
) {

  UseMethod("join_to_fcst")

}

#' @export
join_to_fcst.default <- function(
  .fcst,
  .join,
  join_type  = c("inner", "left", "right", "full", "semi", "anti"),
  by         = NULL,
  force_join = FALSE,
  ...
) {

  join_type <- match.arg(join_type)

  join_func <- get(paste0(join_type, "_join"), envir = asNamespace("dplyr"))

  # Check for units columns
  has_fcst_units <- is.element("units", colnames(.fcst))
  has_join_units <- is.element("units", colnames(.join))

  if (has_fcst_units & has_join_units) {

    do_join <- TRUE
    fcst_units <- unique(.fcst$units)
    join_units <- unique(.join$units)

    if (length(fcst_units) != 1) {
      warning(".fcst has more than one units name: ", fcst_units, call. = FALSE, immediate. = TRUE)
      do_join <- FALSE
    } else if (length(unique(.fcst$units)) != 1) {
      warning(".join has more than one units name: ", join_units, call. = FALSE, immediate. = TRUE)
      do_join <- FALSE
    } else {
      if (fcst_units != join_units) {
        warning(".fcst has units: ", fcst_units, " and .join has units: ", join_units, call. = FALSE, immediate. = TRUE)
        do_join <- FALSE
      }
    }

  } else if (has_fcst_units & !has_join_units) {

    warning(".join does not have a units column. ", call. = FALSE, immediate. = TRUE)
    do_join <- FALSE

  } else if (!has_fcst_units & has_join_units) {

    warning(".fcst does not have a units column. ", call. = FALSE, immediate. = TRUE)
    do_join <- FALSE

  } else {

    warning("Neither .fcst nor .join have a units column.", call. = FALSE, immediate. = TRUE)
    do_join <- TRUE

  }

  if (!do_join) {
    if (force_join) {
      message("Forcing join without units taken into account.")
      if (is.null(by)) {
        by <- intersect(colnames(.fcst), colnames(.join))
        by <- by[by != "units"]
      }
    } else {
      stop(
        "Join will not be done due to units incompatibility. You can force the join by setting force_join = TRUE\n",
        "OR, units imcompatibility can be fixed with the set_units, scale_point_forecast, or scale_point_obs functions.",
        call. = FALSE
      )
    }
  }

  if (!is.null(by)) {
    message("Joining, by = c(\"", paste(by, collapse = "\", \""), "\")")
  }

  join_func(.fcst, .join, by = by, ...)

}

#' @export
join_to_fcst.harp_fcst <- function(
  .fcst,
  .join,
  join_type  = c("inner", "left", "right", "full", "semi", "anti"),
  by         = NULL,
  force_join = FALSE,
  ...
) {

  try(new_harp_fcst(purrr::map(.fcst, join_to_fcst, .join, join_type, by, force_join, ...)))

}
