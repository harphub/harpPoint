#' Combine observations with point forecasts.
#'
#' @param .fcst A point forecast data frame.
#' @param .obs A point observations data frame.
#' @param parameter Which observed parameter to use in the verification. If set
#'   to NULL it is the first observation column in the OBS data frame.
#' @param join_type How to join the data frame. Acceptable values are: "inner",
#'   "left", "right", "full", "semi", "anti". See \code{\link[dplyr]{join}} for
#'   more details.
#' @param by Which columns to join by. Default is c("SID", "validdate").
#'
#' @return The input forecast data fram with an "obs" column added.
#' @export
#'
#' @examples
#'
merge_obs <- function(
  .fcst,
  .obs,
  parameter = NULL,
  join_type = c("inner", "left", "right", "full", "semi", "anti"),
  by        = c("SID", "validdate")
) {

    UseMethod("merge_obs")

}

#' @export
merge_obs.default <- function(
  .fcst,
  .obs,
  parameter = NULL,
  join_type = c("inner", "left", "right", "full", "semi", "anti"),
  by        = c("SID", "validdate")
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

  switch(join_type[1],
    inner = dplyr::inner_join(.fcst, .obs, by = by),
    left  = dplyr::left_join(.fcst, .obs, by = by),
    right = dplyr::right_join(.fcst, .obs, by = by),
    full  = dplyr::full_join(.fcst, .obs, by = by),
    semi  = dplyr::semi_join(.fcst, .obs, by = by),
    anti  = dplyr::anti_join(.fcst, .obs, by = by)
  )

}

#' @export
merge_obs.harp_fcst <- function(
  .fcst,
  .obs,
  parameter = NULL,
  join_type = c("inner", "left", "right", "full", "semi", "anti"),
  by        = c("SID", "validdate")
) {

  out <- purrr::map(.fcst, merge_obs, .obs, parameter, join_type, by)
  class(out) <- class(.fcst)
  out

}
