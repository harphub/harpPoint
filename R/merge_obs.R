#' Combine observations with point forecasts.
#'
#' @param FCST A point forecast data frame.
#' @param OBS A point observations data frame.
#' @param join_type How to join the data frame. Acceptable values are: "inner",
#'   "left", "right", "full", "semi", "anti". See \code{\link[dplyr]{join}} for
#'   more details.
#' @param by Which columns to join by. Default is c("SID", "validdate").
#'
#' @return The input forecast data fram with an "obs" column added.
#' @export
#'
#' @examples
merge_obs <- function(
  FCST,
  OBS,
  join_type = c("inner", "left", "right", "full", "semi", "anti"),
  by        = c("SID", "validdate")
) {

  if (!is.element("dataframe_format", names(attributes(FCST)))) {
    warning("Input forecast data frame does not have a dataframe_format attribute")
    has_df_format <- FALSE
  } else {
    has_df_format <- TRUE
    dataframe_format <- attr(FCST, "dataframe_format")
  }

  valid_joins <- c("inner", "left", "right", "full", "semi", "anti")
  if (length(intersect(joint_type, valid_joins)) < 1) {
    stop(
      paste(
        "Invalid join_type: ", join_type[1], ".\n",
        "Must be one of: 'inner', 'left', 'right', 'full', 'semi', 'anti'"
      )
    )
  }

  join_function <- get(paste0("dplyr::", join_type[1], "_join"))

  FCST <- join_function(FCST, OBS, by = by)

  if (has_df_format) {
    attr(FCST, "dataframe_format") <- dataframe_format
  }

  FCST
}
