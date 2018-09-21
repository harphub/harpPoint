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
merge_obs <- function(
  .fcst,
  .obs,
  parameter = NULL,
  join_type = c("inner", "left", "right", "full", "semi", "anti"),
  by        = c("SID", "validdate")
) {

  if (!is.element("dataframe_format", names(attributes(.fcst)))) {
    warning("Input forecast data frame does not have a dataframe_format attribute")
    has_df_format <- FALSE
  } else {
    has_df_format <- TRUE
    dataframe_format <- attr(.fcst, "dataframe_format")
  }

  valid_joins <- c("inner", "left", "right", "full", "semi", "anti")
  if (length(intersect(join_type, valid_joins)) < 1) {
    stop(
      paste(
        "Invalid join_type: ", join_type[1], ".\n",
        "Must be one of: 'inner', 'left', 'right', 'full', 'semi', 'anti'"
      )
    )
  }

  if (is.null(parameter)) {
    parameter <- colnames(.obs)[3]
  }

  obs_col <- rlang::sym(parameter)

  .fcst <- switch(join_type[1],
    inner = dplyr::inner_join(.fcst, .obs, by = by),
    left  = dplyr::left_join(.fcst, .obs, by = by),
    right = dplyr::right_join(.fcst, .obs, by = by),
    full  = dplyr::full_join(.fcst, .obs, by = by),
    semi  = dplyr::semi_join(.fcst, .obs, by = by),
    anti  = dplyr::anti_join(.fcst, .obs, by = by)
  )

  if (has_df_format) {
    attr(.fcst, "dataframe_format") <- dataframe_format
  }

  dplyr::rename(.fcst, obs = !!obs_col)
}
