#' Reliability for an ensemble.
#'
#' @param .fcst A \code{harp_fcst} object with tables that have a column for
#'   observations, or a single forecast table.
#' @param parameter The name of the column for the observed data.
#' @param thresholds A numeric vector of thresholds for which to compute the
#'   reliability.
#' @param groupings The groups for which to compute the ensemble mean and
#'   spread. See \link[dplyr]{group_by} for more information of how grouping
#'   works.
#' @param climatology The climatology to use for the Brier Skill Score. Can be
#'   "sample" for the sample climatology (the default), a named list with
#'   elements eps_model and member to use a member of an eps model in the
#'   harp_fcst object for the climatology, or a data frame with columns for
#'   threshold and climatology and also optionally leadtime.
#'
#' @return A data frame with data grouped for the \code{groupings} column(s) and
#'   a nested column for reliability. The column can be unnested with
#'   \link[tidyr]{unnest}.
#' @export
#'
#' @examples
ens_reliability <- function(
  .fcst,
  parameter,
  thresholds,
  groupings = "leadtime",
  climatology = "sample",
  show_progress = FALSE
) {
  parameter <- rlang::enquo(parameter)
  ens_brier(
    .fcst,
    !! parameter,
    thresholds,
    groupings = groupings,
    climatology = climatology,
    keep_score = "reliability",
    show_progress = show_progress
  )
}

