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
#' @param rel_probs Probabilities to use for reliability diagrams. Set to NA
#'   (the default) to select automatically.
#' @param show_progress Logical - whether to show a progress bar. The default is
#' `TRUE`
#' @param fcst_model The name of the forecast model to use in the `fcst_model`
#'   column of the output. If the function is dispatched on a `harp_list`
#'   object, the names of the `harp_list` are automatically used.
#' @param ... Passed to \link{ens_brier}.
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
  groupings     = "leadtime",
  climatology   = "sample",
  rel_probs     = NA,
  show_progress = TRUE,
  fcst_model    = NULL,
  ...
) {

  # Set progress bar to false for batch running
  if (!interactive()) show_progress <- FALSE

    if (missing(parameter)) {
    cli::cli_abort(
      "Argument {.arg parameter} is missing with no default."
    )
  }
  ens_brier(
    .fcst,
    {{parameter}},
    thresholds,
    groupings     = groupings,
    climatology   = climatology,
    rel_probs     = rel_probs,
    keep_score    = "reliability",
    show_progress = show_progress,
    fcst_model    = fcst_model
  )
}

