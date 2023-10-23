#' Reliability for an ensemble.
#'
#' @inheritParams ens_verify
#' @export
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

