# Functions to compute individual ensemble verification scores. These are more
# convenient than setting all of the unwanted scores to FALSE in ens_verify()

#' Compute the skill (RMSE) and spread of an ensemble forecast
#'
#' The ensemble mean and spread are computed as columns in a \code{harp_verif}
#' object. By default the scores are aggregated over lead time by other grouping
#' variables cam be chosen. The mean bias and standard deviation of the error
#' are also computed.
#'
#' This is simply a wrapper around \code{\link{ens_verify()}} with `summary` as
#' the only selected score.
#'
#' @inheritParams ens_verify
#'
#' @return A `harp_verif` object with an entry for "ens_summary_scores"
#' @export
ens_spread_and_skill <- function(
  .fcst,
  parameter,
  groupings          = "lead_time",
  circle             = NULL,
  spread_drop_member = NULL,
  jitter_fcst        = NULL,
  show_progress      = TRUE,
  ...
) {

  parameter <- rlang::enquo(parameter)
  ens_verify(
    .fcst, !!parameter, groupings = groupings, circle = circle,
    spread_drop_member = spread_drop_member, jitter_fcst = jitter_fcst,
    rank_hist = FALSE, crps = FALSE, crps_decomp = FALSE, hexbin = FALSE,
    verify_members = FALSE, show_progress = show_progress, thresholds = NULL,
    ...
  )
}

#' Compute the CRPS an ensemble forecast
#'
#' The CRPS is computed using the kernel method as a column in a `harp_verif`
#' object. By default the score is aggregated by lead time, though any other
#' grouping strategy can be chosen. Optionally the "fair" CRPS can be computed
#' for a any number of ensemble members. Note that this method of computing the
#' CRPS is typically faster and less memory intensive than the CDF method used
#' by \code{\link{ens_crps_decomp()}}.
#'
#' This is simply a wrapper around \code{\link{ens_verify()}} with `crps` as
#' the only selected score.
#'
#' @inheritParams ens_verify
#'
#' @return A `harp_verif` object with an entry for "ens_summary_scores"
#' @export
ens_crps <- function(
  .fcst,
  parameter,
  groupings       = "lead_time",
  num_ref_members = NA,
  jitter_fcst     = NULL,
  show_progress   = TRUE,
  ...
) {

  parameter <- rlang::enquo(parameter)
  ens_verify(
    .fcst, !!parameter, groupings = groupings, jitter_fcst = jitter_fcst,
    rank_hist = FALSE, crps = TRUE, crps_decomp = FALSE, hexbin = FALSE,
    verify_members = FALSE, show_progress = show_progress, thresholds = NULL,
    num_ref_members = num_ref_members, summary = FALSE, ...
  )
}

#' Compute the CRPS and its decomposition of an ensemble forecast
#'
#' The CRPS is computed using the CDF method along with its decomposition into
#' reliability and potential components as columns in a `harp_verif`
#' object. By default the score is aggregated by lead time, though any other
#' grouping strategy can be chosen. Note that this method of computing the
#' CRPS is typically slower and more memory intensive than the kernel method
#' used by \code{\link{ens_crps()}}.
#'
#' This is simply a wrapper around \code{\link{ens_verify()}} with `crps_decomp`
#' as the only selected score.
#'
#' @inheritParams ens_verify
#'
#' @return A `harp_verif` object with an entry for "ens_summary_scores"
#' @export
ens_crps_decomp <- function(
  .fcst,
  parameter,
  groupings       = "lead_time",
  jitter_fcst     = NULL,
  show_progress   = TRUE,
  ...
) {

  parameter <- rlang::enquo(parameter)
  ens_verify(
    .fcst, !!parameter, groupings = groupings, jitter_fcst = jitter_fcst,
    rank_hist = FALSE, crps = FALSE, crps_decomp = TRUE, hexbin = FALSE,
    verify_members = FALSE, show_progress = show_progress, thresholds = NULL,
    summary = FALSE, ...
  )
}

#' Compute the rank histogram an ensemble forecast
#'
#' The rank histogram as a nested column in a `harp_verif` object. By default
#' the score is aggregated by lead time, though any other grouping strategy can
#' be chosen. Observation errors can be taken into account by supplying a
#' jitter function. See \code{\link{jitter_fcst}} for more details.
#'
#' This is simply a wrapper around \code{\link{ens_verify()}} with `rank_hist`
#' as the only selected score.
#'
#' @inheritParams ens_verify
#'
#' @return A `harp_verif` object with an entry for "ens_summary_scores"
#' @export
ens_rank_histogram <- function(
  .fcst,
  parameter,
  groupings       = "lead_time",
  jitter_fcst     = NULL,
  show_progress   = TRUE,
  ...
) {

  parameter <- rlang::enquo(parameter)
  ens_verify(
    .fcst, !!parameter, groupings = groupings, jitter_fcst = jitter_fcst,
    rank_hist = TRUE, crps = FALSE, crps_decomp = FALSE, hexbin = FALSE,
    verify_members = FALSE, show_progress = show_progress, thresholds = NULL,
    summary = FALSE, ...
  )
}

#' Compute hexbins for an ensemble forecast
#'
#' A hexbin compares the value of observations and forecasts by binning them
#' into hexagonal areas on a forecast vs observations hexagonal grid. By default
#' the score is aggregated by lead time, though any other grouping strategy can
#' be chosen. Hexbins for each ensemble are computed as well, but can be skipped
#' by setting `verify_members = FALSE`.
#'
#' This is simply a wrapper around \code{\link{ens_verify()}} with `hexbin`
#' as the only selected score.
#'
#' @inheritParams ens_verify
#'
#' @return A `harp_verif` object with an entry for "ens_summary_scores", and
#'   "det_summary_scores" if `verify_members = TRUE`.
#' @export
ens_hexbin <- function(
  .fcst,
  parameter,
  groupings       = "lead_time",
  verify_members  = TRUE,
  show_progress   = TRUE,
  ...
) {

  parameter <- rlang::enquo(parameter)
  ens_verify(
    .fcst, !!parameter, groupings = groupings,
    rank_hist = FALSE, crps = FALSE, crps_decomp = FALSE, hexbin = TRUE,
    verify_members = verify_members, show_progress = show_progress,
    thresholds = NULL, summary = FALSE, ...
  )
}

# For dropping members - used in ens_verify()
parse_member_drop <- function(x, nm) {

  if (!is.null(names(x))) {
    x <- as.list(x)
  }

  if (!is.list(x)) {
    if (is.null(x)) {
      return(sapply(nm, function(x) NULL, simplify = FALSE))
    }
    if (length(x) == 1) {
      return(sapply(nm, function(.x) x, simplify = FALSE))
    }
    if (length(x) == length(nm)) {
      x <- as.list(x)
      names(x) <- nm
      return(x)
    }
    stop("Bad input for `spread_exclude_member`", call. = FALSE)
  }

  if (is.null(names(x))) {

    if (length(x) == length(nm)) {
      names(x) <- nm
      return(x)
    }

    stop(
      "If `spread_exclude_member` is a list ",
      "it must be the same length as `.fcst` or have names",
      call. = FALSE
    )

  }

  if (identical(sort(names(x)), sort(nm))) {
    return(x[nm])
  }

  if (length(intersect(names(x), nm)) < 1) {
    stop(
      "spread_exclude_member: ",
      paste(names(x), collapse = ", "),
      " not found in `.fcst`.",
      call. = FALSE
    )
  }

  if (length(setdiff(names(x), nm)) > 0) {
    stop(
      "spread_exclude_member: ",
      paste(setdiff(names(x), nm), collapse = ", "),
      " not found in `.fcst`.",
      call. = FALSE
    )
  }

  x <- c(x, sapply(setdiff(nm, names(x)), function(x) NULL, simplify = FALSE))

  x[nm]

}

### Threshold scores

#' Brier Score for an ensemble forecast
#'
#' Computes the Brier Score, its decomposition, and the Brier Skill Score for an
#' ensemble forecast. If `num_ref_members` is supplied, the fair Brier Score is
#' also computed for that number of members. For the Brier Skill Score, the
#' default is to use the sample climatology as the reference. It is also
#' possible to pass the climatology as a data frame that can be inner joined
#' with the forecast data.
#'
#' This is simply a wrapper around \code{\link{ens_verify()}} with `brier`
#' as the only selected score.
#'
#' @inheritParams ens_verify
#'
#' @return A `harp_verif` object with an entry for "ens_threshold_scores".
#' @export
ens_brier <- function(
  .fcst,
  parameter,
  thresholds,
  comparator      = c("ge", "gt", "le", "lt", "eq", "between", "outside"),
  include_low     = TRUE,
  include_high    = TRUE,
  groupings       = "lead_time",
  climatology     = "sample",
  num_ref_members = NA,
  show_progress   = TRUE,
  ...
) {
  parameter <- rlang::enquo(parameter)
  ens_verify(
    .fcst, !!parameter, groupings = groupings, verify_members = FALSE,
    rank_hist = FALSE, crps = FALSE, crps_decomp = FALSE, hexbin = FALSE,
    summary = FALSE, thresholds = thresholds, comparator = comparator,
    include_low = include_low, include_high = include_high,
    brier = TRUE, reliability = FALSE, roc = FALSE, econ_val = FALSE,
    tw_crps = FALSE, climatology = climatology, rel_probs = NA,
    num_ref_members = num_ref_members, show_progress = show_progress, ...
  )["ens_threshold_scores"]
}

#' Reliability for an ensemble forecast
#'
#' Computes the reliability for an ensemble forecast. That is, the observed
#' frequency for a given forecast probability. The binning into to probability
#' categories can be set using `rel_probs`.
#'
#' This is simply a wrapper around \code{\link{ens_verify()}} with `reliability`
#' as the only selected score.
#'
#' @inheritParams ens_verify
#'
#' @return A `harp_verif` object with an entry for "ens_threshold_scores".
#' @export
ens_reliability <- function(
  .fcst,
  parameter,
  thresholds,
  comparator      = c("ge", "gt", "le", "lt", "eq", "between", "outside"),
  include_low     = TRUE,
  include_high    = TRUE,
  groupings       = "lead_time",
  climatology     = "sample",
  rel_probs       = NA,
  show_progress   = TRUE,
  ...
) {
  parameter <- rlang::enquo(parameter)
  ens_verify(
    .fcst, !!parameter, groupings = groupings, verify_members = FALSE,
    rank_hist = FALSE, crps = FALSE, crps_decomp = FALSE, hexbin = FALSE,
    summary = FALSE, thresholds = thresholds, comparator = comparator,
    include_low = include_low, include_high = include_high,
    brier = FALSE, reliability = TRUE, roc = FALSE, econ_val = FALSE,
    tw_crps = FALSE, climatology = climatology, rel_probs = NA,
    num_ref_members = NA, show_progress = show_progress, ...
  )["ens_threshold_scores"]
}

#' ROC for an ensemble forecast
#'
#' Computes the Relative Operating Characteristic (ROC) for an ensemble
#' forecast. That is, the hit rate and the false alarm rate for
#' a range probabilities. In addition, the are under the ROC curve is computed
#' to provide a summary over all probabilities.
#'
#' This is simply a wrapper around \code{\link{ens_verify()}} with `roc`
#' as the only selected score.
#'
#' @inheritParams ens_verify
#'
#' @return A `harp_verif` object with an entry for "ens_threshold_scores".
#' @export
ens_roc <- function(
  .fcst,
  parameter,
  thresholds,
  comparator      = c("ge", "gt", "le", "lt", "eq", "between", "outside"),
  include_low     = TRUE,
  include_high    = TRUE,
  groupings       = "lead_time",
  show_progress   = TRUE,
  ...
) {
  parameter <- rlang::enquo(parameter)
  ens_verify(
    .fcst, !!parameter, groupings = groupings, verify_members = FALSE,
    rank_hist = FALSE, crps = FALSE, crps_decomp = FALSE, hexbin = FALSE,
    summary = FALSE, thresholds = thresholds, comparator = comparator,
    include_low = include_low, include_high = include_high,
    brier = FALSE, reliability = FALSE, roc = TRUE, econ_val = FALSE,
    tw_crps = FALSE, rel_probs = NA,
    num_ref_members = NA, show_progress = show_progress, ...
  )["ens_threshold_scores"]
}

#' Economic value for an ensemble forecast
#'
#' Computes the outer envelope of the economic value for a range of cost-loss
#' ratios. The output can be interpreted as the increase in economic value that
#' the ensemble provides over climatology for all cost-loss ratios.
#'
#' This is simply a wrapper around \code{\link{ens_verify()}} with `econ_val`
#' as the only selected score.
#'
#' @inheritParams ens_verify
#'
#' @return A `harp_verif` object with an entry for "ens_threshold_scores".
#' @export
ens_value <- function(
  .fcst,
  parameter,
  thresholds,
  comparator      = c("ge", "gt", "le", "lt", "eq", "between", "outside"),
  include_low     = TRUE,
  include_high    = TRUE,
  groupings       = "lead_time",
  show_progress   = TRUE,
  ...
) {
  parameter <- rlang::enquo(parameter)
  ens_verify(
    .fcst, !!parameter, groupings = groupings, verify_members = FALSE,
    rank_hist = FALSE, crps = FALSE, crps_decomp = FALSE, hexbin = FALSE,
    summary = FALSE, thresholds = thresholds, comparator = comparator,
    include_low = include_low, include_high = include_high,
    brier = FALSE, reliability = FALSE, roc = FALSE, econ_val = TRUE,
    tw_crps = FALSE, rel_probs = NA,
    num_ref_members = NA, show_progress = show_progress, ...
  )["ens_threshold_scores"]
}

#' Threshold weighted CRPS for an ensemble forecast
#'
#' Computes the threshold weighted CRPS for an ensemble forecast. First values
#' outside of the threshold are clamped to the threhshold value and then the
#' CRPS is computed using the kernel method.
#'
#' This is simply a wrapper around \code{\link{ens_verify()}} with `tw_crps`
#' as the only selected score.
#'
#' @inheritParams ens_verify
#'
#' @return A `harp_verif` object with an entry for "ens_threshold_scores".
#' @export
ens_tw_crps <- function(
  .fcst,
  parameter,
  thresholds,
  comparator      = c("ge", "gt", "le", "lt", "between"),
  include_low     = TRUE,
  include_high    = TRUE,
  groupings       = "lead_time",
  show_progress   = TRUE,
  ...
) {
  parameter <- rlang::enquo(parameter)
  comparator <- match.arg(comparator)
  ens_verify(
    .fcst, !!parameter, groupings = groupings, verify_members = FALSE,
    rank_hist = FALSE, crps = FALSE, crps_decomp = FALSE, hexbin = FALSE,
    summary = FALSE, thresholds = thresholds, comparator = comparator,
    include_low = include_low, include_high = include_high,
    brier = FALSE, reliability = FALSE, roc = FALSE, econ_val = FALSE,
    tw_crps = TRUE, show_progress = show_progress, ...
  )["ens_threshold_scores"]
}


