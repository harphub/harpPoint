#' Set or modify and output the perfect scores for different verificaiton
#' metrics
#'
#' The function returns a named vector giving the perfect score for all
#' verification scores computed by harpPoint that give a single value. These
#' perfect values can be modified by specifying them in named arguments and
#' perfect scores for new scores can be set in the same way.
#'
#' @param ... Named arguments that give values for the perfect score
#'
#' @return A named vector
#' @export
#'
#' @examples
#' perfect_score()
#' perfect_score(bss = 1)
#' perfect_score(bias = -1)
perfect_score <- function(...) {

  orientation <- c(
    bias                     = 0,
    rmse                     = 0,
    mae                      = 0,
    stde                     = 0,
    threat_score             = 1,
    hit_rate                 = 1,
    miss_rate                = 0,
    false_alarm_rate         = 0,
    false_alarm_ratio        = 0,
    heidke_skill_score       = 1,
    pierce_skill_score       = 1,
    kuiper_skill_score       = 1,
    percent_correct          = 1,
    frequency_bias           = 1,
    equitable_threat_score   = 1,
    odds_ratio               = 1e6,
    log_odds_ratio           = 1e6,
    odds_ratio_skill_score   = 1,
    extreme_dependency_score = 1,
    symmetric_eds            = 1,
    extreme_dependency_index = 1,
    symmetric_edi            = 1,
    mean_bias                = 0,
    spread                   = 1e6,
    spread_skill_ratio       = 1,
    crps                     = 0,
    crps_potential           = 0,
    crps_reliability         = 0,
    fair_brier_score         = 0,
    fair_crps                = 0,
    brier_score              = 0,
    brier_skill_score        = 1,
    brier_score_reliability  = 0,
    brier_score_resolution   = 1,
    roc_area                 = 1
  )

  dots <- list(...)

  if (any(sapply(dots, length) > 1)) {
    stop("Arguments must be of length 1.")
  }

  dots <- unlist(dots)

  for (i in seq_along(dots)) {
    orientation[names(dots)[i]] <- dots[i]
  }

  orientation

}
