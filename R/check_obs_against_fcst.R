#' Observation error check against forecast
#'
#' For each station (SID) and lead time the standard deviation of the forecast
#' over all forecast cycles and models (mname), and in the case of an eps
#' forecast, the ensemble members is computed. The difference between the
#' observation and the forecast is expected to be smaller than a number of
#' multiples of the standard deviation. The number of multiples of the standard
#' deviation can be supplied or a default value used depending on the parameter.
#' @param .fcst A data frame of class \code{harp_point_forecast_obs}.
#' @param parameter The forecast parameter.
#' @param num_sd_allowed The number of standard deviations of the forecast that
#'   the difference between the forecast and the observation must be smaller
#'   than.
#'
#' @return A data frame of class \code{harp_point_forecast_obs} with an
#'   attribute named \code{removed_cases} containing a data frame of the removed
#'   cases.
#' @export
#'
#' @examples

check_obs_against_fcst <- function(.fcst, parameter, num_sd_allowed = NULL) {

  parameter_quo  <- rlang::enquo(parameter)
  parameter_expr <- rlang::quo_get_expr(parameter_quo)
  if (is.character(parameter_expr)) {
    parameter_quo <- rlang::sym(parameter_expr)
  }
  parameter_name <- rlang::quo_name(parameter_quo)

    if (is.null(num_sd_allowed)) {
    num_sd_allowed <- switch(
      parameter_name,
      "T2m"       = 6,
      "RH2m"      = 6,
      "Pmsl"      = 6,
      "S10m"      = 6,
      "G10m"      = 6,
      "AccPcp1h"  = 8,
      "AccPcp3h"  = 8,
      "AccPcp6h"  = 8,
      "AccPcp12h" = 8,
      "AccPcp24h" = 8,
      0
    )
  }

  if (num_sd_allowed > 0) {

    tolerance <- .fcst %>%
      join_models(
        by = c(
          "SID",
          "leadtime",
          "fcst_cycle",
          "fcdate",
          "validdate",
          parameter_name
        )
      )

    tolerance_df <- ens_spread_and_skill(
      tolerance,
      !! parameter_quo,
      groupings = c("SID", "leadtime")
    )$ens_summary_scores %>%
      dplyr::ungroup() %>%
      dplyr::transmute(
        .data$SID,
        .data$leadtime,
        tolerance_allowed = .data$spread * num_sd_allowed
      )

    tolerance <- join_to_fcst(
      tolerance,
      tolerance_df,
      by = c("SID", "leadtime")
    )

    tolerance <- tolerance[[1]] %>%
      dplyr::mutate_at(
        dplyr::vars(contains("_mbr")),
        dplyr::funs(abs(. - !! parameter_quo))
      )

    tolerance <- tolerance %>%
      dplyr::mutate(
        min_diff = matrixStats::rowMins(
          as.matrix(
            dplyr::select(tolerance, dplyr::contains("_mbr"))
          )
        )
      )

    good_obs <- tolerance %>%
      dplyr::filter(.data$min_diff <= .data$tolerance_allowed) %>%
      dplyr::select(.data$SID, .data$validdate, !! parameter_quo) %>%
      dplyr::group_by(.data$SID, .data$validdate) %>%
      dplyr::summarise(!! rlang::sym(parameter_name) := unique(!! parameter_quo)) %>%
      dplyr::ungroup()

    bad_obs  <- tolerance %>%
      dplyr::filter(.data$min_diff > .data$tolerance_allowed) %>%
      dplyr::select(.data$SID, .data$validdate, !! parameter_quo) %>%
      dplyr::group_by(.data$SID, .data$validdate) %>%
      dplyr::summarise(!! rlang::sym(parameter_name) := unique(!! parameter_quo)) %>%
      dplyr::ungroup()

    .fcst <- .fcst %>%
      join_to_fcst(
        dplyr::select(
          good_obs, .data$SID, .data$validdate
        ),
        by = c("SID", "validdate")
      )

    attr(.fcst, "removed_cases") <- bad_obs

  } else {

    attr(.fcst, "removed_cases") <- NULL

  }

  .fcst

}
