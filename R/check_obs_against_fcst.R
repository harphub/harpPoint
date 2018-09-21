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

  if (is.null(num_sd_allowed)) {
    num_sd_allowed <- switch(
      parameter,
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

    bad_obs <- dplyr::inner_join(
      .fcst,
      .fcst %>%
        dplyr::group_by(.data$SID, .data$leadtime) %>%
        dplyr::summarise(
          tolerance = sd(.data$forecast) * num_sd_allowed
        ),
      by = c("SID", "leadtime")
    ) %>%
      dplyr::group_by(.data$SID, .data$fcdate, .data$validdate) %>%
      dplyr::summarise(
        closest   = min(abs(.data$forecast - .data$obs)),
        tolerance = unique(.data$tolerance)
      ) %>%
      dplyr::filter(.data$closest > .data$tolerance)

    good_obs <- dplyr::anti_join(.fcst, bad_obs, by = c("SID", "validdate"))

    bad_obs  <- .fcst %>%
      dplyr::inner_join(bad_obs, by = c("SID", "validdate", "fcdate")) %>%
      dplyr::select(-.data$closest)

    attr(good_obs, "removed_cases") <- bad_obs

  } else {

    good_obs <- .fcst
    attr(good_obs, "removed_cases") <- NULL

  }

  good_obs

}
