#' Observation error check against forecast
#'
#' For each stratification the standard deviation of the forecast over all
#' forecast cycles and models (mname), and in the case of an eps forecast, the
#' ensemble members is computed. The difference between the observation and the
#' forecast is expected to be smaller than a number of multiples of the standard
#' deviation. The number of multiples of the standard deviation can be supplied
#' or a default value used depending on the parameter.
#'
#' @param .fcst A data frame of class \code{harp_point_forecast_obs}.
#' @param parameter The forecast parameter.
#' @param num_sd_allowed The number of standard deviations of the forecast that
#'   the difference between the forecast and the observation must be smaller
#'   than.
#' @param stratification The columns to stratify the data by when computing the
#'   allowed tolerance. In most cases the column must exist in the input data,
#'   but "quarter_day" can be passed to divide the observations into classes of
#'   [0, 6), [6, 12), [12, 18) and [18, 24) hour of day. The default behaviour
#'   is to stratify by station ("SID") and "quarter_day".
#'
#' @return A data frame of class \code{harp_point_forecast_obs} with an
#'   attribute named \code{removed_cases} containing a data frame of the removed
#'   cases.
#' @export
#'
#' @examples

check_obs_against_fcst <- function(
    .fcst, parameter, num_sd_allowed = NULL, stratification = c("SID", "quarter_day")
) {

  parameter_quo  <- rlang::enquo(parameter)
  parameter_expr <- rlang::quo_get_expr(parameter_quo)
  if (is.character(parameter_expr)) {
    parameter_quo <- rlang::sym(parameter_expr)
  }
  parameter_name <- rlang::quo_name(parameter_quo)

  if (is.null(num_sd_allowed)) {
    .num_sd_allowed <- switch(
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
  } else {
    .num_sd_allowed <- num_sd_allowed
  }

  fcst_regex <- "_mbr[[:digit:]]{3}|_det$|^fcst$|^forecast$"
  if (.num_sd_allowed > 0) {

    tolerance <- join_models(
      .fcst,
      by = Reduce(
        intersect,
        lapply(
          .fcst,
          function(x) {
            grep(
              paste0(fcst_regex, "|fcst_model"),
              colnames(x),
              value = TRUE, invert = TRUE
            )
          }
        )
      )
    )

    # Create extra columns for stratifying
    if (!is.element("valid_hour", colnames(tolerance))) {

      if (!is.element("valid_dttm", colnames(tolerance))) {
        stop(".fcst must have `valid_dttm` column.")
      }

      if (is.numeric(tolerance[["valid_dttm"]])) {
        tolerance <- harpCore::expand_date(tolerance, .data[["valid_dttm"]])
      } else if (inherits(tolerance[["valid_dttm"]], "POSIXct")) {
        tolerance <- dplyr::mutate(
          tolerance,
          valid_hour = lubridate::hour(.data[["valid_dttm"]]),
          valid_month = lubridate::month(.data[["valid_dttm"]])
        )
      } else {
        stop("Cannot convert valid_dttm column to hour and months")
      }

    }

    if (is.element("quarter_day", stratification)) {
      tolerance <- dplyr::mutate(
        tolerance,
        quarter_day = cut(.data[["valid_hour"]], seq(0, 24, 6), right = FALSE)
      )
    }

    tolerance_df <- tidyr::pivot_longer(
      tolerance,
      tidyselect::matches(fcst_regex)
    ) %>%
      dplyr::group_by(!!!rlang::syms(stratification)) %>%
      dplyr::summarise(
        tolerance_allowed = stats::sd(.data[["value"]]) * .num_sd_allowed,
        num_cases         = dplyr::n()
      )

    tolerance <- suppressWarnings(suppressMessages(harpCore::join_to_fcst(
      tolerance,
      tolerance_df,
      by = stratification,
      force = TRUE
    )))

    tolerance <- dplyr::mutate(
      tolerance,
      dplyr::across(
        dplyr::matches(fcst_regex),
        ~abs(. - !!parameter_quo)
      )
    )

    tolerance <- dplyr::mutate(
      tolerance,
      min_diff = matrixStats::rowMins(
        as.matrix(
          dplyr::select(tolerance, dplyr::matches(fcst_regex))
        )
      )
    )

    # tolerance <- tolerance %>%
    #   dplyr::mutate(
    #     min_diff = matrixStats::rowMins(
    #       as.matrix(
    #         dplyr::select(tolerance, dplyr::contains("_mbr"))
    #       )
    #     )
    #   )

    good_obs <- tolerance %>%
      dplyr::filter(.data$min_diff <= .data$tolerance_allowed) %>%
      dplyr::select(.data$SID, .data$valid_dttm, !! parameter_quo) %>%
      dplyr::group_by(.data$SID, .data$valid_dttm) %>%
      dplyr::summarise(!! rlang::sym(parameter_name) := unique(!! parameter_quo)) %>%
      dplyr::ungroup()

    bad_obs  <- tolerance %>%
      dplyr::filter(.data$min_diff > .data$tolerance_allowed)

    if (nrow(bad_obs) > 0) {
      bad_obs <- bad_obs %>%
        dplyr::select(
          .data$SID, .data$valid_dttm, !!parameter_quo, .data$min_diff,
          .data$tolerance_allowed
        ) %>%
        dplyr::group_by(.data$SID, .data$valid_dttm) %>%
        dplyr::summarise(
          !!rlang::sym(parameter_name) := unique(!!parameter_quo),
          dist_to_fcst = min(.data$min_diff),
          tolerance    = mean(.data$tolerance_allowed)
        ) %>%
        dplyr::ungroup()
    }

    .fcst <- suppressWarnings(suppressMessages(harpCore::join_to_fcst(
      .fcst,
      dplyr::select(
        good_obs, .data$SID, .data$valid_dttm
      ),
      by = c("SID", "valid_dttm"),
      force = TRUE
    )))

    num_bad_obs <- nrow(bad_obs)
    if (num_bad_obs > 0) {
      cli::cli_inform(c(
        "!" = "{num_bad_obs} cases with more than {num_sd_allowed} std dev{?s} error.",
        "i" = "Removed cases can be seen in the \"removed_cases\" attribute."
      ))
    }

    attr(.fcst, "removed_cases") <- bad_obs

  } else {

    attr(.fcst, "removed_cases") <- NULL

  }

  .fcst

}
