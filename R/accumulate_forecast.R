#' Accumulate a forecast over a given time period.
#'
#' Given an object of class \code{harp_point_forecast},
#' \code{accumulate_forecast} will return the same object, but with the forecast
#' column having the amount accumulated over the specified
#' \code{accumulation_time}. This is done through subtraction of the forecast at
#' lead time T - \code{accumulation_time} from the forecast at lead time T.
#' \code{accumulation_time} must therefore be in the same units as the
#' \code{leadtime} column.
#'
#' @param FCST A data frame of class \code{harp_point_forecast}.
#' @param accumulation_time The time over which the accumulation is to be
#'   calculated. Must be in the same time units as the \code{leadtime} column.
#'
#' @return A data frame of class \code{harp_point_forecast} with the
#'   \code{forecast} column containing the accumulated forecast.
#' @export
#'
#' @examples
accumulate_forecast <- function(FCST, accumulation_time) {

  message("Accumulating forecast for", accum, "hour accumulations")

  FCST <- FCST %>%
    dplyr::mutate(lead_acc = .data$leadtime - accumulation_time) %>%
    dplyr::inner_join(
      dplyr::select(
        FCST,
        .data$SID,
        .data$fcdate,
        lead_acc = .data$leadtime,
        fcst_acc = .data$forecast,
        .data$mname,
        .data$member
      ),
      by = c("SID", "fcdate", "lead_acc", "mname", "member")
    ) %>%
    dplyr::mutate(forecast = .data$forecast - .data$fcst_acc) %>%
    dplyr::select(-dplyr::contains("_acc"))

  FCST
}
