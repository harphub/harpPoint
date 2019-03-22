#' Lags a forecast
#'
#' Lagging is done by supplying the parent forecast cycles. This function will
#' then work out which cycles belong to the parent and return the lagged
#' forecast with members from the child cycles appended to those of the parent
#' cycle.
#'
#' @param .fcst A harp_fcst object.
#' @param fcst_model The name of the forecast model in the harp_fcst object to
#'   be lagged. Must be quoted.
#' @param parent_cycles A numeric vector of forecast cycles that form the
#'   parents for the lagging. Members between parent cycles are the child
#'   cycles.
#' @param dierction The direction to do the lagging in. 1 (the default) lags
#'   backwards in time from the parent cycles and -1 lags forwards in time.
#'
#' @return A harp_fcst object with \code{fcst_model} now containing the lagged
#'   forecast.
#' @export
#'
#' @examples
lag_forecast <- function(.fcst, fcst_model, parent_cycles, direction = 1) {

  if (!direction %in% c(1, -1)) {
    stop("direction must be 1 to lag backwards in time, or -1 to lag forwards in time", call. = FALSE)
  }

  fcst_df <- .fcst[[fcst_model]]

  if (is.null(fcst_df)) {
    stop("fcst_model '",fcst_model,"' not found in .fcst.", call. = FALSE)
  }

  fcst_df <- fcst_df %>%
    dplyr::group_by(.data$fcst_cycle) %>%
    tidyr::nest() %>%
    dplyr::arrange(.data$fcst_cycle) %>%
    dplyr::mutate(
      parent_cycle = purrr::map_chr(
        .data$fcst_cycle, ~ formatC(find_parent(as.numeric(.x), parent_cycles, direction), width = 2, flag = "0")
      )
    )

  .fcst[[fcst_model]] <- purrr::map_dfr(split(fcst_df, fcst_df$parent_cycle), lag_cycle, direction)

  purrr::map(.fcst, tidyr::drop_na) %>%
    new_harp_fcst()

}

find_parent <- function(val, vec, direction) {
  if (direction == 1) {
    diffs <- vec - val
    if (all(diffs < 0)) {
      diffs <- diffs - diffs[1]
    }
  } else {
    diffs <- val - vec
    if (all(diffs < 0)) {
      diffs <- diffs - diffs[length(diffs)]
    }
  }
    vec[which(diffs == min(diffs[diffs >= 0]))]
}

lag_cycle <- function(df, direction) {
  parent_cycle   <- unique(df$parent_cycle)
  child_cycles   <- df$fcst_cycle[df$fcst_cycle != parent_cycle]
  num_children   <- length(child_cycles)
  lagged_df      <- dplyr::filter(df, .data$fcst_cycle == parent_cycle) %>%
    tidyr::unnest() %>%
    dplyr::select_if(~ !all(is.na(.))) %>%
    dplyr::select(-.data$parent_cycle)

  if (num_children > 0) {

    for (i in 1:num_children) {

      lag_hours <- (as.numeric(parent_cycle) - as.numeric(child_cycles[i])) * direction
      lag_hours[lag_hours < 0] <- lag_hours[lag_hours < 0] + 24

      lagged_df <- dplyr::inner_join(
        lagged_df,
        dplyr::filter(df, .data$fcst_cycle == child_cycles[i]) %>%
          tidyr::unnest() %>%
          dplyr::select_if(~ !all(is.na(.))) %>%
          dplyr::mutate(
            leadtime = .data$leadtime - lag_hours * direction,
            fcdate   = .data$fcdate   + lag_hours * 3600 * direction
          ) %>%
          dplyr::select(-.data$fcst_cycle, -.data$parent_cycle),
        by = intersect(c("SID", "fcdate", "leadtime", "validdate", "parameter", "units"), colnames(lagged_df)),
        suffix = c("", "_lag")
      )

    }

  }

  lagged_df

}


