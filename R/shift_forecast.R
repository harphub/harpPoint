#' Shift a forecast
#'
#' This function is used to shift the start times and lead times of forecasts to
#' simulate lagging.
#'
#' @param .fcst A harp_fcst object as created by \link{read_point_forecast}, or
#'   a data frame with columns including 'fcdate' (in seconds) and 'leadtime'
#'   (in hours).
#' @param fcst_shifts A named list with names that exist in '.fcst' or a single
#'   numeric value to apply to all forecast models. If a list, each element must
#'   be numeric and of length 1 as only 1 shift can be applied to each forecast.
#'   The shifts are specified in hours. Postive values will shift the forecast
#'   start dates forward in time and reduce the lead times by the corresponding
#'   amounts. Negative values will do the opposite.
#' @return An object of the same class as '.fcst' with the forecast start times
#'   and lead times shifted for the forecast models and number hours given in
#'   'fcst_shifts'.
#' @export
shift_forecast <- function(.fcst, fcst_shifts, keep_unshifted = FALSE, drop_negative_lead_times = TRUE) {

  UseMethod("shift_forecast")
}

#' @export
shift_forecast.default <- function(.fcst, fcst_shifts, drop_negative_lead_times = TRUE) {

  if (length(fcst_shifts) > 1) {
    stop("Only one 'fcst_shifts' allowed per forecast model.", call. = FALSE)
  }

  colnames(.fcst) <- suppressWarnings(harpCore::psub(
    colnames(.fcst),
    c("fcdate", "validdate", "leadtime"),
    c("fcst_dttm", "valid_dttm", "lead_time")
  ))

  .fcst <- .fcst %>%
    dplyr::mutate(
      fcst_dttm  = .data$fcst_dttm + fcst_shifts * 3600,
      lead_time  = .data$lead_time - fcst_shifts,
      fcst_cycle = substr(harpIO::unixtime_to_str_datetime(.data$fcst_dttm, harpIO::YMDh), 9, 10)
    )

  if (drop_negative_lead_times) {
    .fcst <- dplyr::filter(.fcst, .data$lead_time >= 0)
  }

  harpCore::as_harp_df(.fcst)

}

#' @export
shift_forecast.harp_list <- function(.fcst, fcst_shifts, keep_unshifted = FALSE, drop_negative_lead_times = TRUE) {

  if (!is.list(fcst_shifts)) {
    if (length(fcst_shifts) > 1) {
      stop("'fcst_shifts' should either be a single numeric value or a named list.", call. = FALSE)
    }
    if (length(.fcst) > 1) {
      warning("Only one 'fcst_shifts' supplied. Applying to all forecast models.", immediate. = TRUE, call. = FALSE)
    }
    fcst_shifts        <- rep(list(fcst_shifts), length(.fcst))
    names(fcst_shifts) <- names(.fcst)
  } else {
    if (length(fcst_shifts) == 1 && is.null(names(fcst_shifts)) && length(.fcst) > 1 ) {
      warning("'fcst_shifts' is not a named list. Applying to all forecast models.", immediate. = TRUE, call. = FALSE)
      fcst_shifts        <- rep(fcst_shifts, length(.fcst))
      names(fcst_shifts) <- names(.fcst)
    }
    if (length(fcst_shifts) != length(.fcst) && length(fcst_shifts) > 1) {
      stop("'fcst_shifts' must be of length 1 or the same as the length of '.fcst': ", length(.fcst), call. = FALSE)
    }
    if (is.null(names(fcst_shifts))) {
      warning("No names supplied for 'fcst_shifts' - assuming the same order as '.fcst'", immediate. = TRUE, call. = FALSE)
      names(fcst_shifts) <- names(.fcst)
    }
    bad_names <- setdiff(names(fcst_shifts), names(.fcst))
    if (length(bad_names) > 0) {
      stop(paste(bad_names, collapse = ", "), " not found in .fcst", call. = FALSE)
    }
  }

  list_names <- names(fcst_shifts)
  if (keep_unshifted) {
    list_names <- mapply(function(x, y) paste0(x, "_shifted_", y, "h"), list_names, fcst_shifts, USE.NAMES = FALSE)
  }
  .fcst[list_names] <- purrr::map2(.fcst[names(fcst_shifts)], fcst_shifts, shift_forecast, drop_negative_lead_times)
  if (!keep_unshifted) {
    shifted_names <- sapply(list_names, function(x) which(names(.fcst) == x), USE.NAMES = FALSE)
    names(.fcst)[shifted_names] <- mapply(
      function(x, y) paste0(x, "_shifted_", y, "h"),
      list_names,
      fcst_shifts,
      USE.NAMES = FALSE
    )
  }
  harpCore::as_harp_list(.fcst)
}

