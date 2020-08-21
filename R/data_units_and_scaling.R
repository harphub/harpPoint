#' Set the units for meteorological point data.
#'
#' Note that the contents of the data are not changed in any way. Rather, if a
#' column called 'units' exists, its contents are modified to be that passed in
#' the units_name argument - if the 'units' column does not exist, it is added.
#'
#' @param point_data A harp_fcst object, or a data frame.
#' @param units_name The name of the units of the data, e.g. "m/s", "kg/m\^2" etc.
#'
#' @return A An object of the same type as \code{point_data}, but with the units
#'   column modified to contain \code{units_name}, or the units column added.
#' @export
#'
#' @examples
set_units <- function(point_data, units_name) {
  UseMethod("set_units")
}

#' @export
set_units.default <- function(point_data, units_name) {

  # Check for data columns so that units is added between the metadata and the data

  # Ensemble forecast data
  data_cols <- grep("[[:graph:]]+mbr+[[:digit:]]+", colnames(point_data), perl = TRUE, value = TRUE)

  # Deterministic forecast data
  if (length(data_cols) < 1) {
    data_cols <- grep("[[:graph:]]+det$", colnames(point_data), perl = TRUE, value = TRUE)
  }

  # Other (probably observations) data
  if (length(data_cols) < 1) {
    data_cols <- setdiff(colnames(point_data), c("SID" , "validdate", "lat", "lon", "units"))
  }

  # Mutate units column and return data frame with metadata columns followed by data columns
  metadata_cols <- setdiff(colnames(point_data), data_cols)
  if (!is.element("units", metadata_cols)) metadata_cols <- c(metadata_cols, "units")

  point_data <- dplyr::mutate(point_data, units = units_name)

  point_data[c(metadata_cols, data_cols)]

}

#' @export
set_units.harp_fcst <- function(point_data, units_name) {
  new_harp_fcst(purrr::map(point_data, set_units, units_name))
}


#' Scale forecast data
#'
#' If you wish to scale the forecast values, for example when temperature data
#' are in Kelvin and you want them in degrees C, this function can be used to
#' scale the data.
#'
#' @param .fcst A \code{harp_fcst} object as read in by read_point_forecast from
#'   harpIO.
#' @param scale_factor The scaling factor.
#' @param new_units The name of the new units - if set to NULL, the name is not
#'   changed.
#' @param multiplicative If the scaling is to be done multiplicatively, i.e. the
#'   new forecast value is to be the old value * scale_factor, set
#'   multiplicative to TRUE. The defualt (multiplicative = FALSE) is to do the
#'   scaling additively, i.e. the new forecast value is the old value +
#'   scale_factor.
#'
#' @return A \code{harp_fcst} object with forecast values scaled by
#'   \code{scale_factor}.
#' @export
#'
#' @examples
scale_point_forecast <- function(.fcst, scale_factor, new_units = NULL, multiplicative = FALSE) {
  UseMethod("scale_point_forecast")
}

#' @export
scale_point_forecast.default <- function(.fcst, scale_factor, new_units = NULL, multiplicative = FALSE) {

  if (multiplicative) {
    scale_function <- function(x, sf) {
      x * sf
    }
  } else {
    scale_function <- function(x, sf) {
      x + sf
    }
  }

  # Ensemble forecast data
  data_cols <- union(
    grep("[[:graph:]]+mbr+[[:digit:]]+$", colnames(.fcst), perl = TRUE, value = TRUE),
    grep("[[:graph:]]+mbr+[[:digit:]]+_lag$", colnames(.fcst), perl = TRUE, value = TRUE)
  )

  # Deterministic forecast data
  if (length(data_cols) < 1) {
    data_cols <- grep("[[:graph:]]+det$", colnames(.fcst), perl = TRUE, value = TRUE)
  }

  # Error
  if (length(data_cols) < 1) {
    stop("No forecast data columns found. Are you sure this is a harp_fcst object?", call. = FALSE)
  }

  .fcst <- dplyr::mutate_at(.fcst, data_cols, scale_function, scale_factor)

  # Add or modify units if new_units is passed
  if (!is.null(new_units)) {
    .fcst <- set_units(.fcst, new_units)
  }

  .fcst
}

#' @export
scale_point_forecast.harp_fcst <- function(.fcst, scale_factor, new_units = NULL, multiplicative = FALSE) {
  new_harp_fcst(purrr::map(.fcst, scale_point_forecast, scale_factor, new_units, multiplicative))
}


#' Scale observations data
#'
#' If you wish to scale the observations values, for example when temperature
#' data are in Kelvin and you want them in degrees C, this function can be used
#' to scale the data.
#'
#' @param .obs A data frame of point observations as read in by read in by
#'   read_point_obs from harpIO.
#' @param parameter The column name of the data to be scaled. Must be unquoted.
#' @param scale_factor The scaling factor.
#' @param new_units The name of the new units - if set to NULL, the name is not
#'   changed.
#' @param multiplicative If the scaling is to be done multiplicatively, i.e. the
#'   new forecast value is to be the old value * scale_factor, set
#'   multiplicative to TRUE. The defualt (multiplicative = FALSE) is to do the
#'   scaling additively, i.e. the new forecast value is the old value +
#'   scale_factor.
#'
#' @return The observations data frame with the parameter column scaled by
#'   \code{scale_factor}.
#' @export
#'
#' @examples
scale_point_obs <- function(.obs, parameter, scale_factor, new_units = NULL, multiplicative = FALSE) {

  parameter      <- rlang::enquo(parameter)
  parameter_name <- rlang::quo_name(parameter)
  parameter_sym  <- rlang::sym(parameter_name)

  if (!is.element(parameter_name, colnames(.obs))) {
    stop("Parameter \"", parameter_name, "\" not found in .obs", call. = FALSE)
  }

  if (multiplicative) {
    scale_function <- function(x, sf) {
      x * sf
    }
  } else {
    scale_function <- function(x, sf) {
      x + sf
    }
  }

  .obs <- dplyr::mutate(.obs, !!parameter_name := scale_function(!!parameter_sym, scale_factor))

  if (!is.null(new_units)) {
    .obs <- set_units(.obs, new_units)
  }

  .obs

}
