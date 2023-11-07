#' Jitter a forecast to account for observation errors.
#'
#' To account for observation errors in ensemble verification the forecast
#' values can be perturbed by sampling from a specified error distribution.
#' Jittering the forecast is only likely to have an effect on the ensemble
#' spread and rank histograms.
#'
#' Note that the jitter function should be a function that works on a vector of
#' forecast values and it should have only one or two arguments. The first
#' argument is for the forecast data, and if there is a second argument, that is
#' for the observations.
#'
#' @param .fcst An object of class 'harp_fcst'.
#' @param jitter_func The function to be applied to the forecast values.
#' @param obs_col The observations column if it is to be used in the jitter
#'   function. Can be the column name, quoted, or unquoted. If a variable it
#'   should be embraced - i.e. wrapped in `{{}}`.
#' @param ... Other arguments to `jitter_func`.
#'
#' @return An object of the same class as `.fcst` with jittered forecast values.
#' @export
#'
jitter_fcst <- function(.fcst, jitter_func, obs_col = NULL, ...) {
  if (!is.function(jitter_func)) {
    cli::cli_abort("{.arg jitter_func} must be a function.")
  }
  UseMethod("jitter_fcst")
}

#' @export
jitter_fcst.harp_ens_point_df <- function(
    .fcst, jitter_func, obs_col = NULL, ...
) {

  if (rlang::quo_is_null(rlang::enquo(obs_col))) {
    return(
      dplyr::mutate(
        .fcst,
        dplyr::across(
          dplyr::matches("_mbr[[:digit:]]{3}"),
          ~jitter_func(.x, ...)
        )
      )
    )
  }

  obs_col <- rlang::ensym(obs_col)
  dplyr::mutate(
    .fcst,
    dplyr::across(
      dplyr::matches("_mbr[[:digit:]]{3}"),
      ~jitter_func(.x, !!obs_col, ...)
    )
  )
}

#' @export
jitter_fcst.harp_list <- function(.fcst, jitter_func, obs_col = NULL, ...) {
  harpCore::as_harp_list(
    lapply(.fcst, jitter_fcst, jitter_func, {{obs_col}}, ...)
  )
}
