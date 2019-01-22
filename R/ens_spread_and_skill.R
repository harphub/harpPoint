#' Compute the skill (RMSE) and spread of an ensemble forecast
#'
#' The ensemble mean and spread are computed as columns in a \code{harp_fcst}
#' object. Typically the scores are aggregated over lead time by other grouping
#' variables cam be chosen. The mean bias is also computed.
#'
#' @param .fcst A \code{harp_fcst} object with tables that have a column for
#'   observations, or a single forecast table.
#' @param parameter The name of the column for the observed data.
#' @param groupings The groups for which to compute the ensemble mean and
#'   spread. See \link[dplyr]{group_by} for more information of how grouping
#'   works.
#' @param jitter_fcst A function to perturb the forecast values by. This is used
#'   to account for observation error in the spread. For other statistics it is
#'   likely to make little difference since it is expected that the observations
#'   will have a mean error of zero.
#'
#' @return An object of the same format as the inputs but with data grouped for
#'   the \code{groupings} column(s) and columns for \code{rmse}, \code{spread}
#'   and \code{mean_bias}.
#' @export
#'
#' @examples
ens_spread_and_skill <- function(.fcst, parameter, groupings = "leadtime", jitter_fcst = NULL) {
  UseMethod("ens_spread_and_skill")
}

#' @export
ens_spread_and_skill.default <- function(.fcst, parameter, groupings = "leadtime", jitter_fcst = NULL) {

  col_names  <- colnames(.fcst)
  parameter  <- rlang::enquo(parameter)
  chr_param  <- rlang::quo_name(parameter)
  groupings  <- rlang::syms(groupings)
  if (length(grep(chr_param, col_names)) < 1) {
    stop(paste("No column found for", chr_param), call. = FALSE)
  }

  if (is.function(jitter_fcst)) {
    .fcst <- dplyr::mutate_at(.fcst,  dplyr::vars(dplyr::contains("_mbr")), ~ purrr::map_dbl(., jitter_fcst))
  }

  .fcst <- ens_mean_and_var(.fcst, mean_name = "ss_mean", var_name = "ss_var")

  ens_mean <- rlang::sym("ss_mean")
  ens_var  <- rlang::sym("ss_var")

  .fcst %>%
    dplyr::group_by(!!! groupings) %>%
    dplyr::summarise(
      num_cases = dplyr::n(),
      mean_bias = mean(!! ens_mean - !! parameter),
      rmse      = sqrt(mean((!! ens_mean - !! parameter) ^ 2)),
      spread    = sqrt(mean(!! ens_var))
    )
}

#' @export
ens_spread_and_skill.harp_fcst <- function(.fcst, parameter, groupings = "leadtime", jitter_fcst = NULL) {
  parameter <- rlang::enquo(parameter)
  list(
    ens_summary_scores = purrr::map(.fcst, ens_spread_and_skill, !! parameter, groupings, jitter_fcst) %>%
    dplyr::bind_rows(.id = "mname"),
    ens_threshold_scores = NULL
  ) %>%
    add_attributes(.fcst, !! parameter)

}


