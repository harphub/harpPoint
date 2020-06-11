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

  if (!is.list(groupings)) {
    groupings <- list(groupings)
  }

  col_names  <- colnames(.fcst)
  parameter  <- rlang::enquo(parameter)
  chr_param  <- rlang::quo_name(parameter)
  if (length(grep(chr_param, col_names)) < 1) {
    stop(paste("No column found for", chr_param), call. = FALSE)
  }

  if (is.function(jitter_fcst)) {
    .fcst <- dplyr::mutate_at(.fcst,  dplyr::vars(dplyr::contains("_mbr")), ~ purrr::map_dbl(., jitter_fcst))
  }

  ens_mean <- "ss_mean"
  ens_var  <- "ss_var"

  .fcst <- ens_mean_and_var(.fcst, mean_name = ens_mean, var_name = ens_var)

  compute_spread_skill <- function(compute_group, fcst_df) {
    if (length(compute_group) == 1 && compute_group == "threshold") {
      grouped_fcst <- fcst_df
    } else {
      compute_group <- rlang::syms(compute_group[compute_group != "threshold"])
      grouped_fcst  <- dplyr::group_by(fcst_df, !!! compute_group)
    }

    grouped_fcst %>%
      dplyr::summarise(
        num_cases = dplyr::n(),
        mean_bias = mean(.data[[ens_mean]] - !! parameter),
        stde      = stats::sd(.data[[ens_mean]] - !! parameter),
        rmse      = sqrt(mean((.data[[ens_mean]] - !! parameter) ^ 2)),
        spread    = sqrt(mean(.data[[ens_var]])),
      ) %>%
      dplyr::mutate(
        spread_skill_ratio = .data[["spread"]] / .data[["rmse"]]
      )
  }

  purrr::map_dfr(groupings, compute_spread_skill, .fcst) %>%
    fill_group_na(groupings)

}

#' @export
ens_spread_and_skill.harp_fcst <- function(.fcst, parameter, groupings = "leadtime", jitter_fcst = NULL) {

  parameter   <- rlang::enquo(parameter)
  if (!inherits(try(rlang::eval_tidy(parameter), silent = TRUE), "try-error")) {
    if (is.character(rlang::eval_tidy(parameter))) {
      parameter <- rlang::eval_tidy(parameter)
      parameter <- rlang::ensym(parameter)
    }
  }

  list(
    ens_summary_scores = purrr::map(.fcst, ens_spread_and_skill, !! parameter, groupings, jitter_fcst) %>%
    dplyr::bind_rows(.id = "mname"),
    ens_threshold_scores = NULL
  ) %>%
    add_attributes(.fcst, !! parameter)

}


