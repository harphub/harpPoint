#' Read forecast and observations and verify.
#'
#' This is a wrapper for the verification process. Forecasts and observations
#' are read in, filtered down to common cases, errors checked, and a full
#' verification is done for all scores. To minimise memory usage, the
#' verification can be done for one lead time at time. It would also be possible
#' to parallelise the process using for example \link[parallel]{mclapply}, or
#' \link[furrr]{future_map}.
#'
#' @param start_date Start date to for the verification. Should be numeric or
#'   character. YYYYMMDD(HH)(mm).
#' @param end_date End date for the verification. Should be numeric or
#'   character.
#' @param parameter The parameter to verify.
#' @param fcst_model The forecast model(s) to verify. Can be a single string or
#'   a character vector of model names.
#' @param fcst_path The path to the forecast FCTABLE files.
#' @param obs_path The path to the observatoin OBSTABLE files.
#' @param lead_time The lead times to verify.
#' @param num_iterations The number of iterations per verification calculation.
#'   The default is to do the same number of iterations as there are lead times.
#'   If a small number of iterations is set, it may be useful to set
#'   \code{show_progress = TRUE}. The higher the number of iterations, the
#'   smaller the amount of data that is held in memory at any one time.
#' @param thresholds The thresholds to compute categorical scores for.
#' @param members The members to include in the iteration. This will select the
#'   same member numbers from each \code{fcst_model}. In the future it will
#'   become possible to specify members for each \code{fcst_model}.
#' @param obsfile_template The template for OBSTABLE files - the default is
#'   "obstable", which is \code{OBSTABLE_{YYYY}.sqlite}.
#' @param groupings The groups to verify for. The default is "leadtime". Another
#'   common grouping might be \code{groupings = c("leadtime", "fcst_cycle")}.
#' @param by The frequency of forecast cycles to verify.
#' @param climatology The climatology to use for the Brier Skill Score. Can be
#'   "sample" for the sample climatology (the default), a named list with
#'   elements eps_model and member to use a member of an eps model in the
#'   harp_fcst object for the climatology, or a data frame with columns for
#'   threshold and climatology and also optionally leadtime.
#' @param stations The stations to verify for. The default is to use all
#'   stations from \link[harpIO]{station_list} that are common to all
#'   \code{fcst_model} domains.
#' @param jitter_fcst A function to perturb the forecast values by. This is used
#'   to account for observation error in the rank histogram. For other
#'   statistics it is likely to make little difference since it is expected that
#'   the observations will have a mean error of zero.
#' @param gross_error_check Logical of whether to perform a gross error check.
#' @param min_allowed The minimum value of observation to allow in the gross
#'   error check. If set to NULL the default value for the parameter is used.
#' @param max_allowed The maximum value of observation to allow in the gross
#'   error check. If set to NULL the default value for the parameter is used.
#' @param num_sd_allowed The number of standard deviations of the forecast that
#'   the obseravtions should be within. Set to NULL for automotic value
#'   depeninding on parameter.
#' @param show_progress Logical - whether to show a progress bar. Defaults to
#'   FALSE.
#' @param verif_path If set, verification files will be saved to this path.
#'
#' @return A list containting two data frames: \code{ens_summary_scores} and
#'   \code{ens_threshold_scores}.
#' @export
#'
#' @examples
ens_read_and_verify <- function(
  start_date,
  end_date,
  parameter,
  fcst_model,
  fcst_path,
  obs_path,
  lead_time           = seq(0, 48, 3),
  num_iterations      = length(lead_time),
  thresholds          = NULL,
  members             = NULL,
  obsfile_template    = "obstable",
  groupings           = "leadtime",
  by                  = "1d",
  climatology         = "sample",
  stations            = NULL,
  jitter_fcst         = NULL,
  gross_error_check   = TRUE,
  min_allowed         = NULL,
  max_allowed         = NULL,
  num_sd_allowed      = NULL,
  show_progress       = FALSE,
  verif_path          = NULL
) {

  first_obs <- start_date
  last_obs  <- (str_datetime_to_unixtime(end_date) + 3600 * max(lead_time)) %>%
    unixtime_to_str_datetime(YMDhm)

  obs_data <- read_point_obs(
    start_date        = first_obs,
    end_date          = last_obs,
    parameter         = parameter,
    obs_path          = obs_path,
    obsfile_template  = obsfile_template,
    gross_error_check = gross_error_check,
    min_allowed       = min_allowed,
    max_allowed       = max_allowed
  )

  verif_data   <- list()

  parameter_sym <- rlang::sym(parameter)

  if (num_iterations > length(lead_time)) {
    num_iterations <- length(lead_time)
  }

  lead_list <- split(lead_time, sort(seq_along(lead_time) %% num_iterations))

  for (i in 1:num_iterations) {

    cat("Lead time:", lead_list[[i]], "( Iteration", i, "of", num_iterations, ")\n")
    cat(rep("=", 80), "\n", sep = "")

    fcst_data <- harpIO::read_point_forecast(
      start_date = start_date,
      end_date   = end_date,
      fcst_model = fcst_model,
      fcst_type  = "EPS",
      parameter  = parameter,
      lead_time  = lead_list[[i]],
      by         = by,
      file_path  = fcst_path,
      stations   = stations,
      members    = members
    ) %>%
      merge_multimodel() %>%
      dplyr::filter(.data$leadtime %in% lead_list[[i]]) %>%
      common_cases()

    fcst_data <- join_to_fcst(fcst_data, obs_data) %>%
      check_obs_against_fcst(!! parameter_sym, num_sd_allowed = num_sd_allowed)

    verif_data[[i]] <- ens_verify(
      fcst_data,
      !! parameter_sym,
      thresholds    = thresholds,
      groupings     = groupings,
      jitter_fcst   = jitter_fcst,
      climatology   = climatology,
      show_progress = show_progress
    )

  }

#  verif_attr < attributes(verif_data)
  verif_data <- list(
    ens_summary_scores   = purrr::map(verif_data, "ens_summary_scores") %>% dplyr::bind_rows(),
    ens_threshold_scores = purrr::map(verif_data, "ens_threshold_scores") %>% dplyr::bind_rows()
  )
#  attributes(verif_data) <- verif_attr

  if (!is.null(verif_path)) {
    harpIO::save_point_verif(verif_data, verif_path = verif_path)
  }

  verif_data

}
