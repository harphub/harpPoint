#' Title
#'
#' @param FCST A long form data frame that includes column names \code{mname},
#'   \code{SID}, \code{fcdate}, \code{leadtime}, \code{member}, \code{forecast}
#'   and \code{obs}. Note that \code{fcdate} is the date in Unix time format
#'   i.e. seconds since 1970-01-01 00:00:00 UTC.
#' @param thresholds A numeric vector of thresholds for which to compute
#'   threshold based scores.
#' @param calc_uui Whether to compute the unbiased u-statistic spread-skill.
#'   TRUE or FALSE. Note that this is quite slow!
#' @param obs_error_func A function to model the observation error.
#'
#' @return A list with two data frames: \cr \code{scores} - for non-threshold
#'   based scores, \cr\code{scores_thresh} - for threshold based scores.
#' @export
#'
#' @examples
#'
verify_eps <- function(
  FCST,
  thresholds     = NA,
  calc_uui       = FALSE,
  obs_error_func = function(x) x + 0
) {

# Check the column names in FCST

  FCST_col_names <- c("mname", "member", "SID", "fcdate", "leadtime", "forecast", "obs")
  if (!identical(intersect(FCST_col_names, colnames(FCST)), FCST_col_names)) {
    stop(
      paste(
        "Input FCST data frame does not have the correct column names.",
        "Supplied column names: ", paste(colnames(FCST), collapse = " "),
        "Should include: ", paste(FCST_col_names, collapse = " "),
        sep = "\n   "
      )
    )
  }

# Set column names to NULL to avoid scoping for global variables - Using the
# .data pronoun in summarise creates a massive slow down (up to 10 x slower).
# Doing this means that we can get away without using it.

  mname    <- NULL
  member   <- NULL
  SID      <- NULL
  fcdate   <- NULL
  leadtime <- NULL
  forecast <- NULL
  obs      <- NULL
  ens_mean <- NULL
  ens_var  <- NULL

  cat("Verifying EPS : \n")

  FCST <- dplyr::mutate(FCST, forecast = obs_error_func(.data$forecast))

# Compute the basic statistics (RMSE,Bias,ensemble mean,)

  cat("   Computing basic statistics")
  scores <- FCST %>%
    dplyr::group_by(.data$mname, .data$SID, .data$fcdate, .data$leadtime) %>%
    dplyr::summarise(ens_mean = mean(forecast), ens_var = stats::var(forecast), obs = mean(obs)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$leadtime, .data$mname) %>%
    dplyr::summarise(
      mean_bias = mean(ens_mean - obs, na.rm = TRUE),
      rmse      = sqrt(mean((ens_mean - obs) ^ 2, na.rm = TRUE)),
      spread    = sqrt(mean(ens_var))
    ) %>%
    dplyr::ungroup()
  cat(" ---> DONE\n")

# Compute the CRPS, rank histogram and probabilities for threshold preserving leadtime and mname

  cat("   Computing full ensemble scores and probabilities \n")
  verif_full <- FCST %>%
    dplyr::group_by(.data$leadtime, .data$mname) %>%
    dplyr::do(
      temp_crps      = harp_crps(tidyr::spread(., member, forecast)),
      temp_rank_hist = harp_rank_hist(tidyr::spread(., member, forecast)),
      temp_probs     = harp_probs(tidyr::spread(., member, forecast), thresholds)
    ) %>%
    dplyr::ungroup()
  cat(" ---> DONE\n")

# Alternative method - awaiting purrr progress bars, but it is a little bit faster.
# Will also be able to modify for parallel processing with furrr.
# VERIFall <- FCST %>%
#   dplyr::group_by(.data$leadtime, .data$mname) %>%
#   tidyr::nest() %>%
#   dplyr::transmute(
#     .data$mname,
#     .data$leadtime,
#     temp_crps      = purrr::map(data, ~ tidy_crps(tidyr::spread(.x, .data$member, .data$forecast))),
#     temp_rank_hist = purrr::map(data, ~ call_rank_hist(tidyr::spread(.x, .data$member, data$forecast))),
#     temp_probs     = purrr::map(data, ~ call_fcprob(tidyr::spread(.x, .data$members, .data$forecast)))
#   )

# Append data from verif_all to the verif_scores tibble

  verif_full <- verif_full %>%
    dplyr::mutate(
      num_cases  = purrr::map_int(.data$temp_probs, nrow),
      crps       = purrr::map_dbl(.data$temp_crps, "CRPS"),
      cprs_pot   = purrr::map_dbl(.data$temp_crps, "CRPSpot"),
      crps_rel   = purrr::map_dbl(.data$temp_crps, "Reli"),
      rank       = purrr::map(.data$temp_rank_hist, ~ seq(1, length(.x))),
      rank_count = .data$temp_rank_hist
    )

  scores <- dplyr::inner_join(
    scores,
    dplyr::select(verif_full, -dplyr::starts_with("temp_")),
    by = c("mname", "leadtime")
  )

# Compute the UUI spread-skill

  if (calc_uui) {
    cat("   Computing UUI spread-skill \n")
    UUI <- uui(FCST, verbose = TRUE)
    scores <- scores %>% dplyr::left_join(UUI, by = c("leadtime", "mname"))
    cat("   Computing UUI spread-skill ---> DONE\n")
  }

# Compute the verification for thresholds - first gather the thresholds together

  if (all(is.finite(thresholds))) {
    cat("   Computing scores for thresholds \n")
    verif_thresh <- verif_full %>%
      dplyr::select(.data$leadtime, .data$mname, .data$temp_probs) %>%
      tidyr::unnest(.data$temp_probs) %>%
      dplyr::select(-dplyr::contains("obs")) %>%
      tidyr::gather(dplyr::contains("pred"), key = "threshold", value = "pred") %>%
      dplyr::transmute(
        .data$leadtime,
        .data$mname,
        threshold = readr::parse_number(.data$threshold),
        .data$pred
      )

    obs_thresh <- verif_full %>%
      dplyr::select(.data$leadtime, .data$mname, .data$temp_probs) %>%
      tidyr::unnest(.data$temp_probs) %>%
      dplyr::select(-dplyr::contains("pred")) %>%
      tidyr::gather(dplyr::contains("obs_"), key = "threshold", value = "binary_obs") %>%
      dplyr::transmute(
        .data$leadtime,
        .data$mname,
        threshold = readr::parse_number(.data$threshold),
        .data$binary_obs
      )

    verif_thresh <- verif_thresh %>%
      dplyr::inner_join(
        obs_thresh,
        by = c("leadtime", "mname", "threshold")
      )

    verif_thresh <- verif_thresh %>%
      dplyr::group_by(.data$leadtime, .data$mname, .data$threshold) %>%
      dplyr::do(
        verif  = verification::verify(.$binary_obs, .$pred, show = FALSE),
        ecoval = call_value(.$binary_obs, .$pred),
        roc    = call_roc(.$binary_obs, .$pred)
      ) %>%
      dplyr::ungroup()
    cat(" ---> DONE\n")

#   Create the ScoresThresh tibble extracting scores from the verify object in VERIFthresh

    scores_thresh <- verif_thresh %>%
      dplyr::transmute(
        leadtime  = .data$leadtime,
        mname     = .data$mname,
        threshold = purrr::map(.data$threshold, ~ readr::parse_number(.x)),
        BS        = purrr::map(.data$verif, "bs"),
        BSS       = purrr::map(.data$verif, "ss"),
        BSrel     = purrr::map(.data$verif, "bs.reliability"),
        BSres     = purrr::map(.data$verif, "bs.resol"),
        BSunc     = purrr::map(.data$verif, "bs.uncert")
      ) %>%
      tidyr::unnest()

#   Extract the reliability data from VERIFthresh and append to ScoresThresh

    scores_thresh <- scores_thresh %>%
      dplyr::inner_join(
        verif_thresh %>%
          dplyr::transmute(
            leadtime  = .data$leadtime,
            mname     = .data$mname,
            threshold = purrr::map(.data$threshold, ~ readr::parse_number(.x)),
            prob      = purrr::map(.data$verif, "y.i"),
            freq      = purrr::map(.data$verif, "obar.i"),
            prop      = purrr::map(.data$verif, "prob.y"),
            roc_area  = purrr::map(.data$roc, "roc_area")
          ) %>%
          tidyr::unnest(.data$threshold, .data$roc_area),
        by = c("leadtime", "mname", "threshold")
      ) %>%
      tidyr::nest(.data$prob, .data$freq, .data$prop, .key = reliability)

#   Append the remaining list columns to ScoresThresh

    scores_thresh$ecoval <- verif_thresh$ecoval
    scores_thresh$ROC    <- verif_thresh$roc %>% lapply(function(x) x$roc_data)

  } else {
    scores_thresh = NULL
  }

# Return the scores and scores_thresh tibbles

  list(scores = scores, scores_thresh = scores_thresh)

}

