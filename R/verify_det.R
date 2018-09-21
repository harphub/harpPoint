#' Verify a deterministic forecast
#'
#' @param .fcst A harp deterministc forecast object with an obs column.
#' @param thresholds A numeric vector of thresholds for which to compute
#'   threshold based scores. If set to NULL, no threshold based scores will be
#'   computed.
#'
#' @return A list with two data frames: \cr \code{scores} - for non-threshold
#'   based scores, \cr\code{scores_thresh} - for threshold based scores.
#' @export
#'
#' @examples
verify_det <- function(.fcst, thresholds = NULL) {
#
###### Add check for class of intput object to decide on group_vars - currently ignores member
###### so can't do multi member verification for ensembles.
#

# Compute basic statistics

  message("Verifying for continuous scores\n")
  scores <- .fcst %>%
    dplyr::group_by(mname, leadtime) %>%
    dplyr::summarise(
      Bias = mean(forecast - obs, na.rm = TRUE),
      RMSE = sqrt(mean((forecast - obs) ^ 2, na.rm = TRUE)),
      MAE  = mean(abs(forecast - obs), na.rm = TRUE),
      STDE = sd(forecast - obs, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  if (is.null(thresholds)) {

    return(list(scores = scores, scores_thresh = NULL))

  } else {

#   Compute the binary probabilities for the thresholds

    message("Computing binary probabilities \n")
    verif_all <- .fcst %>%
      #dplyr::group_by(member) %>%
      dplyr::do(
        SID       = .$SID,
        fcdate    = .$fcdate,
        leadtime  = .$leadtime,
        validdate = .$validdate,
        mname     = .$mname,
        probs     = harp_probs(., thresholds, fcstType="DET")
      ) %>%
      dplyr::ungroup() %>%
      tidyr::unnest()

#   Gather the thresholds to make a tidy tibble

    scores_thresh <- verif_all %>%
      dplyr::select(-dplyr::contains("obs")) %>%
      tidyr::gather(dplyr::contains("pred"), key="threshold", value="pred") %>%
      dplyr::mutate(threshold = readr::parse_number(.data$threshold))

    obs_thresh   <- verif_all %>%
      dplyr::select(-dplyr::contains("pred")) %>%
      tidyr::gather(dplyr::contains("obs_"), key="threshold", value="binary_obs") %>%
      dplyr::mutate(threshold = readr::parse_number(.data$threshold))

    ### This is robust, but slow.
    #scores_thresh <- scores_thresh %>%
      #dplyr::inner_join(
        #obs_thresh,
        #by = c("SID", "fcdate", "leadtime", "validdate", "mname", "threshold")
      #) %>%
      #dplyr::select(-dplyr::contains("."))

    ### This is fast - appears to give the same results, but not fully tested
    scores_thresh <- scores_thresh %>%
      dplyr::select(
        .data$SID,
        .data$fcdate,
        .data$leadtime,
        .data$validdate,
        .data$mname,
        .data$threshold,
        .data$pred) %>%
      dplyr::bind_cols(dplyr::select(obs_thresh, .data$binary_obs))

#   Compute the verification scores

    message("\nComputing threshold scores \n")
    scores_thresh <- scores_thresh %>%
      dplyr::group_by(leadtime, mname, threshold) %>%
      dplyr::do(verif = harp_verify(.$binary_obs, .$pred, frcst.type="binary", obs.type="binary"))

#   Get the data from the verify object to columns in the output tibble

    scores_thresh <- scores_thresh %>%
      dplyr::ungroup() %>%
      dplyr::transmute(
        #member     = member,
        leadtime   = leadtime,
        mname      = mname,
        threshold  = purrr::map(threshold, ~ readr::parse_number(.x)),
        verifStats = purrr::map(verif, ~ dplyr::bind_cols(.x[2:33])),
        contTab    = purrr::map(
          verif,
          ~ tibble::as_tibble(.x$tab) %>% dplyr::rename(forecast = Var1, observed = Var2, count = n)
        )
      ) %>%
      tidyr::unnest(threshold, verifStats)

  }

  list(scores = scores, scores_thresh = scores_thresh)

}
