
# Functions to call verification functions that are a bit more
# complicated than simply calling the function. These are internal to
# the package and not exported.

#####################################################################################

harp_rank_hist <- function (.fcst, .param) {

  # Separate out a vector of observations and matrix of member
  # forecasts and call the fast rankHistogram function from HARPrcpp

  param <- rlang::enquo(.param)
  obs   <- dplyr::pull(.fcst, !! param)
  eps   <- dplyr::select(.fcst, dplyr::contains("mbr")) %>%
    as.matrix()
  rankHistogram(obs, eps)

}

#####################################################################################

harp_probs <- function (.fcst, .param, thresholds, obs_prob = TRUE, fcst_type = "EPS") {

  # Separate out a matrix of member forecasts and call the fast fcprob
  # function from HARPrcpp. The columns then need naming

  fcst_col_name   <- ifelse (fcst_type == "EPS", "mbr", "det")
  eps             <- dplyr::select(.fcst, dplyr::contains(fcst_col_name))
  probs           <- fcprob(as.matrix(eps), thresholds) %>%
    tibble::as_tibble()
  colnames(probs) <- c(paste0("fcst_prob_", thresholds), "num_members", "ens_mean", "ens_var")
  probs           <- dplyr::select(probs, dplyr::contains("_prob_"))

  # Do the same for the observations to get a binary 1 or 0. Then bind the
  # binary observations and convert to tibble

  if (obs_prob) {

    param                <- rlang::enquo(.param)
    obs                  <- dplyr::select(.fcst, !! param)
    binary_obs           <- fcprob(as.matrix(obs), thresholds) %>%
      tibble::as_tibble()
    colnames(binary_obs) <- c(paste0("obs_prob_", thresholds), "numMember", "ensMean", "ensVar")

    probs <- probs %>%
      dplyr::bind_cols(obs) %>%
      dplyr::bind_cols(binary_obs) %>%
      dplyr::select(dplyr::contains("_prob_"))

  }

  probs

}

#####################################################################################

harp_ecoval <- function(
  obs,
  pred,
  costloss   = seq(0.05, 0.95, by = 0.05),
  thresholds = seq(0.05, 0.95, by = 0.05)
) {

  # We only want to return the outer envelope.

  fullValue <- ecoval(obs, pred, costloss = costloss, thresholds = thresholds)

  tibble::tibble(cost_loss_ratio = fullValue$cl, value = fullValue$value_env)

}


# OBSOLETE
harp_ecoval0 <- function(obs, pred, costloss = seq(0.05, 0.95, by = 0.05)) {

  # We only want to return the outer envelope. Nest in a 'try' as if there
  # are not enough data the value function from the verification package fails.
  # (should make a modified function returning NAs - and it needs speeding up!)

  err <- try(
    fullValue <- verification::value(obs, pred, cl = costloss, plot = FALSE),
    silent = TRUE
  )

  if (!inherits(err, "try-error")) {
    ecoval <- tibble::tibble(cost_loss_ratio = fullValue$cl, value = apply(fullValue$V, 1, max))
  } else {
    ecoval <- tibble::tibble(cost_loss_ratio = costloss, value = NA)
  }

  ecoval

}

#####################################################################################

harp_roc <- function(obs, pred, prob_thresholds = seq(0.05, 0.95, by = 0.05)) {

  # If there are not enough data, the roc.plot function from the verification package
  # fails, so wrap in a try.

  ROCall <- roc(obs, pred, thresholds = prob_thresholds)

  ROC <- tibble::tibble(
    probability_bin  = c(-0.05, ROCall$thresholds, 1.05),
    hit_rate         = c(1, ROCall$H, 0),
    false_alarm_rate = c(1, ROCall$F, 0)
  )

  list(roc_data = ROC, roc_area = ROCall$area)

}

# OBSOLETE:
harp_roc0 <- function(obs, pred, prob_thresholds = seq(0.05, 0.95, by = 0.05)) {

  # If there are not enough data, the roc.plot function from the verification package
  # fails, so wrap in a try.

  err <- try(
    ROCall <- verification::roc.plot(obs, pred, thresholds = prob_thresholds, plot = NULL),
    silent = TRUE
  )

  if (!inherits(err, "try-error")) {

    ROC <- tibble::tibble(
      probability_bin  = c(ROCall$plot.data[,1,1], 1.05),
      hit_rate         = c(ROCall$plot.data[,2,1], 0),
      false_alarm_rate = c(ROCall$plot.data[,3,1], 0)
    )
    ROCarea <- ROCall$roc.vol$Area

  } else {

    ROC     <- tibble::tibble(
      probability_bin   = prob_thresholds,
      hit_rate          = NA,
      false_alarm_rate  = NA
    )
    ROCarea <- NA

  }

  list(roc_data = ROC, roc_area = ROCarea)

}
