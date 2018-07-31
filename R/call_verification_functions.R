
# Functions to call verification functions that are a bit more
# complicated than simply calling the function. These are internal to
# the package and not exported.

#####################################################################################

call_rank_hist <- function (FCST) {

# Separate out a vector of observations and matrix of member
# forecasts and call the fast rankHistogram function from HARPrcpp

  obs <- dplyr::pull(FCST, .data$obs)
  eps <- dplyr::select(FCST, dplyr::contains("mbr")) %>%
    as.matrix()
  rankHistogram(obs, eps)

}

#####################################################################################

call_fcprob <- function (FCST, thresholds, fcstType = "EPS") {

# Separate out a matrix of member forecasts and call the fast fcprob
# function from HARPrcpp. The columns then need naming

  fcstColName     <- ifelse (fcstType == "EPS", "mbr", "forecast")
  eps             <- dplyr::select(FCST, dplyr::contains(fcstColName))
  probs           <- HARPrcpp::fcprob(as.matrix(eps), thresholds) %>%
    tibble::as_tibble()
  colnames(probs) <- c(paste0("pred_", thresholds), "num_members", "ens_mean", "ens_var")

# Do the same for the observations to get a binary 1 or 0. Then bind the
# binary observations and convert to tibble

  obs              <- dplyr::select(FCST, .data$obs)
  binObs           <- fcprob(as.matrix(obs), thresholds) %>%
    tibble::as_tibble()
  colnames(binObs) <- c(paste0("obs_", thresholds), "numMember", "ensMean", "ensVar")

  probs %>%
    dplyr::bind_cols(obs) %>%
    dplyr::bind_cols(binObs) %>%
    dplyr::mutate(bias = .data$ens_mean - .data$obs)

}

#####################################################################################

call_value <- function(obs, pred, costloss = seq(0.05, 0.95, by = 0.05)) {

# We only want to return the outer envelope. Nest in a 'try' as if there
# are not enough data the value function from the verification package fails.
# (should make a modified function returning NAs - and it needs speeding up!)

  err <- try(
    fullValue <- verification::value(obs, pred, cl = costloss, plot = FALSE),
    silent = TRUE
  )

  if (!inherits(err, "try-error")) {
    ecoval <- tibble::tibble(cl = fullValue$cl, value = apply(fullValue$V, 1, max))
  } else {
    ecoval <- tibble::tibble(cl = costloss, value = NA)
  }

  ecoval

}

#####################################################################################

call_roc <- function(obs, pred, prob_thresholds = seq(0.05, 0.95, by = 0.05)) {

# If there are not enough data, the roc.plot function from the verification package
# fails, so wrap in a try.

  err <- try(
    ROCall <- verification::roc.plot(obs, pred, thresholds = prob_thresholds, plot = NULL),
    silent = TRUE
  )

  if (!inherits(err, "try-error")) {

    ROC <- tibble::tibble(
      prob = ROCall$plot.data[,1,1],
      HR   = ROCall$plot.data[,2,1],
      FAR  = ROCall$plot.data[,3,1]
    )
    ROCarea <- ROCall$roc.vol$Area

  } else {

    ROC     <- tibble::tibble(
      prob = prob_thresholds,
      HR   = NA,
      FAR  = NA
    )
    ROCarea <- NA

  }

  list(roc_data = ROC, roc_area = ROCarea)

}
