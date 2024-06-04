
# Functions to call verification functions that are a bit more
# complicated than simply calling the function. These are internal to
# the package and not exported.

#####################################################################################

harp_rank_hist <- function (.fcst, .param) {

  # Separate out a vector of observations and matrix of member
  # forecasts and call the fast rankHistogram function from HARPrcpp

  param <- rlang::enquo(.param)
  obs   <- dplyr::pull(.fcst, !! param)
  eps   <- dplyr::select(.fcst, dplyr::contains("_mbr")) %>%
    as.matrix()
  rankHistogram(obs, eps)

}

#####################################################################################

harp_probs <- function (
  .fcst,
  thresholds,
  comparator = c("ge", "gt", "le", "lt", "eq", "between", "outside"),
  include_low = TRUE,
  include_high = TRUE,
  .param = NULL,
  obs_prob = FALSE,
  fcst_type = "EPS"
) {

  comparator <- match.arg(comparator)

  thresholds <- check_thresholds(thresholds, comparator)

  # Separate out a matrix of member forecasts and call the fast fcprob
  # function from HARPrcpp. The columns then need naming

  fcst_col_name   <- ifelse (fcst_type == "EPS", "_mbr", "_det|^fcst$")
  eps             <- dplyr::select(.fcst, dplyr::matches(fcst_col_name))
  if (is.list(thresholds)) {
    probs <- do.call(
      cbind,
      lapply(
        thresholds,
        function(x) fcprob(
          as.matrix(eps), x, comparator, include_low, include_high
        )
      )
    )
  } else {
    probs <- fcprob(
      as.matrix(eps), thresholds, comparator, include_low, include_high
    )
  }
  colnames(probs) <- make_colnames(
    thresholds, comparator, include_low, include_high, "fcst_prob"
  )
  probs           <- tibble::as_tibble(probs)
  probs           <- dplyr::select(probs, dplyr::contains("_prob_"))

  # Do the same for the observations to get a binary 1 or 0. Then bind the
  # binary observations and convert to tibble

  if (obs_prob) {

    obs <- dplyr::select(.fcst, .data[[.param]])

    if (is.list(thresholds)) {
      binary_obs <- do.call(
        cbind,
        lapply(
          thresholds,
          function(x) fcprob(
            as.matrix(obs), x, comparator, include_low, include_high
          )
        )
      )
    } else {
      binary_obs <- fcprob(
        as.matrix(obs), thresholds, comparator, include_low, include_high
      )
    }
    colnames(binary_obs) <- make_colnames(
      thresholds, comparator, include_low, include_high, "obs_prob"
    )
    binary_obs           <- tibble::as_tibble(binary_obs)

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

check_thresholds <- function(th, comp, caller = rlang::caller_env()) {
  if (comp %in% c("between", "outside")) {
    if (!is.list(th)) {
      th <- list(th)
    }
    if (!all(vapply(th, length, numeric(1)) == 2)) {
      cli::cli_abort(c(
        "Incorrect {.arg thresholds} for {.arg comparator} = \"{comp}\"",
        "i" = paste(
          "For {.arg comparator} = \"{comp}\", {.arg thresholds} must",
          "be list of length 2 vectors."
        )
      ))
    }
  }
  th
}

make_colnames <- function(th, comp, il, ih, type) {
  if (!is.list(th)) {
    return(paste(type, comp, th, sep = "_"))
  }
  comp1 <- ifelse(il, "ge", "gt")
  comp2 <- ifelse(ih, "le", "lt")
  if (comp == "outside") {
    comp1 <- ifelse(il, "le", "lt")
    comp2 <- ifelse(ih, "ge", "gt")
  }
  vapply(
    th,
    function(x) paste(type, comp1, min(x), comp2, max(x), sep = "_"),
    character(1)
  )
}
