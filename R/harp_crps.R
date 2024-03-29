# Modified version of the crps function from the verification package to be used in harp.
# The function is internal to the harpPoint package.
#
harp_crps <- function (.fcst, .param) {

  param <- rlang::enquo(.param)
  obs <- .fcst %>% dplyr::pull(!! param)
  # Accounting problems occur when there are many forecast -
  # observation pairs that have the exact same value, e.g. for
  # cloud amount in oktas. This is fixed by adding a very small
  # random number to the observations - here 1e-6 * std dev of obs
  # is used as the standard deviation in the random number generation
  # from a Gaussian distribution
  obs <- obs + stats::rnorm(length(obs), 0, 1e-6 * stats::sd(obs))
  eps <- .fcst %>%
    dplyr::select(dplyr::contains("_mbr")) %>%
    dplyr::select_if(~sum(!is.na(.)) > 0)
  nMember = dim(eps)[2]
  nObs <- length(obs)
  alpha <- rep(0, nObs * (nMember + 1))
  beta <- rep(0, nObs * (nMember + 1))
  heaviside0 <- rep(0, nObs)
  heavisideN <- rep(0, nObs)
  dim(alpha) <- c(nObs, nMember + 1)
  dim(beta) <- c(nObs, nMember + 1)
  prev <- sort_members(as.matrix(eps))
  index <- which(obs < prev[, 1])
  beta[index, 1] <- prev[index, 1] - obs[index]
  index <- which(obs > prev[, nMember])
  alpha[index, nMember + 1] <- obs[index] - prev[index, nMember]
  index <- which(obs <= prev[, 1])
  heaviside0[index] <- 1
  index <- which(obs <= prev[, nMember])
  heavisideN[index] <- 1
  for (i in 1:(nMember - 1)) {
    index <- which(obs > prev[, i + 1])
    alpha[index, i + 1] <- prev[index, i + 1] - prev[index,i]
    index <- which(obs < prev[, i])
    beta[index, i + 1] <- prev[index, i + 1] - prev[index,i]
    index <- which((prev[, i + 1] > obs) & (obs > prev[,i]))
    alpha[index, i + 1] <- obs[index] - prev[index, i]
    beta[index, i + 1] <- prev[index, i + 1] - obs[index]
  }
  crps <- verification::crpsFromAlphaBeta(alpha, beta, heaviside0, heavisideN)
  crps <- list(CRPS = crps$CRPS, CRPSpot = crps$CRPSpot, Reli = crps$Reli,
    alpha = alpha, beta = beta, heaviside0 = heaviside0,
    heavisideN = heavisideN)
  crps
}
