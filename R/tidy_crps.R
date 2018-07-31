tidy_crps <- function (FCST) {
#
# function to compute the CRPS with tidy data
#
	obs <- FCST %>% dplyr::pull(obs)
	eps <- FCST %>%
		dplyr::select(dplyr::contains("mbr")) %>%
		dplyr::select_if(~sum(!is.na(.)) > 0)
	nMember = dim(eps)[2]
	nObs <- length(obs)
	alpha <- rep(0, nObs * (nMember + 1))
	beta <- rep(0, nObs * (nMember + 1))
	heaviside0 <- rep(0, nObs)
	heavisideN <- rep(0, nObs)
	dim(alpha) <- c(nObs, nMember + 1)
	dim(beta) <- c(nObs, nMember + 1)
	prev <- sort2d(eps)
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
