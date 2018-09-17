# verify() from the verification package. Calls harp_table_stats instead of table.stats
# to prevent integer overflow.

harp_verify <- function (obs, pred = NULL, p = NULL, baseline = NULL, frcst.type = "prob",
    obs.type = "binary", thresholds = seq(0, 1, 0.1), show = TRUE,
    bins = TRUE, fudge = 0.01, ...)
{
    if (min(diff(thresholds)) < 0)
        stop("Thresholds must be listed in ascending order")
    if (length(obs) > 4 && !is.matrix(obs)) {
        id <- is.finite(obs) & is.finite(pred)
        obs <- obs[id]
        pred <- pred[id]
    }
    if (frcst.type == "binary" && obs.type == "binary" && is.null(pred)) {
        A <- table.stats(obs, fudge = fudge)
        class(A) <- c("verify", "bin.bin")
    }
    else if (frcst.type == "binary" & obs.type == "binary") {
        if (length(unique(obs)) > 2 | length(unique(pred)) >
            2) {
            warning("Prediction or observation may not be binary \n")
        }
        A <- harp_table_stats(obs, pred, fudge = fudge)
        class(A) <- c("verify", "bin.bin")
    }
    else if (frcst.type == "prob" & obs.type == "binary") {
        if (show) {
            cat("If baseline is not included, baseline values  will be calculated from the  sample obs. \n")
        }
        A <- brier(obs, pred, baseline, thresholds, bins = bins)
        class(A) <- c("verify", "prob.bin")
    }
    else if (frcst.type == "quantile" & obs.type == "cont") {
        if (is.null(p))
            warning("verify: Missing p. \n")
        A <- quantileScore(obs = obs, pred = pred, p = p, breaks = thresholds)
        class(A) <- c("verify", "quantile")
    }
    else if (frcst.type == "norm.dist" & obs.type == "cont") {
        A <- crps(obs, pred)
        class(A) <- class(A) <- c("verify", "norm.dist.cont")
    }
    else if (frcst.type == "cont" & obs.type == "cont") {
        A <- c()
        if (is.null(baseline)) {
            baseline <- mean(obs)
            A$baseline.tf <- FALSE
        }
        else {
            A$baseline.tf <- TRUE
        }
        A$MAE <- mean(abs(pred - obs))
        A$MSE <- mean((pred - obs)^2)
        A$ME <- mean((pred - obs))
        A$MSE.baseline <- mean((mean(baseline) - obs)^2)
        A$MSE.pers <- mean((obs[-length(obs)] - obs[-1])^2)
        A$SS.baseline <- 1 - (A$MSE/A$MSE.baseline)
        class(A) <- c("verify", "cont.cont")
    }
    else if (frcst.type == "cat" & obs.type == "cat") {
        if (is.matrix(obs) & is.null(pred)) {
            print("Assuming data is summarized in a contingency table./n  ")
            print("Columns summarize observed values.  Rows summarize predicted values /n")
            DAT <- obs
        }
        else {
            a <- sort(unique(c(obs, pred)))
            obs.a <- c(a, obs)
            pred.a <- c(a, pred)
            DAT <- table(pred.a, obs.a)
            diag(DAT) <- diag(DAT) - 1
        }
        A <- multi.cont(DAT)
        class(A) <- c("verify", "cat.cat")
    }
    else cat("This combination of predictions \n and observations is not \n currently supported. \n")
    A$obs <- obs
    A$pred <- pred
    A$baseline <- baseline
    return(A)
}
