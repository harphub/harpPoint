# table.stats() from the verification package. For large data sets some variables
# that were of type integer became too large - here they are of type numeric to
# prevent integer overflow.

harp_table_stats <- function(a, b, c, d, n, fudge = 0.01, silent = FALSE)
{
    tab.out <- as.table(
      matrix(c(d, c, b, a), ncol = 2, dimnames = list(c(0, 1), c(0, 1)))
    )
    if (n == 0)
        n <- fudge
    s <- (a + c)/n
    TS <- a/(a + b + c + fudge)
    POD <- H <- a/(a + c + fudge)
    F <- b/(b + d + fudge)
    TS.se <- sqrt((TS^2) * ((1 - H)/(a + fudge) + b * (1 - F)/((a +
        b + c)^2 + fudge)))
    SH2 <- H * (1 - H)/(a + c + fudge)
    SF2 <- F * (1 - F)/(b + d + fudge)
    POD.se <- sqrt(SH2)
    F.se <- sqrt(SF2)
    M <- c/(a + c + fudge)
    FAR <- b/(a + b + fudge)
    FAR.se <- sqrt((FAR^4) * ((1 - H)/(a + fudge) + (1 - F)/(b +
        fudge)) * (a^2)/(b^2 + fudge))
    HSS <- 2 * (a * d - b * c)/(1 * (a + c) * (c + d) + 1 * (a +
        b) * (b + d) + fudge)
    SHSS2 <- SF2 * (HSS^2) * (1/(H - F + fudge) + (1 - s) * (1 -
        2 * s))^2 + SH2 * (HSS^2) * (1/(H - F + fudge) - s *
        (1 - 2 * s))^2
    HSS.se = sqrt(SHSS2)
    PSS <- 1 - M - F
    PSS.se <- sqrt(SH2 + SF2)
    KSS <- (a * d - b * c)/((a + c) * (b + d) + fudge)
    PC <- (a + d)/(a + b + c + d + fudge)
    PC.se <- sqrt(s * H * (1 - H)/n + (1 - s) * F * (1 - F)/n)
    if (a + c == 0)
        BIAS <- (a + b)/fudge
    else BIAS <- (a + b)/(a + c)
    if (b * c == 0)
        OR <- a * d/fudge
    else OR <- a * d/(b * c)
    if (a * b + b * c == 0)
        ORSS <- (a * d - b * c)/fudge
    else ORSS <- (a * d - b * c)/(a * d + b * c)
    HITSrandom <- 1 * (a + c) * (a + b)/n
    p <- (a + c)/n
    if (a + b + c - HITSrandom == 0)
        ETS <- (a - HITSrandom)/fudge
    else ETS <- (a - HITSrandom)/(a + b + c - HITSrandom)
    if (2 - HSS == 0)
        ETS.se <- sqrt(4 * SHSS2/fudge)
    else ETS.se <- sqrt(4 * SHSS2/((2 - HSS)^4))
    if (b * c == 0)
        theta <- a * d/fudge
    else theta <- (a * d)/(b * c)
    log.theta <- log(a) + log(d) - log(b) - log(c)
    if (a == 0)
        a.z <- fudge
    else a.z <- a
    if (b == 0)
        b.z <- fudge
    else b.z <- b
    if (c == 0)
        c.z <- fudge
    else c.z <- c
    if (d == 0)
        d.z <- fudge
    else d.z <- d
    if (1/a.z + 1/b.z + 1/c.z + 1/d.z == 0)
        n.h <- 1/fudge
    else n.h <- 1/(1/a.z + 1/b.z + 1/c.z + 1/d.z)
    if (theta + 1 == 0)
        yules.q <- (theta - 1)/fudge
    else yules.q <- (theta - 1)/(theta + 1)
    if (n.h == 0)
        SLOR2 <- 1/fudge
    else SLOR2 <- 1/n.h
    LOR.se <- sqrt(SLOR2)
    if (OR + 1 == 0)
        ORSS.se <- sqrt(SLOR2 * 4 * OR^2/fudge)
    else ORSS.se <- sqrt(SLOR2 * 4 * OR^2/((OR + 1)^4))
    if (log(a/n) == 0) {
        eds <- 2 * log((a + c)/n)/fudge - 1
        seds <- (log((a + b)/n) + log((a + c)/n))/fudge - 1
    }
    else {
        eds <- 2 * log((a + c)/n)/log(a/n) - 1
        seds <- (log((a + b)/n) + log((a + c)/n))/log(a/n) -
            1
    }
    eds.se <- 2 * abs(log(p))/(H * (log(p) + log(H))^2) * sqrt(H *
        (1 - H)/(p * n))
    seds.se <- sqrt(H * (1 - H)/(n * p)) * (-log(BIAS * p^2)/(H *
        log(H * p)^2))
    if (log(F) + log(H) == 0)
        EDI <- (log(F) - log(H))/fudge
    else EDI <- (log(F) - log(H))/(log(F) + log(H))
    EDI.se <- 2 * abs(log(F) + H/(1 - H) * log(H))/(H * (log(F) +
        log(H))^2) * sqrt(H * (1 - H)/(p * n))
    SEDI <- (log(F) - log(H) - log(1 - F) + log(1 - H))/(log(F) +
        log(H) + log(1 - F) + log(1 - H))
    SEDI.se <- 2 * abs(((1 - H) * (1 - F) + H * F)/((1 - H) *
        (1 - F)) * log(F * (1 - H)) + 2 * H/(1 - H) * log(H *
        (1 - F)))/(H * (log(F * (1 - H)) + log(H * (1 - F)))^2) *
        sqrt(H * (1 - H)/(p * n))
    return(list(tab = tab.out, TS = TS, TS.se = TS.se, POD = POD,
        POD.se = POD.se, M = M, F = F, F.se = F.se, FAR = FAR,
        FAR.se = FAR.se, HSS = HSS, HSS.se = HSS.se, PSS = PSS,
        PSS.se = PSS.se, KSS = KSS, PC = PC, PC.se = PC.se, BIAS = BIAS,
        ETS = ETS, ETS.se = ETS.se, theta = theta, log.theta = log.theta,
        LOR.se = LOR.se, n.h = n.h, orss = yules.q, orss.se = ORSS.se,
        eds = eds, eds.se = eds.se, seds = seds, seds.se = seds.se,
        EDI = EDI, EDI.se = EDI.se, SEDI = SEDI, SEDI.se = SEDI.se))
}
