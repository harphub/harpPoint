# table.stats() from the verification package. For large data sets some variables
# that were of type integer became too large - here they are of type numeric to
# prevent integer overflow.

harp_table_stats <- function(a, b, c, d, n, show_prog, env, fudge = 0.01, silent = FALSE)
{
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

    if (show_prog) {
      cli::cli_progress_update(.envir = env)
    }

    tibble::as_tibble(list(
      num_cases_for_threshold_total      = a + b + c,
      num_cases_for_threshold_observed   = a + c,
      num_cases_for_threshold_forecast   = a + b,
      hits                               = a,
      false_alarms                       = b,
      misses                             = c,
      correct_rejections                 = d,
      threat_score                       = TS,
      hit_rate                           = POD,
      miss_rate                          = M,
      false_alarm_rate                   = F,
      false_alarm_ratio                  = FAR,
      heidke_skill_score                 = HSS,
      pierce_skill_score                 = PSS,
      kuiper_skill_score                 = KSS,
      percent_correct                    = PC,
      frequency_bias                     = BIAS,
      equitable_threat_score             = ETS,
      odds_ratio                         = theta,
      log_odds_ratio                     = log.theta,
      odds_ratio_skill_score             = ORSS,
      extreme_dependency_score           = eds,
      symmetric_eds                      = seds,
      extreme_dependency_index           = EDI,
      symmetric_edi                      = SEDI,
      threat_score_std_error             = TS.se,
      hit_rate_std_error                 = POD.se,
      false_alarm_rate_std_error         = F.se,
      false_alarm_ratio_std_error        = FAR.se,
      heidke_skill_score_std_error       = HSS.se,
      pierce_skill_score_std_error       = PSS.se,
      percent_correct_std_error          = PC.se,
      equitable_threat_score_std_error   = ETS.se,
      log_odds_ratio_std_error           = LOR.se,
      log_odds_ratio_degrees_of_freedom  = n.h,
      odds_ratio_skill_score_std_error   = ORSS.se,
      extreme_dependency_score_std_error = eds.se,
      symmetric_eds_std_error            = seds.se,
      extreme_dependency_index_std_error = EDI.se,
      symmetric_edi_std_error            = SEDI.se
    ))
}
