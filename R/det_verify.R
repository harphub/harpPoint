#' Title
#'
#' @param .fcst
#' @param parameter
#' @param thresholds
#' @param groupings
#'
#' @return
#' @export
#'
#' @examples
det_verify <- function(.fcst, parameter, thresholds = NULL, groupings = "leadtime") {
  UseMethod("det_verify")
}

#' @export
det_verify.default <- function(.fcst, parameter, thresholds = NULL, groupings = "leadtime") {

  col_names <- colnames(.fcst)
  parameter <- rlang::enquo(parameter)
  chr_param <- rlang::quo_name(parameter)

  if (length(grep(chr_param, col_names)) < 1) {
    stop(paste("No column found for", chr_param), call. = FALSE)
  }

  fcst_col  <- col_names[grep("_det", col_names)]

  if (length(fcst_col) > 1) {

    stop("Cannot have more than 1 deterministic model in a table.", call. = FALSE)

  } else if (length(fcst_col) < 1) {

    fcst_col <- col_names[grep("_mbr", col_names)]
    if (length(fcst_col) < 1) {
      stop(
        "Cannot find any forecast data. Forecast column names must contain '_det' or '_mbr'",
        call. = FALSE
      )

    } else {

      message("This looks like an ensemble - will compute deterministic scores for each member.")

      attr(.fcst, "dataframe_format") <- "long"

      .fcst     <- gather_members(.fcst)
      groupings <- rlang::syms(c(groupings, "member"))
      fcst_col  <- "forecast"

    }

  } else {

    groupings <- rlang::syms(groupings)

  }

  det_summary_scores <- .fcst %>%
    dplyr::group_by(!!! groupings) %>%
    tidyr::nest(.key = "grouped_fcst") %>%
    dplyr::transmute(
      !!! groupings,
      num_cases = purrr::map_int(.data$grouped_fcst, nrow),
      bias      = purrr::map_dbl(.data$grouped_fcst, ~ mean(.x[[fcst_col]] - .x[[chr_param]])),
      rmse      = purrr::map_dbl(.data$grouped_fcst, ~ sqrt(mean((.x[[fcst_col]] - .x[[chr_param]]) ^ 2))),
      mae       = purrr::map_dbl(.data$grouped_fcst, ~ mean(abs(.x[[fcst_col]] - .x[[chr_param]]))),
      stde      = purrr::map_dbl(.data$grouped_fcst, ~ sd(.x[[fcst_col]] - .x[[chr_param]]))
    )

  if (is.numeric(thresholds)) {

    join_cols  <- c("SID", "fcdate", "leadtime", "validdate", "threshold")
    meta_cols  <- rlang::quos(c(SID, fcdate, leadtime, validdate))
    thresh_col <- rlang::sym("threshold")

    .fcst <- det_probabilities(.fcst, !! parameter, thresholds)

    fcst_thresh <- .fcst %>%
      dplyr::select(!!! meta_cols, dplyr::contains("fcst_prob")) %>%
      tidyr::gather(dplyr::contains("fcst_prob"), key = "threshold", value = "fcst_prob") %>%
      dplyr::mutate(!! thresh_col := readr::parse_number(!! thresh_col))

    obs_thresh <- .fcst %>%
      dplyr::select(!!! meta_cols, dplyr::contains("obs_prob")) %>%
      tidyr::gather(dplyr::contains("obs_prob"), key = "threshold", value = "obs_prob") %>%
      dplyr::mutate(!! thresh_col := readr::parse_number(!! thresh_col))

    .fcst <- dplyr::inner_join(
      fcst_thresh,
      obs_thresh,
      by = join_cols
    )

    det_threshold_scores <- .fcst %>%
      dplyr::group_by(!!! groupings, !! thresh_col) %>%
      tidyr::nest(.key = "grouped_fcst") %>%
      dplyr::transmute(
        !!! groupings,
        !! thresh_col,
        verif = purrr::map(
          .data$grouped_fcst,
          ~ harp_verify(.x$obs_prob, .x$fcst_prob, frcst.type = "binary", obs.type = "binary")
        )
      ) %>%
      sweep_det_thresh(groupings, thresh_col)

  } else {

    det_threshold_scores <- empty_det_threshold_scores(.fcst, groupings)

  }

  list(det_summary_scores = det_summary_scores, det_threshold_scores = det_threshold_scores)

}

#' @export
det_verify.harp_fcst <- function(.fcst, parameter, thresholds = NULL, groupings = "leadtime") {
  parameter <- rlang::enquo(parameter)
  list_result <- purrr::map(.fcst, det_verify, !! parameter, thresholds, groupings)
  list(
    det_summary_scores   = dplyr::bind_rows(
      purrr::map(list_result, "det_summary_scores"),
      .id = "mname"
    ),
    det_threshold_scores = dplyr::bind_rows(
      purrr::map(list_result, "det_threshold_scores"),
      .id = "mname"
    )
  )
}

sweep_det_thresh <- function(det_threshold_df, groupings, thresh_col) {

  sweep_cont_tab <- function(.cont_tab) {
    .cont_tab %>%
      tibble::as_tibble() %>%
      dplyr::rename(observed = .data$Var1, forecasted = .data$Var2, count = .data$n) %>%
      dplyr::mutate(type = c("correct_rejection", "false_alarm", "miss", "hit"))
  }

  det_threshold_df %>%
    dplyr::transmute(
      !!! groupings,
      !! thresh_col,
      oberved_cases                      = purrr::map_dbl(.data$verif, ~ sum(.x$obs)),
      forecasted_cases                   = purrr::map_dbl(.data$verif, ~ sum(.x$pred)),
      cont_tab                           = purrr::map(.data$verif, ~ sweep_cont_tab(.x$tab)),
      threat_score                       = purrr::map_dbl(.data$verif, "TS"),
      hit_rate                           = purrr::map_dbl(.data$verif, "POD"),
      miss_rate                          = purrr::map_dbl(.data$verif, "M"),
      false_alarm_rate                   = purrr::map_dbl(.data$verif, "F"),
      false_alarm_ratio                  = purrr::map_dbl(.data$verif, "FAR"),
      heidke_skill_score                 = purrr::map_dbl(.data$verif, "HSS"),
      pierce_skill_score                 = purrr::map_dbl(.data$verif, "PSS"),
      kuiper_skill_score                 = purrr::map_dbl(.data$verif, "KSS"),
      percent_correct                    = purrr::map_dbl(.data$verif, "PC"),
      frequency_bias                     = purrr::map_dbl(.data$verif, "BIAS"),
      equitable_threat_score             = purrr::map_dbl(.data$verif, "ETS"),
      odds_ratio                         = purrr::map_dbl(.data$verif, "theta"),
      log_odds_ratio                     = purrr::map_dbl(.data$verif, "log.theta"),
      odds_ratio_skill_score             = purrr::map_dbl(.data$verif, "orss"),
      extreme_dependency_score           = purrr::map_dbl(.data$verif, "eds"),
      symmetric_eds                      = purrr::map_dbl(.data$verif, "seds"),
      extreme_dependency_index           = purrr::map_dbl(.data$verif, "EDI"),
      symmetric_edi                      = purrr::map_dbl(.data$verif, "SEDI"),
      threat_score_std_error             = purrr::map_dbl(.data$verif, "TS.se"),
      hit_rate_std_error                 = purrr::map_dbl(.data$verif, "POD.se"),
      false_alarm_rate_std_error         = purrr::map_dbl(.data$verif, "F.se"),
      false_alarm_ratio_std_error        = purrr::map_dbl(.data$verif, "FAR.se"),
      heidke_skill_score_std_error       = purrr::map_dbl(.data$verif, "HSS.se"),
      pierce_skill_score_std_error       = purrr::map_dbl(.data$verif, "PSS.se"),
      percent_correct_std_error          = purrr::map_dbl(.data$verif, "PC.se"),
      equitable_threat_score_std_error   = purrr::map_dbl(.data$verif, "ETS.se"),
      log_odds_ratio_std_error           = purrr::map_dbl(.data$verif, "LOR.se"),
      log_odds_ratio_degrees_of_freedom  = purrr::map_dbl(.data$verif, "n.h"),
      odds_ratio_skill_score_std_error   = purrr::map_dbl(.data$verif, "orss.se"),
      extreme_dependency_score_std_error = purrr::map_dbl(.data$verif, "eds.se"),
      symmetric_eds_std_error            = purrr::map_dbl(.data$verif, "seds.se"),
      extreme_dependency_index_std_error = purrr::map_dbl(.data$verif, "EDI.se"),
      symmetric_edi_std_error            = purrr::map_dbl(.data$verif, "SEDI.se")
    )

}

empty_det_threshold_scores <- function(fcst_df, groupings) {
  groupings <- rlang::syms(groupings)
  fcst_df %>%
    dplyr::transmute(!!! groupings) %>%
    dplyr::group_by(!!! groupings) %>%
    tidyr::nest() %>%
    dplyr::select(-.data$data) %>%
    dplyr::mutate(
      threshold                          = NA_real_,
      oberved_cases                      = NA_real_,
      forecasted_cases                   = NA_real_,
      cont_tab                           = list(
        tibble::tibble(
          observed   = character(),
          forecasted = character(),
          count      = integer(),
          type       = character()
        )
      ),
      threat_score                       = NA_real_,
      hit_rate                           = NA_real_,
      miss_rate                          = NA_real_,
      false_alarm_rate                   = NA_real_,
      false_alarm_ratio                  = NA_real_,
      heidke_skill_score                 = NA_real_,
      pierce_skill_score                 = NA_real_,
      kuiper_skill_score                 = NA_real_,
      percent_correct                    = NA_real_,
      frequency_bias                     = NA_real_,
      equitable_threat_score             = NA_real_,
      odds_ratio                         = NA_real_,
      log_odds_ratio                     = NA_real_,
      odds_ratio_skill_score             = NA_real_,
      extreme_dependency_score           = NA_real_,
      symmetric_eds                      = NA_real_,
      extreme_dependency_index           = NA_real_,
      symmetric_edi                      = NA_real_,
      threat_score_std_error             = NA_real_,
      hit_rate_std_error                 = NA_real_,
      false_alarm_rate_std_error         = NA_real_,
      false_alarm_ratio_std_error        = NA_real_,
      heidke_skill_score_std_error       = NA_real_,
      pierce_skill_score_std_error       = NA_real_,
      percent_correct_std_error          = NA_real_,
      equitable_threat_score_std_error   = NA_real_,
      log_odds_ratio_std_error           = NA_real_,
      log_odds_ratio_degrees_of_freedom  = NA_real_,
      odds_ratio_skill_score_std_error   = NA_real_,
      extreme_dependency_score_std_error = NA_real_,
      symmetric_eds_std_error            = NA_real_,
      extreme_dependency_index_std_error = NA_real_,
      symmetric_edi_std_error            = NA_real_
    )

}
