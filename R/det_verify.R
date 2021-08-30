#' Compute verification scores for deterministic forecasts.
#'
#' @param .fcst A \code{harp_fcst} object with tables that have a column for
#'   observations, or a single forecast table.
#' @param parameter The name of the column for the observed data.
#' @param thresholds A numeric vector of thresholds for which to compute the
#'   threshold based scores. Set to NULL (the default) to only compute summary
#'   scores.
#' @param groupings The groups for which to compute the scores. See
#' \link[dplyr]{group_by} for more information of how grouping works.
#' @param show_progress Logical - whether to show a progress bar. The default is
#'   FALSE.
#' @return A list containting two data frames: \code{det_summary_scores} and
#'   \code{det_threshold_scores}.
#' @export
#'
#' @examples
det_verify <- function(.fcst, parameter, thresholds = NULL, groupings = "leadtime", show_progress = TRUE) {
  UseMethod("det_verify")
}

#' @export
det_verify.default <- function(.fcst, parameter, thresholds = NULL, groupings = "leadtime", show_progress = TRUE) {

  if (!is.list(groupings)) {
    groupings <- list(groupings)
  }

  col_names <- colnames(.fcst)
  parameter <- rlang::enquo(parameter)
  chr_param <- rlang::quo_name(parameter)

  if (length(grep(chr_param, col_names)) < 1) {
    stop(paste("No column found for", chr_param), call. = FALSE)
  }

  fcst_col  <- col_names[grep("_det$", col_names)]

  if (length(fcst_col) > 1) {

    stop("Cannot have more than 1 deterministic model in a table.", call. = FALSE)

  } else if (length(fcst_col) < 1) {

    fcst_col <- col_names[grep("_mbr\\d{3}", col_names)]
    if (length(fcst_col) < 1) {
      stop(
        "Cannot find any forecast data. Forecast column names must contain '_det' or '_mbr'",
        call. = FALSE
      )

    } else {

      message("This looks like an ensemble - will compute deterministic scores for each member.")

      attr(.fcst, "dataframe_format") <- "wide"

      .fcst     <- gather_members(.fcst) %>%
        dplyr::rename(forecast_det = .data$forecast)

      groupings <- purrr::map(groupings, union, c("member", "sub_model"))
      fcst_col  <- "forecast_det"

    }

  }

  if (show_progress) {
    progress_total <- sum(
      sapply(
        groupings,
        function(x) length(dplyr::group_rows(dplyr::group_by(.fcst, !!! rlang::syms(intersect(x, names(.fcst))))))
      )
    )
    det_progress <- progress::progress_bar$new(format = "  Deterministic [:bar] :percent eta: :eta", total = progress_total)
  }

  sd_function <- function(x, prog_bar) {
    res <- stats::sd(x)
    if (prog_bar) {
      det_progress$tick()
    }
    res
  }


  compute_summary_scores <- function(compute_group, fcst_df) {

    fcst_df <- group_without_threshold(fcst_df, compute_group)

    fcst_df %>%
      dplyr::mutate(fcst_minus_obs = .data[[fcst_col]] - .data[[chr_param]]) %>%
      dplyr::summarise(
        num_cases = dplyr::n(),
        bias      = mean(.data[["fcst_minus_obs"]]),
        rmse      = sqrt(mean(.data[["fcst_minus_obs"]] ^ 2)),
        mae       = mean(abs(.data[["fcst_minus_obs"]])),
        stde      = sd_function(.data[["fcst_minus_obs"]], prog_bar = show_progress)
      )

  }

  det_summary_scores <- purrr::map_dfr(groupings, compute_summary_scores, .fcst) %>%
    fill_group_na(groupings)

  if (is.numeric(thresholds)) {

    meta_cols     <- grep("_mbr[[:digit:]]+", colnames(.fcst), value = TRUE, invert = TRUE)
    meta_cols_sym <- rlang::syms(meta_cols)
    thresh_col    <- rlang::sym("threshold")

    .fcst <- det_probabilities(.fcst, !! parameter, thresholds)

    fcst_thresh <- .fcst %>%
      dplyr::select(!!! meta_cols_sym, dplyr::contains("fcst_prob")) %>%
      tidyr::gather(dplyr::contains("fcst_prob"), key = "threshold", value = "fcst_prob") %>%
      dplyr::mutate(!! thresh_col := readr::parse_number(!! thresh_col))

    obs_thresh <- .fcst %>%
      dplyr::select(!!! meta_cols_sym, dplyr::contains("obs_prob")) %>%
      tidyr::gather(dplyr::contains("obs_prob"), key = "threshold", value = "obs_prob") %>%
      dplyr::mutate(!! thresh_col := readr::parse_number(!! thresh_col))

    .fcst <- dplyr::mutate(fcst_thresh, obs_prob = obs_thresh[["obs_prob"]])

    verif_func <- function(.df, show_progress) {
      res <- harp_verify(.df$obs_prob, .df$fcst_prob, frcst.type = "binary", obs.type = "binary")
      if (show_progress) {
        thresh_progress$tick()
      }
      res
    }

    groupings <- purrr::map(groupings, union, "threshold")

    if (show_progress) {
      progress_total <- sum(
        sapply(
          groupings,
          function(x) length(dplyr::group_rows(dplyr::group_by(.fcst, !!! rlang::syms(intersect(x, names(.fcst))))))
        )
      )
      thresh_progress <- progress::progress_bar$new(format = "  Deterministic thresholds [:bar] :percent eta: :eta", total = progress_total)
    }

    compute_threshold_scores <- function(compute_group, fcst_df) {
      compute_group_sym <- rlang::syms(compute_group)
      if (harpIO:::tidyr_new_interface()) {
        fcst_df <- tidyr::nest(fcst_df, grouped_fcst = -tidyr::one_of(compute_group))
      } else {
        fcst_df <- fcst_df %>%
          dplyr::group_by(!!! compute_group_sym) %>%
          tidyr::nest(.key = "grouped_fcst")
      }
      fcst_df %>%
        dplyr::transmute(
          !!! compute_group_sym,
          verif = purrr::map(
            .data$grouped_fcst,
            verif_func,
            show_progress
          )
        ) %>%
        sweep_det_thresh(compute_group_sym, thresh_col)
    }

    det_threshold_scores <- purrr::map_dfr(groupings, compute_threshold_scores, .fcst) %>%
      fill_group_na(groupings)

  } else {

    det_threshold_scores <- NULL

  }

  list(det_summary_scores = det_summary_scores, det_threshold_scores = det_threshold_scores)

}

#' @export
det_verify.harp_fcst <- function(.fcst, parameter, thresholds = NULL, groupings = "leadtime", show_progress = TRUE) {

  parameter   <- rlang::enquo(parameter)
  if (!inherits(try(rlang::eval_tidy(parameter), silent = TRUE), "try-error")) {
    if (is.character(rlang::eval_tidy(parameter))) {
      parameter <- rlang::eval_tidy(parameter)
      parameter <- rlang::ensym(parameter)
    }
  }

  list_result <- purrr::map(.fcst, det_verify, !! parameter, thresholds, groupings, show_progress)
  list(
    det_summary_scores   = dplyr::bind_rows(
      purrr::map(list_result, "det_summary_scores"),
      .id = "mname"
    ),
    det_threshold_scores = dplyr::bind_rows(
      purrr::map(list_result, "det_threshold_scores"),
      .id = "mname"
    )
  ) %>% add_attributes(.fcst, !! parameter)
}

sweep_det_thresh <- function(det_threshold_df, groupings, thresh_col) {

  sweep_cont_tab <- function(.cont_tab) {
    .cont_tab           <- as.data.frame(.cont_tab)
    colnames(.cont_tab) <- c("observed", "forecasted", "count")
    .cont_tab %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        type = dplyr::case_when(
          .data$observed == 0 & .data$forecasted == 0 ~ "correct_rejection",
          .data$observed == 1 & .data$forecasted == 0 ~ "miss",
          .data$observed == 0 & .data$forecasted == 1 ~ "false_alarm",
          .data$observed == 1 & .data$forecasted == 1 ~ "hit"
        )
      ) %>% dplyr::select(.data$type, .data$count)
  }

  det_threshold_df %>%
    dplyr::transmute(
      !!! groupings,
      !! thresh_col,
      num_cases_for_threshold_total      = purrr::map_dbl(.data$verif, ~ sum(.x$obs | .x$pred)),
      num_cases_for_threshold_observed   = purrr::map_dbl(.data$verif, ~ sum(.x$obs)),
      num_cases_for_threshold_forecast   = purrr::map_dbl(.data$verif, ~ sum(.x$pred)),
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
  groupings <- rlang::syms(unique(unlist(groupings)))
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
          type       = character(),
          count      = integer()
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
