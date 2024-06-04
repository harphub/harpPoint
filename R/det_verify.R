#' Compute verification scores for deterministic forecasts.
#'
#' @inheritParams ens_verify
#' @export
#'
det_verify <- function(
  .fcst,
  parameter,
  comparator    = c("ge", "gt", "le", "lt", "eq", "between", "outside"),
  include_low   = TRUE,
  include_high  = TRUE,
  thresholds    = NULL,
  groupings     = "lead_time",
  circle        = NULL,
  hexbin        = TRUE,
  num_bins      = 30,
  show_progress = TRUE,
  ...
) {
  if (missing(parameter)) {
    cli::cli_abort(
      "Argument {.arg parameter} is missing with no default."
    )
  }
  check_circle(circle)
  # Set progress bar to false for batch running
  if (!interactive()) show_progress <- FALSE
  UseMethod("det_verify")
}

#' @export
det_verify.harp_ens_point_df <- function(
  .fcst,
  parameter,
  thresholds    = NULL,
  comparator    = c("ge", "gt", "le", "lt", "eq", "between", "outside"),
  include_low   = TRUE,
  include_high  = TRUE,
  groupings     = "lead_time",
  circle        = NULL,
  hexbin        = TRUE,
  num_bins      = 30,
  show_progress = TRUE,
  fcst_model    = NULL,
  ...
) {

  comparator <- match.arg(comparator)

  if (!is.list(groupings)) {
    groupings <- list(groupings)
  }

  .fcst <- harpCore::as_harp_df(harpCore::pivot_members(.fcst))

  groupings <- purrr::map(groupings, ~union(c("sub_model", "member"), .x))

  det_verify(
    .fcst, {{parameter}}, thresholds, comparator, include_low, include_high,
    groupings, circle, hexbin, num_bins,
    show_progress, fcst_model
  )
}


#' @rdname det_verify
#' @inheritParams ens_verify.harp_ens_point_df
#' @export
det_verify.harp_det_point_df <- function(
  .fcst,
  parameter,
  thresholds    = NULL,
  comparator    = c("ge", "gt", "le", "lt", "eq", "between", "outside"),
  include_low   = TRUE,
  include_high  = TRUE,
  groupings     = "lead_time",
  circle        = NULL,
  hexbin        = TRUE,
  num_bins      = 30,
  show_progress = TRUE,
  fcst_model    = NULL,
  ...
) {

  comparator <- match.arg(comparator)

  if (!is.list(groupings)) {
    groupings <- list(groupings)
  }

  fcst_model <- parse_fcst_model(.fcst, fcst_model)
  .fcst[["fcst_model"]] <- fcst_model

  lead_time_col <- intersect(c("lead_time", "leadtime"), colnames(.fcst))

  groupings <- lapply(
    groupings,
    function(x) gsub("lead_time|leadtime", lead_time_col, x)
  )

  col_names <- colnames(.fcst)
  parameter <- rlang::enquo(parameter)
  chr_param <- rlang::quo_name(parameter)

  if (length(grep(chr_param, col_names)) < 1) {
    stop(paste("No column found for", chr_param), call. = FALSE)
  }

  fcst_col  <- col_names[grep("_det$|^fcst$", col_names)]

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

      .fcst     <- harpCore::pivot_members(.fcst) %>%
        dplyr::rename(forecast_det = .data$fcst)

      groupings <- purrr::map(groupings, union, c("member", "sub_model"))
      fcst_col  <- "forecast_det"

    }

  }

  det_score_function <- function(x) {
    tibble::tibble(
      num_cases = nrow(x),
      num_stations = {
        if (is.element("SID", colnames(x))) {
          length(unique(x[["SID"]]))
        } else {
          1L
        }
      },
      bias      = mean(x[["fcst_bias"]]),
      rmse      = sqrt(mean(x[["fcst_bias"]] ^ 2)),
      mae       = mean(abs(x[["fcst_bias"]])),
      stde      = stats::sd(x[["fcst_bias"]])
    )
  }

  compute_summary_scores <- function(compute_group, fcst_df) {

    local_fcst_col <- intersect(c(fcst_col, "fcst"), colnames(fcst_df))

    # Remove the non-grouping columns and ensure no row duplications
    fcst_df <- distinct_rows(fcst_df, compute_group, local_fcst_col, chr_param)

    fcst_df <- dplyr::mutate(
      fcst_df,
      fcst_bias = bias(.data[[local_fcst_col]], .data[[chr_param]], circle)
    )

    fcst_df <- group_without_threshold(fcst_df, compute_group, nest = TRUE)

    group_vars  <- compute_group[compute_group != "threshold"]
    group_names <- glue::glue_collapse(group_vars, sep = ", ", last = " & ")
    score_text  <- cli::col_blue(glue::glue("Det summary for {group_names}"))

    if (show_progress) {
      pb_name <- score_text
    } else {
      pb_name <- FALSE
      message(score_text, appendLF = FALSE)
      score_text <- ""
    }

    fcst_df <- dplyr::transmute(
      fcst_df,
      dplyr::across(
        dplyr::all_of(colnames(fcst_df)[colnames(fcst_df) %in% group_vars])
      ),
      det_score = purrr::map(
        .data[["grouped_data"]], det_score_function, .progress = pb_name
      )
    ) %>%
      tidyr::unnest(dplyr::all_of("det_score"))

    message(score_text, cli::col_green(cli::symbol[["tick"]]))
    fcst_df
  }

  det_summary_scores <- list()
  det_summary_scores[["basic"]] <- purrr::map(
    groupings, compute_summary_scores, .fcst
  ) %>%
    purrr::list_rbind() %>%
    fill_group_na(groupings) %>%
    dplyr::mutate(fcst_model = fcst_model, .before = dplyr::everything())

  if (hexbin) {
    det_summary_scores[["hexbin"]] <- bin_fcst_obs(
      .fcst, !!parameter, groupings, num_bins, show_progress
    )[["det_summary_scores"]]
  }

  res <- list()

  res[["det_summary_scores"]] <- Reduce(
    function(x, y) suppressMessages(dplyr::inner_join(x, y)),
    det_summary_scores
  )

  if (!is.null(thresholds)) {

    thresholds <- check_thresholds(thresholds, comparator)

    meta_cols <- grep(
      "_mbr[[:digit:]]+", colnames(.fcst), value = TRUE, invert = TRUE
    )

    meta_cols_sym <- rlang::syms(meta_cols)
    thresh_col    <- rlang::sym("threshold")

    .fcst <- det_probabilities(
      .fcst, !! parameter, thresholds, comparator, include_low, include_high
    )

    fcst_thresh <- .fcst %>%
      dplyr::select(!!! meta_cols_sym, dplyr::contains("fcst_prob")) %>%
      tidyr::pivot_longer(
        dplyr::contains("fcst_prob"),
        names_to  = "threshold",
        values_to = "fcst_prob"
      ) %>%
      dplyr::mutate(!!thresh_col := gsub("fcst_prob_", "", !!thresh_col))

    obs_thresh <- .fcst %>%
      dplyr::select(!!! meta_cols_sym, dplyr::contains("obs_prob")) %>%
      tidyr::pivot_longer(
        dplyr::contains("obs_prob"),
        names_to  = "threshold",
        values_to = "obs_prob"
      ) %>%
      dplyr::mutate(!!thresh_col := gsub("obs_prob_", "", !!thresh_col))

    .fcst <- dplyr::mutate(fcst_thresh, obs_prob = obs_thresh[["obs_prob"]])

    verif_func <- function(x) {
      res <- harp_verify(
        x[["obs_prob"]], x[["fcst_prob"]],
        frcst.type = "binary", obs.type = "binary"
      )
      sweep_det_thresh(res)
    }

    groupings <- purrr::map(groupings, union, "threshold")

    compute_threshold_scores <- function(compute_group, fcst_df) {

      fcst_obs_col <- c("fcst_prob", "obs_prob")

      # Remove the non-grouping columns and ensure no row duplications
      fcst_df <- distinct_rows(fcst_df, compute_group, fcst_obs_col, chr_param)

      fcst_df <- dplyr::group_nest(
        fcst_df, dplyr::across(dplyr::all_of(compute_group)),
        .key = "grouped_data"
      )

      group_names <- glue::glue_collapse(
        compute_group, sep = ", ", last = " & "
      )

      score_text <- cli::col_blue(
        glue::glue("Det categorical for {group_names}")
      )

      if (show_progress) {
        pb_name <- score_text
      } else {
        pb_name <- FALSE
        message(score_text, appendLF = FALSE)
        score_text <- ""
      }

      fcst_df <- dplyr::transmute(
        fcst_df,
        dplyr::across(dplyr::all_of(compute_group)),
        num_stations = {
          if (is.element("SID", compute_group)) {
            1L
          } else {
            purrr::map_int(.data[["grouped_data"]], ~length(unique(.x[["SID"]])))
          }
        },
        verif = purrr::map(
          .data[["grouped_data"]],
          verif_func,
          .progress = pb_name
        )
      )

      message(score_text, cli::col_green(cli::symbol[["tick"]]))
      fcst_df

    }

    res[["det_threshold_scores"]] <- purrr::map(
      groupings, compute_threshold_scores, .fcst
    ) %>%
      purrr::list_rbind() %>%
      fill_group_na(groupings) %>%
      tidyr::unnest(dplyr::all_of("verif")) %>%
      dplyr::mutate(fcst_model = fcst_model, .before = dplyr::everything())

  } else {

    res[["det_threshold_scores"]] <- tibble::tibble()

  }

  structure(
    add_attributes(
      res[which(vapply(res, nrow, numeric(1)) > 0)],
      harpCore::unique_fcst_dttm(.fcst),
      !!parameter,
      harpCore::unique_stations(.fcst),
      groupings
    ),
    class = "harp_verif"
  )

}

#' @export
det_verify.harp_list <- function(
  .fcst,
  parameter,
  thresholds    = NULL,
  comparator    = c("ge", "gt", "le", "lt", "eq", "between", "outside"),
  include_low   = TRUE,
  include_high  = TRUE,
  groupings     = "lead_time",
  circle        = NULL,
  hexbin        = TRUE,
  num_bins      = 30,
  show_progress = TRUE
) {

  parameter   <- rlang::ensym(parameter)
#  if (!inherits(try(rlang::eval_tidy(parameter), silent = TRUE), "try-error")) {
#    if (is.character(rlang::eval_tidy(parameter))) {
#      parameter <- rlang::eval_tidy(parameter)
#      parameter <- rlang::ensym(parameter)
#    }
#  }

  comparator <- match.arg(comparator)

  list_to_harp_verif(
    purrr::imap(
      .fcst,
      ~det_verify(
        .x, {{parameter}}, thresholds, comparator, include_low, include_high,
        groupings, circle, hexbin, num_bins,
        show_progress, fcst_model = .y
      )
    )
  )

}

sweep_det_thresh <- function(x) {

  sweep_cont_tab <- function(.cont_tab) {
    if (length(.cont_tab) != 4) {
      return(tibble::tibble(
        correct_rejection = NA,
        miss = NA,
        false_alarm = NA,
        hit = NA
      ))
    }
    .cont_tab <- as.data.frame(.cont_tab)
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

  tibble::tibble(
    num_cases_for_threshold_total      = sum(x$obs | x$pred),
    num_cases_for_threshold_observed   = sum(x$obs),
    num_cases_for_threshold_forecast   = sum(x$pred),
    cont_tab                           = list(sweep_cont_tab(x$tab)),
    threat_score                       = x[["TS"]],
    hit_rate                           = x[["POD"]],
    miss_rate                          = x[["M"]],
    false_alarm_rate                   = x[["F"]],
    false_alarm_ratio                  = x[["FAR"]],
    heidke_skill_score                 = x[["HSS"]],
    pierce_skill_score                 = x[["PSS"]],
    kuiper_skill_score                 = x[["KSS"]],
    percent_correct                    = x[["PC"]],
    frequency_bias                     = x[["BIAS"]],
    equitable_threat_score             = x[["ETS"]],
    odds_ratio                         = x[["theta"]],
    log_odds_ratio                     = x[["log.theta"]],
    odds_ratio_skill_score             = x[["orss"]],
    extreme_dependency_score           = x[["eds"]],
    symmetric_eds                      = x[["seds"]],
    extreme_dependency_index           = x[["EDI"]],
    symmetric_edi                      = x[["SEDI"]],
    threat_score_std_error             = x[["TS.se"]],
    hit_rate_std_error                 = x[["POD.se"]],
    false_alarm_rate_std_error         = x[["F.se"]],
    false_alarm_ratio_std_error        = x[["FAR.se"]],
    heidke_skill_score_std_error       = x[["HSS.se"]],
    pierce_skill_score_std_error       = x[["PSS.se"]],
    percent_correct_std_error          = x[["PC.se"]],
    equitable_threat_score_std_error   = x[["ETS.se"]],
    log_odds_ratio_std_error           = x[["LOR.se"]],
    log_odds_ratio_degrees_of_freedom  = x[["n.h"]],
    odds_ratio_skill_score_std_error   = x[["orss.se"]],
    extreme_dependency_score_std_error = x[["eds.se"]],
    symmetric_eds_std_error            = x[["seds.se"]],
    extreme_dependency_index_std_error = x[["EDI.se"]],
    symmetric_edi_std_error            = x[["SEDI.se"]]
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

check_circle <- function(x) {
  if (is.null(x)) return()
  if (!x %in% c(360, 2 * pi)) {
    cli::cli_warn(c(
      "{.arg circle} has a value not equal to 360 or 2 * pi.",
      "i" = "You have set {.arg circle} = {x}.",
      "i" = "Are you sure this is the value you want?"
    ))
  }
}

distinct_rows <- function(.df, grps, fc_cols, obs_col) {
  std_cols <- c("fcst_dttm", "lead_time", "SID")
  .df <- dplyr::select(
    .df,
    dplyr::any_of(c(std_cols, grps, fc_cols, obs_col))
  )
  # Do not include list columns - this is probably only an issue for CRPS
  # where there is some randomness at a very high level of precision
  dplyr::distinct(.df, dplyr::pick(-dplyr::where(is.list)), .keep_all = TRUE)
}
