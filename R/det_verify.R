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
  summary       = TRUE,
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
  summary       = TRUE,
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
    groupings, circle, summary, hexbin, num_bins,
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
  summary       = TRUE,
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

      .fcst <- harpCore::pivot_members(.fcst) %>%
        dplyr::rename(forecast_det = .data$fcst)

      groupings <- purrr::map(groupings, union, c("member", "sub_model"))
      fcst_col  <- "forecast_det"

    }

  }

  needed_cols <- unique(c(
    "fcst_model", "fcst_dttm", "lead_time", "SID",
    unique(unlist(groupings)), fcst_col, chr_param
  ))
  .fcst <- dplyr::select(.fcst, dplyr::any_of(needed_cols))

  res <- list()

  res[["det_summary_scores"]] <- tibble::tibble()

  det_summary_scores <- list()

  if (summary) {
    det_summary_scores[["basic"]] <- compute_det_score(
      groupings, .fcst, fcst_col, chr_param, fcst_model, circle, num_bins,
      "summary", show_progress
    )
  }

  if (hexbin) {
    det_summary_scores[["hexbin"]] <- compute_det_score(
      groupings, .fcst, fcst_col, chr_param, fcst_model, circle, num_bins,
      "hexbin", show_progress
    )
  }

  res[["det_summary_scores"]] <- Reduce(
    function(x, y) suppressMessages(dplyr::inner_join(x, y)),
    det_summary_scores
  )

  rm(det_summary_scores)

  if (!is.null(thresholds)) {

    res[["det_threshold_scores"]] <- compute_det_score(
      groupings, .fcst, fcst_col, chr_param, fcst_model, circle, num_bins,
      "threshold", show_progress, thresholds = thresholds,
      comparator = comparator, include_low = include_low,
      include_high = include_high
    )

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
  summary       = TRUE,
  hexbin        = TRUE,
  num_bins      = 30,
  show_progress = TRUE,
  ...
) {

  parameter   <- rlang::ensym(parameter)

  comparator <- match.arg(comparator)

  list_to_harp_verif(
    purrr::imap(
      .fcst,
      ~det_verify(
        .x, {{parameter}}, thresholds, comparator, include_low, include_high,
        groupings, circle, summary, hexbin, num_bins,
        show_progress, fcst_model = .y, ...
      )
    )
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


# Get the total for a progress bar
get_pb_total <- function(df, grp, multi_grp) {
  dplyr::n_groups(
    group_without_threshold(
      df, setdiff(grp, unique(multi_grp[["key"]])),
      nest = FALSE
    )
  ) * nrow(multi_grp)
}

# Standard dev with a progress bar
sd_pb <- function(x, show_prog, env) {
  res <- stats::sd(x)
  if (show_prog) {
    cli::cli_progress_update(.envir = env)
  }
  res
}

# Score function for list of groups - this should be called by the user facing
# function. score_name should have a matching function called
# compute_det_<score_name>()
compute_det_score <- function(
  grps_list, fcst_df, fcst_col, obs_col, fcst_model, circle, num_bins,
  score_name, show_progress, thresholds = NULL, comparator = "ge",
  include_low = TRUE, include_high = TRUE
) {
  lapply(
    grps_list,
    function(g) {
      compute_grp_det_score(
        g, fcst_df, fcst_col, obs_col, circle, num_bins,
        score_name, show_progress, thresholds,
        comparator = comparator, include_low = include_low,
        include_high = include_high
      )
    }
  ) %>%
    purrr::list_rbind() %>%
    fill_group_na(grps_list) %>%
    #tidyr::unnest(dplyr::all_of("verif")) %>%
    dplyr::mutate(fcst_model = fcst_model, .before = dplyr::everything())
}

# Function to call a deterministic score function for a single set of groups
compute_grp_det_score <- function(
  compute_group, fcst_df, fcst_col, obs_col, circle, num_bins,
  score_name, show_progress, thresholds = NULL, comparator = "ge",
  include_low = TRUE, include_high = TRUE
) {

  local_fcst_col <- intersect(c(fcst_col, "fcst"), colnames(fcst_df))

  # Make generic observations column
  colnames(fcst_df)[colnames(fcst_df) == obs_col] <- "obs"
  obs_col <- "obs"

  fcst_df <- dplyr::mutate(
    fcst_df,
    fcst_bias = bias(.data[[local_fcst_col]], .data[[obs_col]], circle)
  )

  # Where there are multiple groups per row, these should be filtered
  # and looped over

  multi_groups <- data.frame(key = NA, value = NA)
  mg_attr <- attr(fcst_df, "multi_groups")
  mg      <- intersect(compute_group, names(mg_attr))
  if (length(mg) > 0) {
    multi_groups <- dplyr::bind_rows(lapply(
      mg,
      function(g) data.frame(key = g, value = mg_attr[[g]])
    ))
  }

  group_vars  <- compute_group
  if (is.null(thresholds)) {
    thresholds <- NA
  } else {
    group_vars <- union("threshold", group_vars)
    thresholds <- check_thresholds(thresholds, comparator)
  }
  group_names <- glue::glue_collapse(group_vars, sep = ", ", last = " & ")
  score_text  <- cli::col_blue(glue::glue("Det {score_name} for {group_names}"))

  score_func <- get(paste0("compute_det_", score_name))

  if (show_progress) {
    pb_name  <- score_text
    pb_total <- get_pb_total(fcst_df, compute_group, multi_groups) *
      length(thresholds)
    pb_env   <- environment()
    cli::cli_progress_bar(pb_name, total = pb_total)
  } else {
    pb_name <- FALSE
    message(score_text, appendLF = FALSE)
    score_text <- ""
  }


  fcst_df <- dplyr::bind_rows(lapply(
    thresholds,
    function(t, ...) {
      if (!all(is.na(t))) {
        fcst_df <- prep_thresh_data(
          fcst_df, obs_col, t, comparator, include_low, include_high
        )
        compute_group <- union("threshold", compute_group)
      }
      dplyr::bind_rows(
        purrr::map2(
          multi_groups$key, multi_groups$value,
          function(key, value, fcst_data = fcst_df) {
            if (!is.na(key) && !is.na(value)) {
              fcst_data <- dplyr::filter(
                fcst_data, grepl(value, .data[[key]], fixed = TRUE)
              )
              fcst_data[[key]] <- gsub("[<>]", "", value)
            }
            fcst_data <- dplyr::group_by(fcst_data, !!!rlang::syms(compute_group))
            score_func(fcst_data, show_progress, pb_env, ...)
          }
        )
      )
    },
    num_bins = num_bins
  ))

  message(score_text, cli::col_green(cli::symbol[["tick"]]))
  if (show_progress) {
    cli::cli_progress_done(.envir = pb_env)
  }
  fcst_df
}


# Summary scores
compute_det_summary <- function(grouped_fcst, show_prog, pb_env, ...) {
  dplyr::summarise(
    grouped_fcst,
    num_stations = length(unique(!!rlang::sym("SID"))),
    num_cases    = dplyr::n(),
    bias         = mean(!!rlang::sym("fcst_bias")),
    rmse         = sqrt(mean((!!rlang::sym("fcst_bias")) ^ 2)),
    mae          = mean(abs(!!rlang::sym("fcst_bias"))),
    stde         = sd_pb(
      !!rlang::sym("fcst_bias"), show_prog, pb_env
    ),
    .groups = "drop"
  )
}

# Hexbin
compute_det_hexbin <- function(grouped_fcst, show_prog, pb_env, num_bins, ...) {
  dplyr::summarise(
    grouped_fcst,
    num_stations = length(unique(!!rlang::sym("SID"))),
    num_cases    = dplyr::n(),
    hexbin       = list(
      hexbin_df(.data[["obs"]], .data[["fcst"]], num_bins, show_prog, pb_env)
    ),
    .groups = "drop"
  )
}


# Threshold scores
prep_thresh_data <- function(
    fcst_df, obs_col, threshold, comparator, include_low, include_high
) {
  fcst_df <- det_probabilities(
    fcst_df, !!rlang::sym(obs_col), threshold, comparator, include_low, include_high
  )[[1]]

  dplyr::mutate(
    fcst_df,
    a = as.numeric(.data[["fcst_prob"]] & .data[["obs_prob"]]),    # Hit
    b = as.numeric(.data[["fcst_prob"]] & !.data[["obs_prob"]]),   # False Alarm
    c = as.numeric(!.data[["fcst_prob"]] & .data[["obs_prob"]]),   # Miss
    d = as.numeric(!.data[["fcst_prob"]] & !.data[["obs_prob"]])   # Correct Rejection
  )
}

compute_det_threshold <- function(grouped_fcst, show_prog, pb_env, ...) {
  dplyr::summarise(
    grouped_fcst,
    num_stations = length(unique(!!rlang::sym("SID"))),
    num_cases    = dplyr::n(),
    verif        = list(harp_table_stats(
      sum(.data[["a"]]), sum(.data[["b"]]),
      sum(.data[["c"]]), sum(.data[["d"]]),
      dplyr::n(), show_prog, pb_env
    )),
    .groups = "drop"
  ) %>%
    tidyr::unnest("verif")
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


