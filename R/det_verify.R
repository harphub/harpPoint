#' Compute verification scores for deterministic forecasts.
#'
#' @inheritParams ens_verify
#' @param new_det_score String. The name of a new score to compute from
#'   forecast and observation pairs. `det_verify()` will search the global
#'   environment for a function called `compute_det_<new_det_score>()` and
#'   optionally `prep_det_<new_det_score>()`. `compute_det_<new_det_score>`
#'   should take a grouped data frame, a logical indicating whether to use a
#'   progress bar, the progress bar environment and a named list of options to
#'   be used in the function. The function should do some form of aggregation
#'   for each group using \code{\link[dplyr]{summarize}} or similar and must
#'   call `tick_progress()` with the show progress logical and the progress bar
#'   environment. `prep_det_<new_det_score>()` is used to create new columns in
#'   the data frame prior to the score aggregation and should take the data
#'   frame, the name of the forecast column, the name of the observations
#'   column and a named list of options to be used in the function. See
#'   \strong{Adding your own score} below.
#' @param new_det_cont_score String. The name of a function that will compute
#'   a score from contingency table values and or binary probabilities. The
#'   function should exist in the global environment and take as its arguments
#'   (in this order): observed binary probability, forecast binary probability,
#'   hit, false alarm, miss, correct rejection, a logical of whether a progress
#'   bar is used the progress bar environment and a named list of options for
#'   the function. The function must call `tick_progress()` with the show
#'   progress logical and the progress bar environment. See
#'   \strong{Adding your own score} below.
#' @param new_det_score_opts A named list of options for use in any of
#'   `prep_det_<new_det_score>()`, `compute_det_<new_det_score>()`,
#'   `<new_det_cont_score>`. See \strong{Adding your own score} below.
#'
#' @section Adding your own scores:
#'
#' You can add functions to compute scores using the infrastructure of
#' `det_verify()`. These can be for scores calculated from forecast observation
#' pairs, or from binary probabilities and/or contingency table counts for a
#' given threshold and comparator.
#'
#' ## Scores computed from forecast observation pairs.
#'
#' The name of the score is specified in the `new_det_score` argument and
#' `det_verify()` will look for a function with the name
#' `compute_ens_<new_det_score>` that will compute the score using
#' \code{\link[dplyr]{summarize}} or similar. It will optionally look for a
#' function with the name `prep_ens_<new_det_score>` that will do some
#' preparation of the data for each row before aggregation into the score. This
#' is best illustrated with an example:
#'
#' Let's say we want to compute the route mean square error factor. That is to
#' say, the RMS of the ratio of the forecasted value to the observed value. We
#' can first prepare the data by writing a function to compute the sqaured
#' factor for each forecast observation pair before aggregating:
#'
#' \preformatted{
#' prep_det_rmsf <- function(df, fc_col, ob_col, ...) {
#'   dplyr::mutate(
#'     df,
#'     fctr_sq = (.data[[fc_col]] / .data[[ob_col]]) ^ 2
#'   )
#' }
#' }
#'
#' The above function will have the squared ratio of forecasted value to the
#' observed value in the `fctr_sq` column.
#'
#' For the computation of the score, we can use \code{\link[dplyr]{summarize}}
#' to compute the mean of the squared value for each group. The compute function
#' needs to take grouped data frame, some details about the progress bar and
#' optionally a list of arguments.
#'
#' \preformatted{
#' compute_det_rmsf <- function(grouped_df, show_pb, pb_env, ...) {
#'   res <- dplyr::summarize(
#'     grouped_df,
#'     rmsf = sqrt(mean(.data[["fctr_sq"]]))
#'   )
#'   tick_progress(show_pb, pb_env)
#'   res
#' }
#' }
#'
#' We can now run `det_verify(data, obs_col, new_det_score = "rmsf")`
#'
#' We could also make it an option whether to compute the total factor or the
#' ratio of the the difference in the forecasted and observed values. We can
#' do this by adding the option to `new_det_score_opts`
#'
#' \preformatted{
#' prep_det_rmsf <- function(df, fc_col, ob_col, opts) {
#'   if (opts$diff){
#'     dplyr::mutate(
#'       df,
#'       fctr_sq = ((.data[[fc_col]] - .data[[ob_col]]) / .data[[ob_col]]) ^ 2
#'     )
#'   } else {
#'     dplyr::mutate(
#'       df,
#'       fctr_sq = (.data[[fc_col]] / .data[[ob_col]]) ^ 2
#'     )
#'   }.Las
#' }
#' }
#'
#' and then running:
#'
#' \preformatted{
#' det_verify(
#'   data,
#'   obs_col,
#'   new_det_score      = "rmsf",
#'   new_det_score_opts = list(diff = TRUE)
#' )
#' }
#'
#'
#' ## Scores computed from binary probabilities or contingency table counts
#' Computing scores from binary probabilities or contingency table counts is
#' much simpler, since `det_verify()` has already prepped the data and you have
#' the binary forecast and the observed probabilities as well as the counts of
#' hits, false alrms, misses and correct rejections. Here you need to write a
#' function that takes the observed probability, the forecast probability, the
#' hit, false alarm, miss and correct rejection, some information about
#' the progress bar, and optionally some options for the function.
#'
#' Since `det_verify()` computes most known contingency table based scores, in
#' this example we will compute the binary Brier Score as the MSE in
#' probability space and (arbitrarily - this is for illustration purposes only)
#' weight it by the success raio (hits / (hits + false alarms))
#' \preformatted{
#' prob_diff_brier <- function(
#'   ob_prob, fc_prob, hit, false_alarm, miss, correct_rejection,
#'   show_pb, pb_env, opts
#' ) {
#'   brier  <- mean((fc_prob - ob_prob) ^ 2)
#'   weight <- 1
#'   if (opts$sr_weighting) {
#'     weight <- sum(hit) / (sum(hit) + sum(false_alarm))
#'   }
#'   brier <- brier * weight
#'   tick_progress(show_pb, pb_env)
#'   brier
#' }
#' }
#'
#' And now we can run:
#' \preformatted{
#' det_verify(
#'   data,
#'   obs_col,
#'   thresholds         = c(0, 0.5),
#'   new_det_cont_score = "brier",
#'   new_det_score_opts = list(sr_weighting = FALSE)
#' )
#' }
#'
#' @return A list containing up to two data frames: \code{det_summary_scores}
#'   and \code{det_threshold_scores}.
#'
#' @export
#'
det_verify <- function(
  .fcst,
  parameter,
  thresholds         = NULL,
  clean_thresh       = TRUE,
  comparator         = c("ge", "gt", "le", "lt", "eq", "between", "outside"),
  include_low        = TRUE,
  include_high       = TRUE,
  groupings          = "lead_time",
  circle             = NULL,
  summary            = TRUE,
  hexbin             = TRUE,
  num_bins           = 30,
  show_progress      = TRUE,
  new_det_score      = NULL,
  new_det_cont_score = NULL,
  new_det_score_opts = list(),
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
  thresholds         = NULL,
  clean_thresh       = TRUE,
  comparator         = c("ge", "gt", "le", "lt", "eq", "between", "outside"),
  include_low        = TRUE,
  include_high       = TRUE,
  groupings          = "lead_time",
  circle             = NULL,
  summary            = TRUE,
  hexbin             = TRUE,
  num_bins           = 30,
  show_progress      = TRUE,
  new_det_score      = NULL,
  new_det_cont_score = NULL,
  new_det_score_opts = list(),
  fcst_model         = NULL,
  ...
) {

  comparator <- match.arg(comparator)

  if (!is.list(groupings)) {
    groupings <- list(groupings)
  }

  member_cols <- harpCore::member_colnames(.fcst)

  groupings <- purrr::map(groupings, ~union(c("sub_model", "member"), .x))

  bind_point_verif(
    lapply(
      member_cols,
      function(mbr) {
        excluded_mbrs <- member_cols[member_cols != mbr]
        sub_model_name <- regmatches(
          mbr, regexpr("[[:graph:]]+(?=_mbr[0-9]{3})", mbr, perl = TRUE)
        )
        mbr_name <- regmatches(mbr, regexpr("mbr[0-9]{3}[[:graph:]]*", mbr))
        cli::cli_inform(
          cli::col_cyan(
            "Det member verification for: {cli::col_br_yellow(mbr)}"
          )
        )
        det_verify(
          harpCore::as_det(
            dplyr::mutate(
              dplyr::select(.fcst, -dplyr::all_of(excluded_mbrs)),
              member    = mbr_name,
              sub_model = sub_model_name
            )
          ),
          {{parameter}}, thresholds, clean_thresh,
          comparator, include_low, include_high,
          groupings, circle, summary, hexbin, num_bins,
          show_progress, fcst_model, ...
        )
      }
    )
  )
}


#' @rdname det_verify
#' @inheritParams ens_verify.harp_ens_point_df
#' @export
det_verify.harp_det_point_df <- function(
  .fcst,
  parameter,
  thresholds         = NULL,
  clean_thresh       = TRUE,
  comparator         = c("ge", "gt", "le", "lt", "eq", "between", "outside"),
  include_low        = TRUE,
  include_high       = TRUE,
  groupings          = "lead_time",
  circle             = NULL,
  summary            = TRUE,
  hexbin             = TRUE,
  num_bins           = 30,
  show_progress      = TRUE,
  new_det_score      = NULL,
  new_det_cont_score = NULL,
  new_det_score_opts = list(),
  fcst_model         = NULL,
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

  if (clean_thresh) {
    thresholds <- clean_thresholds(
      .fcst[c(fcst_col, chr_param)], thresholds, comparator
    )
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
    det_summary_scores[["basic"]] <- compute_score(
      groupings, .fcst, fcst_col, chr_param, fcst_model,
      "summary", show_progress, score_opts = list(circle = circle)
    )
  }

  if (hexbin) {
    det_summary_scores[["hexbin"]] <- compute_score(
      groupings, .fcst, fcst_col, chr_param, fcst_model,
      "hexbin", show_progress, score_opts = list(num_bins = 30)
    )
  }

  for (new_score in new_det_score) {
    det_summary_scores[[new_score]] <- compute_score(
      groupings, .fcst, fcst_col, chr_param, fcst_model,
      new_score, show_progress, score_opts = new_det_score_opts
    )
  }


  res[["det_summary_scores"]] <- Reduce(
    function(x, y) suppressMessages(dplyr::inner_join(x, y)),
    det_summary_scores
  )

  rm(det_summary_scores)

  if (!is.null(thresholds)) {

    res[["det_threshold_scores"]] <- compute_score(
      groupings, .fcst, fcst_col, chr_param, fcst_model,
      "threshold", show_progress, thresholds = thresholds,
      comparator = comparator, include_low = include_low,
      include_high = include_high,
      score_opts = c(
        list(
          new_det_cont_score = new_det_cont_score
        ),
        new_det_score_opts
      )
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
  thresholds         = NULL,
  clean_thresh       = TRUE,
  comparator         = c("ge", "gt", "le", "lt", "eq", "between", "outside"),
  include_low        = TRUE,
  include_high       = TRUE,
  groupings          = "lead_time",
  circle             = NULL,
  summary            = TRUE,
  hexbin             = TRUE,
  num_bins           = 30,
  show_progress      = TRUE,
  new_det_score      = NULL,
  new_det_cont_score = NULL,
  new_det_score_opts = list(),
  ...
) {

  parameter   <- rlang::ensym(parameter)

  comparator <- match.arg(comparator)

  list_to_harp_verif(
    purrr::imap(
      .fcst,
      ~det_verify(
        .x, {{parameter}}, thresholds, clean_thresh, comparator,
        include_low, include_high, groupings, circle, summary, hexbin, num_bins,
        show_progress, new_det_score, new_det_cont_score, new_det_score_opts,
        fcst_model = .y, ...
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

fun_exists <- function(fun, warn = TRUE) {
  if (exists(fun) && is.function(get(fun))) {
    return(TRUE)
  }
  if (warn) {
    cli::cli_warn(c(
      "Cannot find a function for {fun}."
    ))
  }
  return(FALSE)
}

# Remove thresholds that result in no cases
clean_thresholds <- function(all_data, thresholds, comparator) {
  if (is.null(thresholds) || comparator == "eq") {
    return(thresholds)
  }
  data_range <- range(all_data)
  if (!is.list(thresholds)) {
    thresholds  <- sort(thresholds)
    low_thresh  <- which(thresholds <= data_range[1])
    high_thresh <- which(thresholds >= data_range[2])
    if (length(low_thresh) < 1) {
      thresh_1 <- 1
    } else {
      thresh_1 <- low_thresh[length(low_thresh)]
    }
    if (length(high_thresh) < 1) {
      thresh_2 <- length(thresholds)
    } else {
      thresh_2 <- high_thresh[1]
    }
    thresholds <- thresholds[thresh_1:thresh_2]
    if (comparator %in% c("gt", "ge")) {
      thresholds <- thresholds[thresholds <= data_range[2]]
    }
    if (comparator %in% c("lt", "le")) {
      thresholds <- thresholds[thresholds >= data_range[1]]
    }
    return(thresholds)
  }

  if (comparator == "between") {
    first_last <- which(
      vapply(
        thresholds,
        function(x) any((data_range <= max(x) & data_range >= min(x))),
        logical(1)
      )
    )
    return(thresholds[do.call(seq, as.list(first_last))])
  }

  # Not sure how to handle outside so just return thresholds
  thresholds
}

# Score function for list of groups - this should be called by the user facing
# function. score_name should have a matching function called
# compute_det_<score_name>()
compute_score <- function(
  grps_list, fcst_df, fcst_col, obs_col, fcst_model,
  score_name, show_progress, thresholds = NULL, comparator = "ge",
  include_low = TRUE, include_high = TRUE, score_opts = list(), type = "det"
) {
  prep_fun <- paste("prep", type, score_name, sep = "_")
  if (fun_exists(prep_fun, warn = FALSE)) {
    prep_fun <- get(prep_fun)
    fcst_df  <- prep_fun(fcst_df, fcst_col, obs_col, score_opts)
  }
  lapply(
    grps_list,
    function(g) {
      compute_grp_score(
        g, fcst_df, fcst_col, obs_col,
        score_name, show_progress, thresholds,
        comparator = comparator, include_low = include_low,
        include_high = include_high, score_opts, type
      )
    }
  ) %>%
    purrr::list_rbind() %>%
    fill_group_na(grps_list) %>%
    #tidyr::unnest(dplyr::all_of("verif")) %>%
    dplyr::mutate(fcst_model = fcst_model, .before = dplyr::everything())
}

# Function to call a deterministic score function for a single set of groups
compute_grp_score <- function(
  compute_group, fcst_df, fcst_col, obs_col,
  score_name, show_progress, thresholds = NULL, comparator = "ge",
  include_low = TRUE, include_high = TRUE, score_opts = list(), type = "det"
) {

  local_fcst_col <- intersect(c(fcst_col, "fcst"), colnames(fcst_df))

  # Make generic observations column
  colnames(fcst_df)[colnames(fcst_df) == obs_col] <- "obs"
  obs_col <- "obs"

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
  score_text  <- cli::col_blue(glue::glue("{type}: {score_name} for {group_names}"))

  score_func <- get(paste("compute", type, score_name, sep = "_"))

  if (show_progress) {
    pb_name  <- score_text
    pb_total <- get_pb_total(fcst_df, compute_group, multi_groups) *
      length(thresholds)
    pb_total <- pb_total + pb_total * (sum(
      score_opts$brier | score_opts$reliability,
      score_opts$fair_brier,
      score_opts$roc,
      score_opts$econ_val,
      length(score_opts$new_ens_prob_score),
      length(score_opts$new_det_cont_score)
    ))
    pb_env   <- environment()
    cli::cli_progress_bar(pb_name, total = pb_total)
  } else {
    pb_name  <- FALSE
    pb_env   <- NULL
    appendLF <- (score_name %in% c("prob_scores", "tw_crps") && type == "ens") ||
      (score_name %in% c("threshold") && type == "det")
    message(score_text, appendLF = appendLF)
    score_text <- ""
  }


  fcst_df <- dplyr::bind_rows(lapply(
    thresholds,
    function(t) {
      if (!all(is.na(t))) {
        cli::cli_alert_info(
          cli::col_br_red(
            paste(
              "Threshold:",
              make_threshcol(t, comparator, include_low, include_high)
            )
          )
        )
        prep_fun <- paste("prep", type, "thresh", "data", sep = "_")
        if (exists(prep_fun) && is.function(get(prep_fun))) {
          prep_fun <- get(prep_fun)
          fcst_df <- prep_fun(
            fcst_df, obs_col, t, comparator, include_low, include_high,
            score_opts
          )
        }
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
            score_func(fcst_data, show_progress, pb_env, score_opts)
          }
        )
      )
    }
  ))

  end_msg <- paste(score_text, cli::col_green(cli::symbol[["tick"]]))
  if (show_progress) {
    cli::cli_progress_done(.envir = pb_env)
  } else {
    if ((score_name %in% c("prob_scores", "tw_crps") && type == "ens") ||
        (score_name %in% c("threshold") && type == "det")) {
      end_msg <- paste0("\n", end_msg)
    }
  }
  message(end_msg)
  fcst_df
}

# Prepare ungrouped data frame
prep_det_summary <- function(.fcst, fcst_col, obs_col, opts, ...) {
  dplyr::mutate(
    .fcst,
    fcst_bias = bias(.data[["fcst"]], .data[[obs_col]], opts$circle)
  )
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
    mean_fcst    = mean(!!rlang::sym("fcst")),
    mean_obs     = mean(!!rlang::sym("obs")),
    .groups = "drop"
  )
}

# Hexbin
compute_det_hexbin <- function(grouped_fcst, show_prog, pb_env, opts) {
  dplyr::summarise(
    grouped_fcst,
    num_stations = length(unique(!!rlang::sym("SID"))),
    num_cases    = dplyr::n(),
    hexbin       = list(
      hexbin_df(.data[["obs"]], .data[["fcst"]], opts$num_bins, show_prog, pb_env)
    ),
    .groups = "drop"
  )
}

hexbin_df <- function(
    x, y, nbins, show_pb = FALSE, env = rlang::caller_env(), no_compute = TRUE) {
  if (no_compute && length(x) < 100) {
    if (show_pb) {
      cli::cli_progress_update(.envir = env)
    }
    return(tibble::tibble(obs = x, fcst = y, count = 1))
  }
  xrange <- range(x)
  yrange <- range(y)
  if (diff(xrange) == 0) {
    if (all(x == 0)) {
      xrange <- c(-0.01, 0.01)
    } else {
      xrange[1] <- xrange[1] - abs(xrange[1]) * 0.01
      xrange[2] <- xrange[2] + abs(xrange[2]) * 0.01
    }
  }
  if (diff(yrange) == 0) {
    if (all(y == 0)) {
      yrange <- c(-0.01, 0.01)
    } else {
      yrange[1] <- yrange[1] - abs(yrange[1]) * 0.01
      yrange[2] <- yrange[2] + abs(yrange[2]) * 0.01
    }
  }
  hexes <- hexbin::hexbin(x, y, xbins = nbins, xbnds = xrange, ybnds = yrange)
  res <- dplyr::rename(
    dplyr::mutate(
      tibble::as_tibble(hexbin::hcell2xy(hexes)),
      count = hexes@count
    ),
    obs  = .data[["x"]],
    fcst = .data[["y"]]
  )
  if (show_pb) {
    cli::cli_progress_update(.envir = env)
  }
  res
}



# Threshold scores
prep_det_thresh_data <- function(
    fcst_df, obs_col, threshold, comparator, include_low, include_high, ...
) {
  fcst_df <- det_probabilities(
    fcst_df, !!rlang::sym(obs_col), threshold, comparator, include_low, include_high
  )

  dplyr::mutate(
    fcst_df,
    a = as.numeric(.data[["fcst_prob"]] & .data[["obs_prob"]]),    # Hit
    b = as.numeric(.data[["fcst_prob"]] & !.data[["obs_prob"]]),   # False Alarm
    c = as.numeric(!.data[["fcst_prob"]] & .data[["obs_prob"]]),   # Miss
    d = as.numeric(!.data[["fcst_prob"]] & !.data[["obs_prob"]])   # Correct Rejection
  )
}

compute_det_threshold <- function(
    grouped_fcst, show_prog, pb_env, opts = list()
) {
  res <- list()
  i   <- 1
  res[[i]] <- dplyr::summarise(
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

  for (new_score in opts$new_det_cont_score) {

    i <- i + 1

    if (!fun_exists(new_score)) {
      cli::cli_alert_info("Skipping computation of {new_score}.")
      next()
    }

    col_name <- rlang::sym(new_score)

    if (show_prog) {
      cli::cli_progress_output(
        cli::col_br_cyan(paste0("  ", new_score)), .envir = pb_env
      )
    } else {
      message(cli::col_br_cyan(paste0("  ", new_score, " ")), appendLF = FALSE)
    }

    res[[i]] <- dplyr::summarise(
      grouped_fcst,
      !!col_name := get(new_score)(
        .data[["obs_prob"]],
        .data[["fcst_prob"]],
        .data[["a"]],
        .data[["b"]],
        .data[["c"]],
        .data[["d"]],
        show_prog,
        pb_env,
        opts
      )
    )

    if (!show_prog) {
      message(cli::col_br_green(cli::symbol$tick))
    }

  }

  Reduce(
    function(x, y) dplyr::inner_join(
      x, y, by = intersect(colnames(x), colnames(y))
    ),
    res
  )


}







