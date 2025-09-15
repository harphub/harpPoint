#' Compute all verification scores for an ensemble.
#'
#' @param .fcst A `harp_df` or `harp_list` object with tables that have a column
#'   for observations, or a single forecast table.
#' @param parameter The name of the column for the observations data. Can be the
#'   column name, quoted, or unquoted. If a variable it should be embraced -
#'   i.e. wrapped in `{{}}`.
#' @param verify_members Whether to verify the individual members of the
#'   ensemble. Even if thresholds are supplied, only summary scores are
#'   computed. If you wish to compute categorical scores, the separate
#'   \link[harpPoint]{det_verify} function must be used.
#' @param thresholds A numeric vector of thresholds for which to compute the
#'   threshold based scores. Set to NULL (the default) to only compute summary
#'   scores.
#' @param groupings The groups for which to compute the scores. See
#'   \link[dplyr]{group_by} for more information of how grouping works.
#' @param summary Logical. Whether to compute summary scores or not. Default is
#'   `TRUE`.
#' @param circle If set the parameter is assumed to be cyclic for bias
#'   calculations. Should be this distance around a circle in the units of the
#'   parameter, so would typically have a value of 360 for degrees or `2 * pi`
#'   for radians.
#' @param rel_probs Probabilities to use for reliability diagrams. Set to NA
#'   (the default) to select automatically.
#' @param num_ref_members For "fair" scores, the score is scaled to be valid for
#'   this number of ensemble members. Set to NA (the default) to not modify the
#'   score.
#' @param spread_drop_member Which members to drop for the calculation of the
#'   ensemble variance and standard deviation. For harp_fcst objects, this can
#'   be a numeric scalar - in which case it is recycled for all forecast models;
#'   a list or numeric vector of the same length as the harp_fcst object, or a
#'   named list with the names corresponding to names in the harp_fcst object.
#' @param jitter_fcst A function to perturb the forecast values by. This is used
#'   to account for observation error in the rank histogram. For other
#'   statistics it is likely to make little difference since it is expected that
#'   the observations will have a mean error of zero.
#' @param climatology The climatology to use for the Brier Skill Score. Can be
#'   "sample" for the sample climatology (the default), a named list with
#'   elements eps_model and member to use a member of an eps model in the
#'   harp_fcst object for the climatology, or a data frame with columns for
#'   threshold and climatology and also optionally lead_time.
#' @param hexbin Logical. Whether to compute hexbins for forecast, observation
#'   pairs. Defaults to `TRUE`. See \link{bin_fcst_obs} for more details.
#' @param num_bins The number of bins into which to partition observations for
#'   the hexbin computation.
#' @param rank_hist Logical. Whether to compute the rank histogram. Defaults to
#'   `TRUE`. Note that the computation of the rank histogram can be slow if
#'   there is a large number (> 1000) of groups.
#' @param crps Logical. Whether to compute the CRPS. Defaults to `TRUE`.
#' @param crps_decomp Logical. Whether to compute the decomposition of the CRPS
#'   into potential and reliability components.
#' @param tw_crps Logical. Whether to compute the the threshold weighted CRPS.
#'   Note that tw_crps cannot be computed for `comparator = "eq"` or
#'   `comparator = "outside"`. Will be ignored if no thresholds are set.
#' @param brier Logical. Whether to compute the Brier score. Defaults to `TRUE`.
#'   Will be ignored if no thresholds are set.
#' @param reliability Logical. Whether to compute the reliability. Defaults to
#'   `TRUE`. Will be ignored if no thresholds are set.
#' @param roc Logical. Whether to compute the Relative Operating Characteristic
#'   (ROC). Defaults to `TRUE`. Will be ignored if no thresholds are set.
#' @param econ_val Logical. Whether to compute the economic value. Defaults to
#'   `TRUE`. Will be ignored if no thresholds are set.
#' @param show_progress Logical - whether to show progress bars. Defaults to
#'   `TRUE`.
#' @param new_ens_score Character vector. Names of other
#'   scores to be computed that are not part of this function. Requires a
#'   `compute_ens_<name>()` and optionally a `prep_ens_<name>()` function. See
#'   \strong{Adding your own scores}.
#' @param new_ens_prob_score Character vector. Names of functions to compute
#'   scores from the observed and forecast probabilities. See
#'   \strong{Adding your own scores}.
#' @param new_score_opts Named list of options for `new_ens_score` and
#'   `new_ens_prob_score`.
#' @param ... Reserved for methods.
#'
#' @section Adding your own scores:
#'
#' You can add functions to compute scores using the infrastructure of
#' `ens_verify()`. These can be for scores calculated from the raw ensemble,
#' or from probabilities based on the thresholds and comparator.
#'
#' ## Scores computed from the raw ensemble
#' The name of the score is specified in the `new_ens_score` argument and
#' `ens_verify()` will look for a function with the name `compute_ens_<name>`
#' that will compute the score using \code{\link[dplyr]{summarize}} or
#' similar. It will optionally look for a function with the name
#' `prep_ens_<name>` that will do some preparation of the data for each row
#' before aggregation into the score. This is best illustrated with a
#' (slightly contrived!) example:
#'
#' Let's say we want to compute the mean bias weighted by latitude, and we call
#' the score "weighted_bias". We begin by writing a prep function, which has
#' to take the forecast data frame, the name of the forecast columns, the
#' name of the observation column and optionally a list of options as arguments.
#'
#' \preformatted{
#' prep_ens_weighted_bias <- function(df, fc_col, obs_col, ...) {
#'   df <- dplyr::mutate(
#'     df,
#'     dplyr::across(
#'       dplyr::all_of(fc_col),
#'       ~(.x - .data[[obs_col]]) * abs(lat / 90)
#'      )
#'   )
#'   ens_stats(df, sd = FALSE)
#' }
#' }
#'
#' The above function will have the weighted mean bias in the `ens_mean` column
#' created by the \code{\link[harpCore]{ens_stats()}} function.
#'
#' For the computation of the score, we can use \code{\link[dplyr]{summarize}}
#' to compute the mean for each group. The compute function needs to take
#' grouped data frame, some details about the progress bar and optionally a list
#' of arguments.
#'
#' \preformatted{
#' compute_ens_weighted_bias <- function(grouped_df, show_pb, pb_env, ...) {
#'   res <- dplyr::summarize(
#'     grouped_df,
#'     weighted_bias = sum(ens_mean) / sum(abs(lat / 90))
#'   )
#'   tick_progress(show_pb, pb_env)
#'   res
#' }
#' }
#'
#' We can now run `ens_verify(data, obs_col, new_ens_score = "weighted_bias")`
#'
#' We could also make it an option whether to weight by any column by modifying
#' `prep_ens_weighted_bias` and `compute_ens_weighted_bias` and adding the
#' option to `new_ens_score_opts`
#'
#' \preformatted{
#' prep_ens_weighted_bias <- function(df, fc_col, obs_col, opts) {
#'   df <- dplyr::mutate(
#'     df,
#'     dplyr::across(
#'       dplyr::all_of(fc_col),
#'       ~(.x - .data[[obs_col]]) * abs(.data[[opts$weight_col]])
#'      )
#'   )
#'   ens_stats(df, sd = FALSE)
#' }
#' }
#'
#' \preformatted{
#' compute_ens_weighted_bias <- function(grouped_df, show_pb, pb_env, opts) {
#'   res <- dplyr::summarize(
#'     grouped_df,
#'     weighted_bias = sum(ens_mean) / sum(abs(.data[[opts$weight_col]]))
#'   )
#'   tick_progress(show_pb, pb_env)
#'   res
#' }
#' }
#'
#' And then running
#'
#' \preformatted{
#' ens_verify(
#'   data,
#'   obs_col,
#'   new_ens_score = "weighted_bias",
#'   new_ens_score_opts = list(weight_col = "lon")
#' )
#' }
#'
#' ## Scores computed from probabilities
#' Computing scores from probabilities is much simpler, since `ens_verify()` has
#' already prepped the data and you have the forecast probability and the
#' observed (binary) probability. Here you need to write a function that takes
#' the observed probability, the forecast probability, some information about
#' the progress bar, and optionally some options for the function.
#'
#' In this example we will compute some quantiles of the difference between
#' the forecast and observed probabilities.
#'
#' \preformatted{
#' prob_diff_quant <- function(ob_prob, fc_prob, show_pb, pb_env, opts) {
#'   res <- quantile(fc_prob - ob_prob, probs = opts$quants)
#'   tick_progress(show_pb, pb_env)
#'   if (length(res) < 1) {
#'     return(res)
#'   }
#'   list(res) # we need to return a list if more than 1 value.
#' }
#' }
#'
#' And now we can run:
#' \preformatted{
#' ens_verify(
#'   data,
#'   obs_col,
#'   thresholds         = c(0, 0.5),
#'   new_ens_prob_score = "prob_diff_quant",
#'   new_ens_score_opts = list(quants = c(0.9, 0.99, 0.995, 1))
#' )
#' }
#'
#' @return A list containing three data frames: \code{ens_summary_scores},
#'   \code{ens_threshold_scores} and \code{det_summary_scores}.
#' @export
ens_verify <- function(
  .fcst,
  parameter,
  verify_members     = TRUE,
  thresholds         = NULL,
  comparator         = c("ge", "gt", "le", "lt", "eq", "between", "outside"),
  include_low        = TRUE,
  include_high       = TRUE,
  groupings          = "lead_time",
  summary            = TRUE,
  circle             = NULL,
  rel_probs          = NA,
  num_ref_members    = NA,
  spread_drop_member = NULL,
  jitter_fcst        = NULL,
  climatology        = "sample",
  hexbin             = TRUE,
  num_bins           = 30,
  rank_hist          = TRUE,
  crps               = TRUE,
  crps_decomp        = TRUE,
  tw_crps            = TRUE,
  brier              = TRUE,
  reliability        = TRUE,
  roc                = TRUE,
  econ_val           = TRUE,
  show_progress      = TRUE,
  new_ens_score      = NULL,
  new_ens_prob_score = NULL,
  new_ens_score_opts = list(),
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
  UseMethod("ens_verify")
}

#' @param fcst_model The name of the forecast model to use in the `fcst_model`
#'  column of the output. If the function is dispatched on a `harp_list`
#'  object, the names of the `harp_list` are automatically used.
#' @rdname ens_verify
#' @export
ens_verify.harp_ens_point_df <- function(
  .fcst,
  parameter,
  verify_members     = TRUE,
  thresholds         = NULL,
  comparator         = c("ge", "gt", "le", "lt", "eq", "between", "outside"),
  include_low        = TRUE,
  include_high       = TRUE,
  groupings          = "lead_time",
  summary            = TRUE,
  circle             = NULL,
  rel_probs          = NA,
  num_ref_members    = NA,
  spread_drop_member = NULL,
  jitter_fcst        = NULL,
  climatology        = "sample",
  hexbin             = TRUE,
  num_bins           = 30,
  rank_hist          = TRUE,
  crps               = TRUE,
  crps_decomp        = TRUE,
  tw_crps            = TRUE,
  brier              = TRUE,
  reliability        = TRUE,
  roc                = TRUE,
  econ_val           = TRUE,
  show_progress      = TRUE,
  new_ens_score      = NULL,
  new_ens_prob_score = NULL,
  new_ens_score_opts = list(),
  fcst_model         = NULL,
  ...
) {

  col_names  <- colnames(.fcst)
  parameter  <- rlang::enquo(parameter)
  chr_param  <- rlang::quo_name(parameter)

  comparator <- match.arg(comparator)
  thresholds <- check_thresholds(thresholds, comparator)

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

  if (length(grep(chr_param, col_names)) < 1) {
    stop(paste("No column found for", chr_param), call. = FALSE)
  }

  if (is.function(jitter_fcst)) {
    .fcst <- jitter_fcst(.fcst, jitter_fcst, obs_col = !!parameter)
  }

  if (verify_members) {
    det_summary_scores <- det_verify(
      .fcst, !! parameter, groupings = groupings, circle = circle,
      summary = summary, hexbin = hexbin, num_bins = num_bins,
      show_progress = show_progress
    ) %>%
      purrr::pluck("det_summary_scores")
  } else {
    det_summary_scores <- tibble::tibble()
  }

  num_members <- length(grep("_mbr[[:digit:]]+", colnames(.fcst)))

  if (num_members <= 1) {

    warning("Not enough members to do ensemble verification", immediate. = TRUE, call. = FALSE)
    ens_summary_scores <- tibble::tibble()

  } else if (inherits(.fcst, "harp_ens_probs")) {

    warning("Cannot compute summary scores for probabilities", immediate. = TRUE, call. = FALSE)
    ens_summary_scores <- tibble::tibble()

  } else {

    ens_summary_scores <- list()

    if (summary) {

      ens_summary_scores[["summary"]] <- compute_score(
        groupings,
        .fcst,
        harpCore::member_colnames(.fcst),
        chr_param,
        fcst_model,
        "summary",
        show_progress,
        type = "ens",
        score_opts = list(circle = circle, drop_members = spread_drop_member)
      )

    }

    if (hexbin) {
      ens_summary_scores[["hexbin"]] <- compute_score(
        groupings,
        .fcst,
        harpCore::member_colnames(.fcst),
        chr_param,
        fcst_model,
        "hexbin",
        show_progress,
        type = "ens",
        score_opts = list(num_bins = num_bins)
      )
    }

    if (rank_hist) {
      ens_summary_scores[["rank_histogram"]] <- compute_score(
        groupings,
        .fcst,
        harpCore::member_colnames(.fcst),
        chr_param,
        fcst_model,
        "rank_histogram",
        show_progress,
        type = "ens",
        score_opts = list(circle = circle, drop_members = spread_drop_member)
      )
    }

    if (crps) {
      ens_summary_scores[["crps"]] <- compute_score(
        groupings,
        .fcst,
        harpCore::member_colnames(.fcst),
        chr_param,
        fcst_model,
        "crps",
        show_progress,
        type = "ens",
        score_opts = list(num_ref_members = num_ref_members)
      )
    }

    if (crps_decomp) {
      ens_summary_scores[["crps_decomp"]] <- compute_score(
        groupings,
        .fcst,
        harpCore::member_colnames(.fcst),
        chr_param,
        fcst_model,
        "crps_decomp",
        show_progress,
        type = "ens"
      )
    }

    if (crps && crps_decomp) {
      ens_summary_scores[["crps"]] <- dplyr::select(
        ens_summary_scores[["crps"]], -dplyr::all_of("crps")
      )
    }

    for (new_score in new_ens_score) {
      ens_summary_scores[[new_score]] <- compute_score(
        groupings,
        .fcst,
        harpCore::member_colnames(.fcst),
        chr_param,
        fcst_model,
        new_score,
        show_progress,
        type       = "ens",
        score_opts = new_ens_score_opts
      )
    }

    ens_summary_scores[["meta"]] <- compute_score(
      groupings,
      .fcst,
      harpCore::member_colnames(.fcst),
      chr_param,
      fcst_model,
      "meta",
      show_progress,
      type = "ens"
    )

    ens_summary_scores <- Reduce(
      function(x, y) suppressMessages(dplyr::inner_join(x, y)),
      ens_summary_scores
    )

  }

  if (length(ens_summary_scores) < 1) {
    ens_summary_scores <- tibble::tibble()
  }

  if (!is.null(thresholds) && num_members > 1) {

    ens_threshold_scores <- list()

    if (any(brier, reliability, roc, econ_val)) {
      ens_threshold_scores[["prob_scores"]] <- compute_score(
        groupings,
        .fcst,
        harpCore::member_colnames(.fcst),
        chr_param,
        fcst_model,
        "prob_scores",
        show_progress,
        type = "ens",
        thresholds   = thresholds,
        comparator   = comparator,
        include_low  = include_low,
        include_high = include_high,
        score_opts   = c(
          list(
            tw_crps            = FALSE,
            brier              = brier,
            reliability        = reliability,
            fair_brier         = !is.na(num_ref_members),
            roc                = roc,
            econ_val           = econ_val,
            num_ref_members    = num_ref_members,
            climatology        = climatology,
            prob_breaks        = rel_probs,
            new_ens_prob_score = new_ens_prob_score
          ),
          new_ens_score_opts
        )
      )
    }

    if (tw_crps) {
      ens_threshold_scores[["tw_crps"]] <- compute_score(
        groupings,
        .fcst,
        harpCore::member_colnames(.fcst),
        chr_param,
        fcst_model,
        "tw_crps",
        show_progress,
        type = "ens",
        thresholds   = thresholds,
        comparator   = comparator,
        include_low  = include_low,
        include_high = include_high,
        score_opts   = list(
          tw_crps         = TRUE,
          brier           = FALSE,
          reliability     = FALSE,
          fair_brier      = FALSE,
          roc             = FALSE,
          econ_val        = FALSE,
          num_ref_members = NA
        )
      )
    }


    ens_threshold_scores <- Reduce(
      function(x, y) suppressMessages(dplyr::inner_join(x, y)),
      ens_threshold_scores
    )

  } else {

    ens_threshold_scores <- tibble::tibble()

  }

  if (!is.null(ens_summary_scores)) {
    ens_summary_scores <- dplyr::mutate(
      ens_summary_scores,
      fcst_model = fcst_model, .before = dplyr::everything()
    )
  }
  if (!is.null(ens_threshold_scores)) {
    ens_threshold_scores <- dplyr::mutate(
      ens_threshold_scores,
      fcst_model = fcst_model, .before = dplyr::everything()
    )
  }
  if (!is.null(det_summary_scores)) {
      det_summary_scores <- dplyr::mutate(
      det_summary_scores,
      fcst_model = fcst_model, .before = dplyr::everything()
    )
  }

  res <- list(
    ens_summary_scores   = ens_summary_scores,
    ens_threshold_scores = ens_threshold_scores,
    det_summary_scores   = det_summary_scores
  )

  res <- res[!vapply(res, is.null, logical(1))]

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
ens_verify.harp_list <- function(
  .fcst,
  parameter,
  verify_members     = TRUE,
  thresholds         = NULL,
  comparator         = c("ge", "gt", "le", "lt", "eq", "between", "outside"),
  include_low        = TRUE,
  include_high       = TRUE,
  groupings          = "lead_time",
  summary            = TRUE,
  circle             = NULL,
  rel_probs          = NA,
  num_ref_members    = NA,
  spread_drop_member = NULL,
  jitter_fcst        = NULL,
  climatology        = "sample",
  hexbin             = TRUE,
  num_bins           = 30,
  rank_hist          = TRUE,
  crps               = TRUE,
  crps_decomp        = TRUE,
  tw_crps            = TRUE,
  brier              = TRUE,
  reliability        = TRUE,
  roc                = TRUE,
  econ_val           = TRUE,
  show_progress      = TRUE,
  new_ens_score      = NULL,
  new_ens_prob_score = NULL,
  new_ens_score_opts = list(),
  ...
) {
  parameter   <- rlang::ensym(parameter)

  comparator <- match.arg(comparator)
  threhsolds <- check_thresholds(thresholds, comparator)

  spread_drop_member <- parse_member_drop(spread_drop_member, names(.fcst))

  # if (!is.null(thresholds)) climatology <- get_climatology(
  #   .fcst, !!parameter, thresholds,
  #   comparator, include_low, include_high, climatology
  # )

  list_to_harp_verif(
    purrr::pmap(
      list(.fcst, names(.fcst), spread_drop_member),
      function(x, y, z) ens_verify(
        x, !!parameter, verify_members, thresholds, comparator, include_low,
        include_high, groupings, summary, circle,
        rel_probs, num_ref_members, z, jitter_fcst, climatology,
        hexbin, num_bins, rank_hist, crps, crps_decomp, tw_crps,
        brier, reliability, roc, econ_val, show_progress, new_ens_score,
        new_ens_prob_score, new_ens_score_opts, fcst_model = y
      )
    )
  )


}

# member_colnames <- function(df, drop = NULL) {
#   mbr_cols <- grep("_mbr[0-9]{3}", colnames(df), value = TRUE)
#
#   if (is.null(drop)) {
#     return(mbr_cols)
#   }
#
#   if (is.numeric(drop)) {
#     drop <- paste(
#       paste0("_mbr", formatC(drop, width = 3, flag = "0")),
#       collapse = "|"
#     )
#   }
#
#   grep(drop, colnames(df), value = TRUE)
# }

rms <- function(x) sqrt(mean(x ^ 2))

# Prep data before grouping
prep_ens_summary <- function(.fcst, fcst_col, obs_col, opts, ...) {

  message(
    cli::col_br_yellow("Prepping data for summary scores "), appendLF = FALSE
  )
  if (!is.null(opts$drop_members)) {
    drop_mbr_cols <- harpCore::member_colnames(
      fcst_col, opts$drop_members, invert = TRUE
    )
    drp <- harpCore::ens_stats(
      dplyr::select(.fcst, -dplyr::all_of(drop_mbr_cols)),
      sd           = FALSE,
      var          = TRUE,
      keep_members = FALSE
    )
    drp <- dplyr::mutate(
      drp,
      dropped_members_bias = bias(
        .data[["ens_mean"]], .data[[obs_col]], opts$circle
      ),
      dropped_members_ens_var = .data[["ens_var"]],
      .keep = "unused"
    )
  }

  .fcst <- harpCore::ens_stats(
    .fcst, sd = FALSE, var = TRUE, keep_members = FALSE
  )
  .fcst <- dplyr::mutate(
    .fcst, fcst_bias = bias(.data[["ens_mean"]], .data[[obs_col]], opts$circle)
  )

  if (!is.null(opts$drop_members)) {
    .fcst <- suppressMessages(dplyr::inner_join(.fcst, drp))
  }
  message(cli::col_green(cli::symbol[["tick"]]))
  .fcst
}

prep_ens_crps_decomp <- function(.fcst, fcst_col, obs_col, ...) {
  message(
    cli::col_br_yellow("Prepping data for CRPS decomposition ")
  )
  attr(.fcst, "crps_prep") <- harp_crps_alpha_beta(
    dplyr::select(.fcst, dplyr::all_of(fcst_col)), .fcst[[obs_col]]
  )
  .fcst
}

prep_ens_crps <- function(.fcst, fcst_col, obs_col, opts) {

  message(
    cli::col_br_yellow("Prepping data for CRPS "), appendLF = FALSE
  )

  .fcst <- dplyr::mutate(
    .fcst,
    crps = SpecsVerification::EnsCrps(
      as.matrix(dplyr::pick(fcst_col)), .data[[obs_col]], NA
    ),
    fair_crps = SpecsVerification::EnsCrps(
      as.matrix(dplyr::pick(fcst_col)), .data[[obs_col]], opts$num_ref_members
    )
  )
  message(cli::col_green(cli::symbol[["tick"]]))
  .fcst
}

prep_ens_thresh_data <- function(
  fcst_df, obs_col, threshold, comparator, include_low, include_high, opts
) {
  parameter = obs_col
  if (any(unlist(opts[c("brier", "roc", "econ_val", "reliability")]))) {
    return(ens_probabilities(
      fcst_df, threshold, comparator, include_low, include_high,
      parameter = {{parameter}}
    ))
  }
  if (opts[["tw_crps"]]) {
    return(prep_ens_crps(
      clamp_fcst(
        fcst_df, threshold, comparator, obs_col
      ),
      harpCore::member_colnames(fcst_df),
      obs_col,
      opts
    ))
  }
  fcst_df
}

# tick the progress bar
tick_progress <- function(show_prog, pb_env) {
  if (!show_prog) return()
  cli::cli_progress_update(.envir = pb_env)
}

# Meta data for all scores
compute_ens_meta <- function(grouped_fcst, ...) {
  col_for_len <- setdiff(
    colnames(grouped_fcst), dplyr::group_vars(grouped_fcst)
  )[1]

  n_stns <- function(x) length(unique(x))
  if (is.element("SID", dplyr::group_vars(grouped_fcst))) {
    n_stns <- function(x) 1L
  }

  dplyr::summarise(
    grouped_fcst,
    num_cases    = length(.data[[col_for_len]]),
    num_stations = n_stns(.data[["SID"]])
  )
}

# Summary scores
compute_ens_summary <- function(grouped_fcst, show_prog, pb_env, ...) {
  if (!any(grepl("dropped_members", colnames(grouped_fcst)))) {
    grouped_fcst[["dropped_members_bias"]] <- grouped_fcst[["fcst_bias"]]
    grouped_fcst[["dropped_members_ens_var"]] <- grouped_fcst[["ens_var"]]
  }
  dplyr::summarise(
    grouped_fcst,
    mean_bias              = mean(.data[["fcst_bias"]]),
    stde                   = stats::sd(.data[["fcst_bias"]]),
    rmse                   = rms(.data[["fcst_bias"]]),
    spread                 = sqrt(mean(.data[["ens_var"]])),
    dropped_members_rmse   = rms(.data[["dropped_members_bias"]]),
    dropped_members_spread = sqrt(mean(.data[["dropped_members_ens_var"]]))
  ) %>%
    dplyr::mutate(
      spread_skill_ratio                 = .data[["spread"]] / .data[["rmse"]],
      dropped_members_spread_skill_ratio = .data[["dropped_members_spread"]] /
        .data[["dropped_members_rmse"]]
    )
}

# Hexbin
compute_ens_hexbin <- function(grouped_fcst, show_prog, pb_env, opts, ...) {
  mbr_cols <- harpCore::member_colnames(colnames(grouped_fcst))
  dplyr::summarise(
    grouped_fcst,
    hexbin       = list(
      hexbin_df(
        rep(.data[["obs"]], length(mbr_cols)),
        as.vector(as.matrix(dplyr::pick(mbr_cols))),
        opts$num_bins,
        show_prog,
        pb_env
      )
    ),
    .groups = "drop"
  )
}

# CRPS - decomposed
compute_ens_crps_decomp <- function(grouped_fcst, show_prog, pb_env, ...) {
  sweep_crps(
    dplyr::summarise(
      grouped_fcst,
      crps_list = {
        tick_progress(show_prog, pb_env)
        list(verification::crpsFromAlphaBeta(
          attr(grouped_fcst, "crps_prep")$alpha[dplyr::cur_group_rows(), , drop = FALSE],
          attr(grouped_fcst, "crps_prep")$beta[dplyr::cur_group_rows(), , drop = FALSE],
          attr(grouped_fcst, "crps_prep")$heaviside0[dplyr::cur_group_rows()],
          attr(grouped_fcst, "crps_prep")$heavisideN[dplyr::cur_group_rows()]
        ))
      }
    ),
    "crps_list", FALSE
  )
}

# Quick CRPS (no decomposition) and fair CRPS
compute_ens_crps <- function(grouped_fcst, show_prog, pb_env, ...) {
  dplyr::summarise(
    grouped_fcst,
    crps      = mean(.data[["crps"]]),
    fair_crps = mean(.data[["fair_crps"]])
  )
}

# Rank histogram
compute_ens_rank_histogram <- function(grouped_fcst, show_prog, pb_env, ...) {
  mbr_cols <- harpCore::member_colnames(colnames(grouped_fcst))
  sweep_rank_histogram(
    dplyr::summarise(
      grouped_fcst,
      rank_count = list(
        rankHistogram(.data[["obs"]], dplyr::pick(dplyr::all_of(mbr_cols)))
      )
    )
  )
}



# Probability scores
compute_ens_prob_scores <- function(grouped_fcst, show_prog, pb_env, opts) {

  num_mem <- attr(grouped_fcst, "num_members")
  res <- list()
  i   <- 1

  if (opts$brier || opts$reliability) {

    if (is.character(opts$climatology) && opts$climatology == "sample") {
      climatology <- get_sample_climatology(grouped_fcst)
    } else {
      climatology <- get_climatology(grouped_fcst)
    }

    join_cols <- intersect(colnames(climatology), colnames(grouped_fcst))

    grouped_fcst <- dplyr::rename(
      dplyr::inner_join(grouped_fcst, climatology, by = join_cols),
      bss_ref_climatology = .data$climatology
    )

    sweep_fun <- get_brier_sweep_fun(opts)

    if (show_prog) {
      cli::cli_progress_output(
        cli::col_br_cyan("  Brier Score"), .envir = pb_env
      )
    } else {
      message(cli::col_br_cyan("  Brier Score "), appendLF = FALSE)
    }

    res[[i]] <- sweep_fun(
      dplyr::summarise(
        grouped_fcst,
        brier_output = list(compute_brier_score(
          .data[["fcst_prob"]],
          .data[["obs_prob"]],
          opts[["prob_breaks"]],
          num_mem,
          .data[["bss_ref_climatology"]],
          show_prog,
          pb_env
        ))
      )
    )
    i <- i + 1
    if (!show_prog) {
      message(cli::col_br_green(cli::symbol$tick))
    }

  }

  if (opts$fair_brier) {
    if (show_prog) {
      cli::cli_progress_output(
        cli::col_br_cyan("  Fair Brier Score"), .envir = pb_env
      )
    } else {
      message(cli::col_br_cyan("  Fair Brier Score "), appendLF = FALSE)
    }

    res[[i]] <- dplyr::summarise(
      grouped_fcst,
      fair_brier_score = fair_brier_score(
        .data[["fcst_prob"]],
        .data[["obs_prob"]],
        num_mem,
        opts$num_ref_members,
        show_prog,
        pb_env
      )
    )
    i <- i + 1
    if (!show_prog) {
      message(cli::col_br_green(cli::symbol$tick))
    }
  }

  if (opts$roc) {
    if (show_prog) {
      cli::cli_progress_output(
        cli::col_br_cyan("  ROC"), .envir = pb_env
      )
    } else {
      message(cli::col_br_cyan("  ROC "), appendLF = FALSE)
    }


    res[[i]] <- sweep_roc(dplyr::summarise(
      grouped_fcst,
      roc_output = list(harp_roc(
        .data[["obs_prob"]],
        .data[["fcst_prob"]],
        show_prog = show_prog,
        pb_env = pb_env
      ))
    ))
    i <- i + 1
    if (!show_prog) {
      message(cli::col_br_green(cli::symbol$tick))
    }
  }

  if (opts$econ_val) {
    if (show_prog) {
      cli::cli_progress_output(
        cli::col_br_cyan("  Economic Value"), .envir = pb_env
      )
    } else {
      message(cli::col_br_cyan("  Economic Value "), appendLF = FALSE)
    }


    res[[i]] <- dplyr::summarise(
      grouped_fcst,
      economic_value = list(harp_ecoval(
        .data[["obs_prob"]],
        .data[["fcst_prob"]],
        show_prog = show_prog,
        pb_env = pb_env
      ))
    )
    i <- i + 1
    if (!show_prog) {
      message(cli::col_br_green(cli::symbol$tick))
    }
  }

  # User supplied score functions
  for (new_score in opts$new_ens_prob_score) {
    if (!fun_exists(new_score)) {
      cli::cli_alert_inform("Skipping computation of {new_score}.")
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
        show_prog,
        pb_env,
        opts
      )
    )
    i <- i + 1
    if (!show_prog) {
      message(cli::col_br_green(cli::symbol$tick))
    }
  }

  # metadata
  if (show_prog) {
    cli::cli_progress_output(
      cli::col_br_cyan("  Metadata"), .envir = pb_env
    )
  } else {
    message(cli::col_br_cyan("  Metadata "), appendLF = FALSE)
  }

  res[[i]] <- sweep_thresh_meta(dplyr::summarise(
    grouped_fcst,
    metadata = ens_thresh_metadata(
      .data[["obs_prob"]],
      .data[["fcst_prob"]],
      .data[["SID"]],
      show_prog,
      pb_env
    )
  ))
  if (!show_prog) {
    message(cli::col_br_green(cli::symbol$tick))
  }

  res <- lapply(
    res,
    function(x) {
      x <- x[colnames(x) != "progress"]
      x
    }
  )

  Reduce(
    function(x, y) dplyr::inner_join(
      x, y, by = intersect(colnames(x), colnames(y))
    ),
    res
  )
}

get_brier_sweep_fun <- function(opts) {
  if (opts$brier && opts$reliability) {
    return(sweep_brier_both)
  }
  if (opts$brier && !opts$reliability) {
    return(sweep_brier_brier)
  }
  if (!opts$brier && opts$reliability) {
    return(sweep_brier_reliability)
  }
}

compute_brier_score <- function(
  fcst_prob, obs_prob, prob_breaks, num_mem, bss_ref, show_prog, pb_env
) {
  if (any(is.na(prob_breaks))) {
    if (num_mem > 10) {
      num_mem <- 11
    }
    prob_breaks <- (seq_len(num_mem) - 1) / (num_mem - 1)
  }
  res <- verification::brier(
    obs_prob, fcst_prob, baseline = mean(bss_ref), thresholds = prob_breaks
  )
  res[["bss_ref_climatology"]] <- mean(bss_ref)
  res[["sample_climatology"]]  <- mean(obs_prob)
  tick_progress(show_prog, pb_env)
  res
}

# Fair Brier Score - Equation 8 in Ferro et al (2008): https://doi.org/10.1002/met.45

# FAIR_BS = BS - ((M - m) / (M(m - 1)n)) * sum_over_t(Qt(1 - Qt))

# where M is the reference number of members, m is the number of members,
# n is the sample size of t occurrences and Qt is the probability.

fair_brier_score <- function(
    fcst_prob, obs_prob, m, M, show_prog = F, pb_env = ""
) {

  stopifnot(length(fcst_prob) == length(obs_prob))

  BS <- mean((fcst_prob - obs_prob) ^ 2)

  if (is.na(M)) {
    tick_progress(show_prog, pb_env)
    return(BS)
  }

  n <- length(fcst_prob)

  res <- BS - ((M - m) / (M * (m - 1) * n)) * sum(fcst_prob * (1 - fcst_prob))

  tick_progress(show_prog, pb_env)

  res

}


ens_thresh_metadata <- function(
  obs_prob, fcst_prob, sid, show_prog, pb_env
) {
  if (is.null(obs_prob)) {
    return(NULL)
  }
  res <- list(
    num_cases          = length(obs_prob),
    num_stations       = length(unique(sid)),
    num_cases_total    = sum(
      as.integer(obs_prob) | as.integer(ceiling(fcst_prob))
    ),
    num_cases_observed = sum(as.integer(obs_prob)),
    num_cases_forecast = sum(as.integer(ceiling(fcst_prob)))
  )
  tick_progress(show_prog, pb_env)
  res <- list(res)
}

# threshold weighted crps is the same calculation once the data are clamped
compute_ens_tw_crps <- function(grouped_fcst, show_prog, pb_env, ...) {
  res <- compute_ens_crps(grouped_fcst, show_prog, pb_env)
  colnames(res)[colnames(res) == "crps"] <- "tw_crps"
  res <- res[colnames(res) != "fair_crps"]
  res
}

# Function to prepare data for threshold weighted crps. The values outside
# of the range of interest are clamped to the threshold
clamp_fcst <- function(
  .fcst, threshold, comparator, obs_col
) {
  clamp_fun <- get_clamp_fun(comparator)
  mbr_cols  <- harpCore::member_colnames(.fcst)

  .fcst <- dplyr::mutate(
    .fcst,
    dplyr::across(dplyr::all_of(mbr_cols), ~clamp_fun(.x, threshold))
  )

  if (!missing(obs_col) && !is.null(obs_col)) {
    .fcst[[obs_col]] <- clamp_fun(.fcst[[obs_col]], threshold)
  }

  .fcst[["threshold"]] <- make_threshcol(threshold, comparator, TRUE, TRUE)

  .fcst
}

get_clamp_fun <- function(comp) {
  switch(
    comp,
    "gt"      = ,
    "ge"      = function(x, y) {x[x < y] <- y; x},
    "lt"      = ,
    "le"      = function(x, y) {x[x > y] <- y; x},
    "between" = function(x, y) {
      x[x < min(y)] <- min(y)
      x[x > max(y)] <- max(y)
      x
    },
    cli::cli_abort(
      "tw_crps cannot be computed for {.arg comparator} = \"{comp}\""
    )
  )
}

# Internal function to get climatology for Brier Skill Score
get_climatology <- function(
  .fcst,
  parameter,
  thresholds,
  comparator    = c("ge", "gt", "le", "lt", "eq", "between", "outside"),
  include_low   = TRUE,
  include_high  = TRUE,
  climatology   = "sample"
) {

  comparator <- match.arg(comparator)
  thresholds <- check_thresholds(thresholds, comparator)

  if (inherits(climatology, "data.frame")) {

    if (!all(c("threshold", "climatology") %in% names(climatology))) {
      stop("climatology must at least contain columns named threshold and climatology", call. = FALSE)
    }
    if (is.element("lead_time", names(climatology))) {
      if (!all(.fcst$lead_time %in% climatology$lead_time)) {
        stop("Not all leadtimes for the data exist in climatology", call. = FALSE)
      }
    }
    return(climatology)

  } else if (is.list(climatology)) {

    if (!all(c("eps_model", "member") %in% names(climatology))) {
      stop("When supplying climatology as a list it must have names 'eps_model' and 'member'", call. = FALSE)
    }
    if (!is.character(climatology$eps_model) | length(climatology$eps_model) > 1) {
      stop("When supplying climatology as a list 'eps_model' must be a single string", call. = FALSE)
    }
    if (!is.numeric(climatology$member) | length(climatology$member) > 1) {
      stop("When supplying climatology as a list 'member' must be a single number", call. = FALSE)
    }
    if (!is.element(climatology$eps_model, names(.fcst))) {
      stop("eps_model ", climatology$eps_model, " given in climatology not found in .fcst", call. = FALSE)
    }
    member_name <- paste0(climatology$eps_model, "_mbr", formatC(climatology$member, width = 3, flag = "0"))
    if (!is.element(member_name, names(.fcst[[climatology$eps_model]]))) {
      stop("Member ", climatology$member, " given in climatology not found for ", climatology$eps_model, call. = FALSE)
    }

    member_col   <- rlang::sym(member_name)
    list_element <- which(names(.fcst) == climatology$eps_model)

  } else if (climatology == "sample") {

    if (!missing(parameter)) {
      member_col   <- rlang::enquo(parameter)
    }
    list_element <- 1

  } else {

    stop(
      paste(
        "climatology must be 'sample', a data frame with columns 'threshold' and 'climatology'\n",
        "  or a list with named elements 'eps_model' and 'member'."
      ),
      call. = FALSE
    )

  }

  if (inherits(.fcst, "harp_list")) {
    .fcst <- .fcst[[list_element]]
  }
  if (!inherits(.fcst, "harp_ens_probs")) {
    if (missing(parameter) | missing(thresholds)) {
      stop("parameter and thresholds must be passed as arguments", call. = FALSE)
    } else {
      climatol <- ens_probabilities(
        .fcst, thresholds, comparator, include_low, include_high, !!member_col
      )
    }
  } else {
    climatol <- .fcst
  }

  climatol %>%
    dplyr::group_by(.data$threshold, .data$lead_time) %>%
    dplyr::summarise(climatology = mean(.data$obs_prob))

}

get_sample_climatology <- function(grpd_prob_fcst) {
  dplyr::summarise(
    grpd_prob_fcst, climatology = mean(.data[["obs_prob"]]), .groups = "drop"
  )
}


# Internal function to add forecast attributes to a verification output
add_attributes <- function(.verif, dttm, parameter, stations, groupings) {
  parameter <- rlang::enquo(parameter)

  attr(.verif, "parameter") <- rlang::quo_name(parameter)
  attr(.verif, "dttm") <- dttm
  attr(.verif, "stations") <- stations
  attr(.verif, "group_vars") <- groupings

  .verif
}

# Compare attributes returning a list if they're different and the only value
# if they're the same
compare_attrs <- function(x) {
  if (length(x) < 2) {
    return(x[[1]])
  }
  same <- TRUE
  i <- 1
  while (same && i < length(x)) {
    same <- identical(sort_attr(x[[i]]), sort_attr(x[[(i + 1)]]))
    i <- i + 1
  }
  if (same) {
    return(x[[1]])
  }
  x
}

sort_attr <- function(x) {
  if (is.list(x)) {
    return(lapply(x, sort))
  }
  sort(x)
}

# Parse the fcst_model input
parse_fcst_model <- function(.fcst, fcst_model, caller = rlang::caller_env()) {
  if (!is.null(fcst_model)) {
    message(
      cli::col_green(
        glue::glue("::Computing verification for fcst_model `{fcst_model}`::")
      )
    )
  } else {
    fcst_model <- unique(.fcst[["fcst_model"]])
  }

  fcst_model_err(fcst_model, caller)

  fcst_model
}

# Error message when there is more than one fcst_model in a data frame
fcst_model_err <- function(fcst_model, caller) {
  if (length(fcst_model) == 1) return()

  if (length(fcst_model) < 1) {
    cli::cli_abort(c(
      "No {.var fcst_model} column found in data frame.",
      "x" = "Data frame must have a {.var fcst_model} column.",
      "i" = paste(
        "You can use the {.arg fcst_model} argument to add a {.var fcst_model}",
        "column"
      )
    ), call = caller)

  }

  err <- paste(fcst_model)
  cli::cli_abort(c(
    "More than one {.var fcst_model} found in data frame.",
    "x" = "Data frame has {fcst_model} in the {.var fcst_model} column.",
    "i" = "Split data using e.g. `as_harp_list(split(fcst, fcst$fcst_model)).`"
  ), call = caller)
}

# Convert a list created by a verification method for harp_list to a
# an object of class harp_verif
list_to_harp_verif <- function(.l) {

  res <- list(

    ens_summary_scores = purrr::list_rbind(
      purrr::map(.l, "ens_summary_scores")
    ),

    ens_threshold_scores = purrr::list_rbind(
      purrr::map(.l, "ens_threshold_scores")
    ),

    det_summary_scores = purrr::list_rbind(
      purrr::map(.l, "det_summary_scores")
    ),

    det_threshold_scores = purrr::list_rbind(
      purrr::map(.l, "det_threshold_scores")
    )
  )

  res <- res[vapply(res, function(x) !is.null(x), logical(1))]

  res_attrs <- lapply(.l, attributes)

  param_attr <- compare_attrs(
    lapply(res_attrs, function(x) x[["parameter"]])
  )
  dttm_attr <- compare_attrs(
    lapply(res_attrs, function(x) x[["dttm"]])
  )
  stations_attr <- compare_attrs(
    lapply(res_attrs, function(x) x[["stations"]])
  )
  group_vars_attr <- compare_attrs(
    lapply(res_attrs, function(x) x[["group_vars"]])
  )

  structure(
    res[which(vapply(res, nrow, integer(1)) > 0)],
    class      = "harp_verif",
    parameter  = param_attr,
    dttm       = dttm_attr,
    stations   = stations_attr,
    group_vars = group_vars_attr
  )



}
