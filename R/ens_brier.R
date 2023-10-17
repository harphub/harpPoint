#' Brier score and its decomposition for an ensemble.
#'
#' @param .fcst A \code{harp_fcst} object with tables that have a column for
#'   observations, or a single forecast table.
#' @param parameter The name of the column for the observed data.
#' @param thresholds A numeric vector of thresholds for which to compute the
#'   Brier Score.
#' @param groupings The groups for which to compute the ensemble mean and
#'   spread. See \link[dplyr]{group_by} for more information of how grouping
#'   works.
#' @param climatology The climatology to use for the Brier Skill Score. Can be
#'   "sample" for the sample climatology (the default), a named list with
#'   elements eps_model and member to use a member of an eps model in the
#'   harp_fcst object for the climatology, or a data frame with columns for
#'   threshold and climatology and also optionally leadtime.
#' @param rel_probs Probabilities to use for reliability diagrams. Set to NA
#'   (the default) to select automatically.
#' @param num_ref_members The number of members for which to compute the fair
#'   Brier score.
#' @param keep_score \code{ens_brier} computes the Brier Score and its
#'   components, as well as reliability. If you only want to output the Brier
#'   score and its decomposition set to "brier", to only keep the reliability
#'   information set to "reliability". To keep both, set to "both". The default
#'   behaviour is "both".
#' @param show_progress Logical - whether to show a progress bar. The default is
#' `TRUE`
#' @param ... Reserved for methods.
#'
#' @return A data frame with data grouped for the \code{groupings} column(s) and
#'   columns for \code{brier_score}, \code{brier_skill_score} and the
#'   deomposition of the brier score - \code{brier_score_reliability},
#'   \code{brier_score_resolution} and \code{brier_score_uncertainty}.
#' @export
#'
#' @examples
ens_brier <- function(
  .fcst,
  parameter,
  thresholds,
  groupings       = "lead_time",
  climatology     = "sample",
  rel_probs       = NA,
  num_ref_members = NA,
  keep_score      = c("both", "brier", "reliability"),
  show_progress   = TRUE,
  ...
) {
  keep_score <- match.arg(keep_score)
  # Set progress bar to false for batch running
  if (!interactive()) show_progress <- FALSE
  UseMethod("ens_brier")
}

#' @param fcst_model The name of the forecast model to use in the `fcst_model`
#'  column of the output. If the function is dispatched on a `harp_list`
#'  object, the names of the `harp_list` are automatically used.
#' @export
ens_brier.harp_ens_point_df <- function(
  .fcst,
  parameter,
  thresholds,
  groupings       = "lead_time",
  climatology     = "sample",
  rel_probs       = NA,
  num_ref_members = NA,
  keep_score      = "both",
  show_progress   = TRUE,
  fcst_model      = NULL,
  ...
) {

  if (missing(parameter)) {
    cli::cli_abort(
      "Argument {.arg parameter} is missing with no default."
    )
  }

  ens_brier(
    ens_probabilities(.fcst, thresholds, {{parameter}}),
    parameter       = {{parameter}},
    groupings       = groupings,
    climatology     = climatology,
    rel_probs       = NA,
    num_ref_members = NA,
    keep_score      = keep_score,
    show_progress   = show_progress,
    fcst_model      = fcst_model
  )
}

#' @export
ens_brier.harp_ens_probs <- function(
  .fcst,
  parameter       = NA,
  groupings       = "lead_time",
  climatology     = "sample",
  rel_probs       = NA,
  num_ref_members = NA,
  keep_score      = "both",
  show_progress   = TRUE,
  fcst_model      = NULL,
  ...
) {

  if (!is.list(groupings)) {
    groupings <- list(groupings)
  }

  groupings <- purrr::map(groupings, union, "threshold")

  fcst_model <- parse_fcst_model(.fcst, fcst_model)
  .fcst[["fcst_model"]] <- fcst_model

  sweep_function <- get(paste0("sweep_brier_", keep_score))

  num_members <- attr(.fcst, "num_members")

  if (inherits(climatology, "data.frame")) {
    mandatory_cols  <- c("threshold", "climatology")
    check_clim_cols <- mandatory_cols %in% colnames(climatology)
    missing_cols    <- mandatory_cols[!check_clim_cols]
    if (length(missing_cols) > 0) {
      cli::cli_abort(c(
        "{.arg climatology} does not contain the correct columns:",
        "x" = "{.arg climatology} is missing {missing_cols} column{?s}."
      ))
    }
    join_cols <- intersect(colnames(climatology), colnames(.fcst))
    # cli::cli_inform(c(
    #   "i" = "Climatology for Brier Skill Score uses column{?s}: {join_cols}"
    # ))
    .fcst <- dplyr::inner_join(.fcst, climatology, by = join_cols) %>%
      dplyr::rename(bss_ref_climatology = .data$climatology)
  }

  brier_function <- function(df, prob_breaks, num_mem) {
    if (any(is.na(prob_breaks))) {
      if (num_mem > 10) {
        num_mem <- 11
      }
      prob_breaks <- (seq_len(num_mem) - 1) / (num_mem - 1)
    }
    if (is.element("bss_ref_climatology", names(df))) {
      res <- verification::brier(
        df$obs_prob, df$fcst_prob, baseline = mean(df$bss_ref_climatology),
        thresholds = prob_breaks
      )
    } else {
      res <- verification::brier(
        df$obs_prob, df$fcst_prob, thresholds = prob_breaks
      )
    }
    res
  }

  fair_brier_function <- function(f_prob, o_prob, num_mem, num_ref_mem) {
    res <- fair_brier_score(f_prob, o_prob, num_mem, num_ref_mem)
    res
  }


  compute_brier <- function(compute_group, fcst_df) {
    compute_group_sym <- rlang::syms(compute_group)
    class(fcst_df) <- class(fcst_df)[class(fcst_df) != "harp_ens_probs"]
    if (harpIO:::tidyr_new_interface()) {
      fcst_df <- tidyr::nest(fcst_df, grouped_fcst = -tidyr::one_of(compute_group))
    } else {
    fcst_df <- fcst_df %>%
      dplyr::group_by(!!! compute_group_sym) %>%
      tidyr::nest(.key = "grouped_fcst")
    }
    group_names <- glue::glue_collapse(compute_group, sep = ", ", last = " & ")
    score_text <- glue::glue("Brier Score for {group_names}")
    if (show_progress) {
      pb_brier <- cli::col_blue(score_text)
      pb_fair_brier <- cli::col_blue(glue::glue("Fair ", score_text))
      score_text <- cli::col_blue(score_text)
    } else {
      pb_brier <- FALSE
      pb_fair_brier <- FALSE
      cat(cli::col_blue(score_text))
      score_text <- ""
    }

    fcst_df <- fcst_df %>%
      dplyr::transmute(
        !!! compute_group_sym,
        brier_output = purrr::map(
          .data$grouped_fcst,
          brier_function,
          rel_probs,
          num_members,
          .progress = pb_brier
        ),
        fair_brier_score = purrr::map_dbl(
          .data$grouped_fcst,
          ~ fair_brier_function(
            .x$fcst_prob,
            .x$obs_prob,
            num_members,
            num_ref_members
          ),
          .progress = pb_fair_brier
        ),
        sample_climatology = purrr::map_dbl(
          .data$grouped_fcst,
          ~ sum(.x$obs_prob) / nrow(.x)
        ),
        bss_ref_climatology = purrr::map_dbl(
          .data$grouped_fcst,
          ~ {
            if (is.element("bss_ref_climatology", colnames(.x))) {
              mean(.x$bss_ref_climatology)
            } else {
              NA_real_
            }
          }
        ),
        num_stations = {
          if (is.element("SID", compute_group)) {
            1L
          } else {
            purrr::map_int(.data[["grouped_fcst"]], ~length(unique(.x[["SID"]])))
          }
        },
        num_cases_total = purrr::map_int(
          .data$grouped_fcst,
          ~ sum(as.integer(.x$obs_prob) | as.integer(ceiling(.x$fcst_prob)))
        ),
        num_cases_observed = purrr::map_int(
          .data$grouped_fcst,
          ~ sum(as.integer(.x$obs_prob))
        ),
        num_cases_forecast = purrr::map_int(
          .data$grouped_fcst,
          ~ sum(as.integer(ceiling(.x$fcst_prob)))
        )
      ) %>%
      sweep_function()

    cat(score_text, cli::col_green(cli::symbol[["tick"]]), "\n")
    fcst_df

  }

  res <- list()
  res[["ens_threshold_scores"]] <- purrr::map(
    groupings, compute_brier, .fcst
  ) %>%
    purrr::list_rbind() %>%
    fill_group_na(groupings) %>%
    dplyr::mutate(fcst_model = fcst_model, .before = dplyr::everything())

  structure(
    add_attributes(
      res[which(vapply(res, nrow, numeric(1)) > 0)],
      harpCore::unique_fcst_dttm(.fcst),
      {{parameter}},
      harpCore::unique_stations(.fcst),
      groupings
    ),
    class = "harp_verif"
  )

}

#' @export
ens_brier.harp_list <- function(
  .fcst,
  parameter,
  thresholds,
  groupings       = "lead_time",
  climatology     = "sample",
  rel_probs       = NA,
  num_ref_members = NA,
  keep_score      = "both",
  show_progress   = TRUE,
  ...
) {

  parameter   <- rlang::enquo(parameter)
  if (!inherits(try(rlang::eval_tidy(parameter), silent = TRUE), "try-error")) {
    if (is.character(rlang::eval_tidy(parameter))) {
      parameter <- rlang::eval_tidy(parameter)
      parameter <- rlang::ensym(parameter)
    }
  }

  if (missing(thresholds)) {
    thresholds <- NULL
  }
  climatology <- get_climatology(.fcst, !! parameter, thresholds, climatology)

  get_num_members <- function(df) {
    if (inherits(df, "harp_ens_probs")) {
      attr(df, "num_members")
    } else {
      length(grep("_mbr[[:digit:]]+", colnames(df)))
    }
  }

  .fcst_num_members <- purrr::map(.fcst, get_num_members)

  list_to_harp_verif(
    purrr::imap(
      .fcst,
      ~ens_brier(
        .x,
        parameter       = !! parameter,
        thresholds      = thresholds,
        groupings       = groupings,
        climatology     = climatology,
        rel_probs       = rel_probs,
        num_ref_members = num_ref_members,
        keep_score      = keep_score,
        show_progress   = show_progress,
        fcst_model      = .y
      )
    )
  )
}



# Sweep functions
sweep_brier_brier <- function(brier_df) {
  brier_col <- rlang::sym("brier_output")
  brier_df %>%
    dplyr::mutate(
      brier_score             = purrr::map_dbl(!! brier_col, "bs"),
      brier_skill_score       = purrr::map_dbl(!! brier_col, "ss"),
      brier_score_reliability = purrr::map_dbl(!! brier_col, "bs.reliability"),
      brier_score_resolution  = purrr::map_dbl(!! brier_col, "bs.resol"),
      brier_score_uncertainty = purrr::map_dbl(!! brier_col, "bs.uncert")
    ) %>%
    dplyr::select(-!! brier_col)
}

sweep_brier_reliability <- function(brier_df) {
  brier_col <- rlang::sym("brier_output")
  brier_df <- brier_df %>%
    dplyr::mutate(
      forecast_probability = purrr::map(!! brier_col, "y.i"),
      observed_frequency   = purrr::map(!! brier_col, "obar.i"),
      proportion_occurred  = purrr::map(!! brier_col, "prob.y")
    ) %>%
    dplyr::select(-!! brier_col)

  if (harpIO:::tidyr_new_interface()) {
    tidyr::unnest(
      brier_df,
      tidyr::one_of(c("forecast_probability", "observed_frequency", "proportion_occurred"))
    ) %>%
      tidyr::nest(
        reliability = tidyr::one_of(
          c("forecast_probability", "observed_frequency", "proportion_occurred")
        )
      )
  } else {
    tidyr::unnest(
      brier_df,
      .data$forecast_probability,
      .data$observed_frequency,
      .data$proportion_occurred) %>%
    tidyr::nest(
      .data$forecast_probability,
      .data$observed_frequency,
      .data$proportion_occurred,
      .key = "reliability"
    )
  }

}

sweep_brier_both <- function(ens_threshold_df) {

  brier_output_col <- rlang::sym("brier_output")

  ens_threshold_df <- ens_threshold_df %>%
    dplyr::mutate(
      brier_score             = purrr::map_dbl(!! brier_output_col, "bs"),
      brier_skill_score       = purrr::map_dbl(!! brier_output_col, "ss"),
      brier_score_reliability = purrr::map_dbl(!! brier_output_col, "bs.reliability"),
      brier_score_resolution  = purrr::map_dbl(!! brier_output_col, "bs.resol"),
      brier_score_uncertainty = purrr::map_dbl(!! brier_output_col, "bs.uncert"),
      forecast_probability    = purrr::map(!! brier_output_col, "y.i"),
      observed_frequency      = purrr::map(!! brier_output_col, "obar.i"),
      proportion_occurred     = purrr::map(!! brier_output_col, "prob.y")
    ) %>%
    dplyr::select(-!! brier_output_col)
  if (harpIO:::tidyr_new_interface()) {
    tidyr::unnest(
      ens_threshold_df,
      tidyr::one_of(c("forecast_probability", "observed_frequency", "proportion_occurred"))
    ) %>%
      tidyr::nest(
        reliability = tidyr::one_of(
          c("forecast_probability", "observed_frequency", "proportion_occurred")
        )
      )
  } else {
    tidyr::unnest(
      ens_threshold_df,
      .data$forecast_probability,
      .data$observed_frequency,
      .data$proportion_occurred) %>%
    tidyr::nest(
      .data$forecast_probability,
      .data$observed_frequency,
      .data$proportion_occurred,
      .key = "reliability"
    )

  }

}


# Fair Brier Score - Equation 8 in Ferro et al (2008): https://doi.org/10.1002/met.45

# FAIR_BS = BS - ((M - m) / (M(m - 1)n)) * sum_over_t(Qt(1 - Qt))

# where M is the reference number of members, m is the number of members,
# n is the sample size of t occurences and Qt is the probability is the probability

fair_brier_score <- function(fcst_prob, obs_prob, m, M) {

  stopifnot(length(fcst_prob) == length(obs_prob))

  BS <- mean((fcst_prob - obs_prob) ^ 2)

  if (is.na(M)) {
    return(BS)
  }

  n <- length(fcst_prob)

  res <- BS - ((M - m) / (M * (m - 1) * n)) * sum(fcst_prob * (1 - fcst_prob))

  res

}
