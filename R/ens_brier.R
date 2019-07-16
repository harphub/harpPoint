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
#' @param num_ref_members The number of members for which to compute the fair
#'   Brier score.
#' @param keep_score \code{ens_brier} computes the Brier Score and its
#'   components, as well as reliability. If you only want to output the Brier
#'   score and its decomposition set to "brier", to only keep the reliability
#'   information set to "reliability". To keep both, set to "both". The default
#'   behaviour is "both".
#' @param ... Used internally depending on the class of the input.
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
  groupings = "leadtime",
  climatology = "sample",
  num_ref_members = NA,
  keep_score = c("both", "brier", "reliability"),
  ...
) {
  keep_score <- match.arg(keep_score)
  UseMethod("ens_brier")
}

#' @export
ens_brier.default <- function(
  .fcst,
  parameter,
  thresholds,
  groupings = "leadtime",
  climatology = "sample",
  num_members = NA,
  num_ref_members = NA,
  keep_score = "both"
) {

  if (!is.null(groupings)) {
    groupings  <- rlang::syms(union("threshold", groupings))
  }

  sweep_function <- get(paste0("sweep_brier_", keep_score))

  if (!inherits(.fcst, "harp_ens_probs")) {

    parameter  <- rlang::enquo(parameter)
    if (rlang::quo_is_missing(parameter) | rlang::quo_is_null(parameter)) {
      stop ("parameter must be passed as an argument if probabilities have not been derived.", call. = FALSE)
    }
    if (missing(thresholds) | is.null(thresholds)) {
      stop ("thresholds must be passed as an argument if probabilities have not been derived.", call. = FALSE)
    }
    .fcst       <- ens_probabilities(.fcst, thresholds, !! parameter)
    num_members <- attr(.fcst, "num_members")

  } else {

  }

  if (inherits(climatology, "data.frame")) {
    if (all(c("leadtime", "threshold") %in% names(climatology))) {
      join_cols <- c("leadtime", "threshold")
    } else {
      join_cols <- "threshold"
    }
    .fcst <- dplyr::inner_join(.fcst, climatology, by = join_cols) %>%
      dplyr::rename(bss_ref_climatology = .data$climatology)
  }

  brier_function <- function(df) {
    if (is.element("bss_ref_climatology", names(df))) {
      verification::brier(df$obs_prob, df$fcst_prob, baseline = unique(df$bss_ref_climatology))
    } else {
      verification::brier(df$obs_prob, df$fcst_prob)
    }
  }


  .fcst %>%
    dplyr::group_by(!!! groupings) %>%
    tidyr::nest(.key = "grouped_fcst") %>%
    dplyr::transmute(
      !!! groupings,
      brier_output = purrr::map(
        .data$grouped_fcst,
        brier_function
      ),
      fair_brier_score = purrr::map_dbl(
        .data$grouped_fcst,
        ~ fair_brier_score(
          .x$fcst_prob,
          .x$obs_prob,
          num_members,
          num_ref_members
        )
      ),
      sample_climatology = purrr::map_dbl(
        .data$grouped_fcst,
        ~ sum(.x$obs_prob) / nrow(.x)
      ),
      bss_ref_climatology = purrr::map_dbl(
        .data$grouped_fcst,
        ~ mean(.x$bss_ref_climatology)
      ),
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
}

#' @export
ens_brier.harp_fcst <- function(
  .fcst,
  parameter,
  thresholds,
  groupings = "leadtime",
  climatology = "sample",
  num_ref_members = NA,
  keep_score = "both",
  ...
) {
  if (missing(parameter)) {
    parameter <- rlang::quo(NULL)
  } else {
    parameter   <- rlang::enquo(parameter)
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

  list(
    ens_summary_scores   = NULL,
    ens_threshold_scores = purrr::map2(
      .fcst,
      .fcst_num_members,
      ~ ens_brier(
        .x,
        parameter       = !! parameter,
        thresholds      = thresholds,
        groupings       = groupings,
        climatology     = climatology,
        num_members     = .y,
        num_ref_members = num_ref_members,
        keep_score      = keep_score
      )
    ) %>%
    dplyr::bind_rows(.id = "mname")
  ) %>%
    add_attributes(.fcst, !! parameter)
}



# Sweep functions
sweep_brier_brier <- function(brier_df) {
  brier_col <- rlang::quo(brier_output)
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
  brier_col <- rlang::quo(brier_output)
  brier_df %>%
    dplyr::mutate(
      forecast_probability = purrr::map(!! brier_col, "y.i"),
      observed_frequency   = purrr::map(!! brier_col, "obar.i"),
      proportion_occurred  = purrr::map(!! brier_col, "prob.y")
    ) %>%
    dplyr::select(-!! brier_col) %>%
    tidyr::unnest() %>%
    tidyr::nest(
      .data$forecast_probability,
      .data$observed_frequency,
      .data$proportion_occurred,
      .key = "reliability"
    )
}

sweep_brier_both <- function(ens_threshold_df) {

  brier_output_col <- rlang::sym("brier_output")

  ens_threshold_df %>%
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
    dplyr::select(-!! brier_output_col) %>%
    tidyr::unnest(
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

  BS - ((M - m) / (M * (m - 1) * n)) * sum(fcst_prob * (1 - fcst_prob))

}
