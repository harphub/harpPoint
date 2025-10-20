# Internal functions to clean up outputs from verification functions and
# arrange them properly for data frames

# CRPS
sweep_crps <- function(crps_df, crps_col, keep_full_output) {
  crps_col <- rlang::sym(crps_col)
  crps_df  <- crps_df %>%
    dplyr::mutate(
      crps             = purrr::map_dbl(!! crps_col, "CRPS"),
      crps_potential   = purrr::map_dbl(!! crps_col, "CRPSpot"),
      crps_reliability = purrr::map_dbl(!! crps_col, "Reli")
    )
  if (!keep_full_output) {
    crps_df <- dplyr::select(crps_df, -!!crps_col)
  }
  crps_df
}

# Rank Histogram
sweep_rank_histogram <- function(rank_hist_df) {
  nest_cols <- c("rank", "relative_rank", "rank_count")
  rank_hist_df <- rank_hist_df %>%
    dplyr::mutate(
      rank          = purrr::map(.data$rank_count, ~ seq(1, length(.x))),
      relative_rank = purrr::map(.data$rank, ~ (.x - min(.x)) / (max(.x) - min(.x)))
    )
  if (harpIO:::tidyr_new_interface()) {
    tidyr::unnest(rank_hist_df, tidyr::one_of(nest_cols)) %>%
      tidyr::nest(rank_histogram = tidyr::one_of(nest_cols))
  } else {
    nest_cols <- rlang::syms(nest_cols)
    tidyr::unnest(rank_hist_df) %>%
      tidyr::nest(!!! nest_cols, .key = "rank_histogram")
  }
}

# Brier score and reliability - depends on which one or both selected
sweep_brier_brier <- function(brier_df) {
  brier_col <- rlang::sym("brier_output")
  brier_df <- brier_df %>%
    dplyr::mutate(
      brier_score             = purrr::map_dbl(!!brier_col, "bs"),
      brier_skill_score       = purrr::map_dbl(!!brier_col, "ss"),
      brier_score_reliability = purrr::map_dbl(!!brier_col, "bs.reliability"),
      brier_score_resolution  = purrr::map_dbl(!!brier_col, "bs.resol"),
      brier_score_uncertainty = purrr::map_dbl(!!brier_col, "bs.uncert")
    )
  if (!is.element("bss_ref_climatology", colnames(brier_df))) {
    brier_df <- dplyr::mutate(
      brier_df,
      bss_ref_climatology   = purrr::map_dbl(
        !!brier_col, "bss_ref_climatology"
      )
    )
  }
  if (!is.element("sample_climatology", colnames(brier_df))) {
    brier_df <- dplyr::mutate(
      brier_df,
      bss_ref_climatology   = purrr::map_dbl(
        !!brier_col, "sample_climatology"
      )
    )
  }
  dplyr::select(brier_df, -!!brier_col)
}

sweep_brier_reliability <- function(brier_df) {
  brier_col <- rlang::sym("brier_output")
  brier_df <- brier_df %>%
    dplyr::mutate(
      forecast_probability = purrr::map(!!brier_col, "y.i"),
      observed_frequency   = purrr::map(!!brier_col, "obar.i"),
      proportion_occurred  = purrr::map(!!brier_col, "prob.y")
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
      brier_score             = purrr::map_dbl(!!brier_output_col, "bs"),
      brier_skill_score       = purrr::map_dbl(!!brier_output_col, "ss"),
      brier_score_reliability = purrr::map_dbl(!!brier_output_col, "bs.reliability"),
      brier_score_resolution  = purrr::map_dbl(!!brier_output_col, "bs.resol"),
      brier_score_uncertainty = purrr::map_dbl(!!brier_output_col, "bs.uncert"),
      forecast_probability    = purrr::map(!!brier_output_col, "y.i"),
      observed_frequency      = purrr::map(!!brier_output_col, "obar.i"),
      proportion_occurred     = purrr::map(!!brier_output_col, "prob.y")
    )

  if (!is.element("bss_ref_climatology", colnames(ens_threshold_df))) {
    ens_threshold_df <- dplyr::mutate(
      ens_threshold_df,
      bss_ref_climatology   = purrr::map_dbl(
        !!brier_output_col, "bss_ref_climatology"
      )
    )
  }
  if (!is.element("sample_climatology", colnames(ens_threshold_df))) {
    ens_threshold_df <- dplyr::mutate(
      ens_threshold_df,
      bss_ref_climatology   = purrr::map_dbl(
        !!brier_output_col, "sample_climatology"
      )
    )
  }

  ens_threshold_df <- dplyr::select(ens_threshold_df, -!!brier_output_col)

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

# ROC
sweep_roc <- function(roc_df) {
  roc_col <- rlang::quo(roc_output)
  roc_df %>%
    dplyr::mutate(
      roc      = purrr::map(!! roc_col, "roc_data"),
      roc_area = purrr::map_dbl(!! roc_col, "roc_area")
    ) %>%
    dplyr::select(- !! roc_col)
}

# Metadata
sweep_thresh_meta <- function(df) {
  if (!is.element("metadata", colnames(df))) {
    return(df)
  }
  df <- dplyr::mutate(
    df,
    num_cases            = purrr::map_int(.data[["metadata"]], "num_cases"),
    num_stations         = purrr::map_int(.data[["metadata"]], "num_stations"),
    num_cases_total      = purrr::map_int(
      .data[["metadata"]], "num_cases_total"
    ),
    num_cases_observed   = purrr::map_int(
      .data[["metadata"]], "num_cases_observed"
    ),
    num_cases_forecasted = purrr::map_int(
      .data[["metadata"]], "num_cases_forecast"
    )
  )

  df[grep("metadata", colnames(df), value = TRUE, invert = TRUE)]
}

# Deterministic





