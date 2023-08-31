#' Rank histogram for an ensemble.
#'
#' The rank histogram is computed as columns in a \code{harp_fcst} object.
#' Typically the scores are aggregated over lead time, but other grouping
#' variables can be chosen.
#'
#' @param .fcst A \code{harp_fcst} object with tables that have a column for
#'   observations, or a single forecast table.
#' @param parameter The name of the column for the observed data.
#' @param groupings The groups for which to compute the ensemble mean and
#'   spread. See \link[dplyr]{group_by} for more information of how grouping
#'   works.
#' @param jitter_fcst A function to perturb the forecast values by. This is used
#'   to account for observation error in the rank histogram. For other
#'   statistics it is likely to make little difference since it is expected that
#'   the observations will have a mean error of zero.
#'
#' @return An object of the same format as the inputs but with data grouped for
#'   the \code{groupings} column(s) and columns for \code{rank} and
#'   \code{rank_count} that are nested together in a column with the name
#'   \code{rank_histogram}.
#' @export
#'
#' @examples
ens_rank_histogram <- function(
  .fcst,
  parameter,
  groupings     = "lead_time",
  jitter_fcst   = NULL,
  show_progress = TRUE,
  ...
) {
  UseMethod("ens_rank_histogram")
}

#' @export
ens_rank_histogram.harp_ens_point_df <- function(
  .fcst,
  parameter,
  groupings     = "lead_time",
  jitter_fcst   = NULL,
  show_progress = TRUE,
  fcst_model    = NULL,
  ...
) {

  if (!is.list(groupings)) {
    groupings <- list(groupings)
  }

  fcst_model <- parse_fcst_model(.fcst, fcst_model)
  .fcst[["fcst_model"]] <- fcst_model

  fcst_model_err(fcst_model)

  col_names  <- colnames(.fcst)
  parameter  <- rlang::enquo(parameter)
  chr_param  <- rlang::quo_name(parameter)
  if (length(grep(chr_param, col_names)) < 1) {
    stop(paste("No column found for", chr_param), call. = FALSE)
  }

  if (is.function(jitter_fcst)) {
    .fcst <- dplyr::mutate_at(.fcst,  dplyr::vars(dplyr::contains("_mbr")), ~ purrr::map_dbl(., jitter_fcst))
  }

  compute_rank_hist <- function(compute_group, fcst_df) {
    fcst_df <- group_without_threshold(fcst_df, compute_group)
    group_vars <- dplyr::group_vars(fcst_df)
    group_names <- glue::glue_collapse(group_vars, sep = ", ", last = " & ")
    score_text <- cli::col_blue(glue::glue("Rank histogram for {group_names}"))
    if (show_progress) {
      pb_name <- score_text
    } else {
      pb_name <- FALSE
      cat(score_text)
    }
    if (harpIO:::tidyr_new_interface()) {
      fcst_df <- tidyr::nest(fcst_df, grouped_fcst = -tidyr::one_of(compute_group)) %>%
        dplyr::ungroup()
    } else {
      fcst_df <- tidyr::nest(fcst_df, .key = "grouped_fcst")
    }
    fcst_df <- fcst_df %>%
      dplyr::mutate(
        rank_count = purrr::map(
          .data$grouped_fcst, harp_rank_hist, !! parameter, .progress = pb_name
        )
      ) %>%
      dplyr::select(-.data[["grouped_fcst"]]) %>%
      sweep_rank_histogram()

    cat(score_text, cli::col_green(cli::symbol[["tick"]]), "\n")
    fcst_df
  }

  res <- list()
  res[["ens_summary_scores"]] <- purrr::map(
    groupings, compute_rank_hist, .fcst
  ) %>%
    purrr::list_rbind() %>%
    fill_group_na(groupings) %>%
    dplyr::mutate(fcst_model = fcst_model, .before = dplyr::everything())

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
ens_rank_histogram.harp_list <- function(
  .fcst,
  parameter,
  groupings     = "lead_time",
  jitter_fcst   = NULL,
  show_progress = TRUE,
  ...
) {

  parameter   <- rlang::enquo(parameter)
  if (!inherits(try(rlang::eval_tidy(parameter), silent = TRUE), "try-error")) {
    if (is.character(rlang::eval_tidy(parameter))) {
      parameter <- rlang::eval_tidy(parameter)
      parameter <- rlang::ensym(parameter)
    }
  }

  list_to_harp_verif(
    purrr::imap(
      .fcst,
      ~ens_rank_histogram(
        .x, !! parameter, groupings, jitter_fcst, show_progress, fcst_model = .y
      )
    )
  )

}

# Internal function to return nicely formatted column for ens_rank_histogram.
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
