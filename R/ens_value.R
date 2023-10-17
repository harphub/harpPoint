#' Economic value for an ensemble.
#'
#' @param .fcst A \code{harp_fcst} object with tables that have a column for
#'   observations, or a single forecast table.
#' @param parameter The name of the column for the observed data.
#' @param thresholds A numeric vector of thresholds for which to compute the
#'   economic value.
#' @param groupings The groups for which to compute the economic value. See
#'   \link[dplyr]{group_by} for more information of how grouping works.
#' @param show_progress Logical - whether to show a progress bar. The default is
#' `TRUE`
#'
#' @return A data frame with data grouped for the \code{groupings} column(s) and
#'   a nested column for the economic value with each row containing a data
#'   frame with columns: \code{cl} for cost loss, and \code{value} for the
#'   economic value. Use \link[tidyr]{unnest} to unnest to the nested column.
#' @export
#'
#' @examples
ens_value <- function(
  .fcst,
  parameter,
  thresholds,
  groupings     = "lead_time",
  show_progress = TRUE,
  ...
) {
  # Set progress bar to false for batch running
  if (!interactive()) show_progress <- FALSE
  UseMethod("ens_value")
}

#' @param fcst_model The name of the forecast model to use in the `fcst_model`
#'  column of the output. If the function is dispatched on a `harp_list`
#'  object, the names of the `harp_list` are automatically used.
#' @export
ens_value.harp_ens_point_df <- function(
  .fcst,
  parameter,
  thresholds,
  groupings     = "lead_time",
  show_progress = TRUE,
  fcst_model    = NULL,
  ...
) {

  if (missing(parameter)) {
    cli::cli_abort(
      "Argument {.arg parameter} is missing with no default."
    )
  }

  ens_value(
    ens_probabilities(.fcst, thresholds, {{parameter}}),
    parameter     = {{parameter}},
    thresholds    = thresholds,
    groupings     = groupings,
    show_progress = show_progress,
    fcst_model    = fcst_model
  )
}

#' @export
ens_value.harp_ens_probs <- function(
  .fcst,
  parameter,
  thresholds,
  groupings     = "lead_time",
  show_progress = TRUE,
  fcst_model    = NULL,
  ...
) {

  if (!is.list(groupings)) {
    groupings <- list(groupings)
  }

  groupings <- purrr::map(groupings, union, "threshold")

  fcst_model <- parse_fcst_model(.fcst, fcst_model)
  .fcst[["fcst_model"]] <- fcst_model

  parameter  <- rlang::enquo(parameter)

  if (!inherits(.fcst, "harp_ens_probs")) {
    .fcst   <- ens_probabilities(.fcst, thresholds, !! parameter)
  }


  value_function <- function(obs_vector, prob_vector, prog_bar) {
    res <- harp_ecoval(obs_vector, prob_vector)
    if (prog_bar) {
      value_progress$tick()
    }
    res
  }

  compute_value <- function(compute_group, fcst_df) {
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
    score_text <- cli::col_blue(glue::glue("Econ Value for {group_names}"))
    if (show_progress) {
      pb_name <- score_text
    } else {
      pb_name <- FALSE
      cat(score_text)
      score_text <- ""
    }

    fcst_df <- fcst_df %>%
      dplyr::transmute(
        !!! compute_group_sym,
        num_stations = {
          if (is.element("SID", compute_group)) {
            1L
          } else {
            purrr::map_int(.data[["grouped_fcst"]], ~length(unique(.x[["SID"]])))
          }
        },
        economic_value = purrr::map(
          .data$grouped_fcst,
          ~harp_ecoval(.x$obs_prob, .x$fcst_prob),
          .progress = pb_name
        )
      )

    cat(score_text, cli::col_green(cli::symbol[["tick"]]), "\n")
    fcst_df

  }

  res <- list()
  res[["ens_threshold_scores"]] <- purrr::map(
    groupings, compute_value, .fcst
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
ens_value.harp_list <- function(
  .fcst,
  parameter,
  thresholds,
  groupings     = "lead_time",
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
      ~ens_value(
        .x, !!parameter, thresholds, groupings, show_progress, fcst_model = .y
      )
    )
  )
}

