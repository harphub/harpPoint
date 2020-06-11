#' Economic value for an ensemble.
#'
#' @param .fcst A \code{harp_fcst} object with tables that have a column for
#'   observations, or a single forecast table.
#' @param parameter The name of the column for the observed data.
#' @param thresholds A numeric vector of thresholds for which to compute the
#'   economic value.
#' @param groupings The groups for which to compute the economic value. See
#'   \link[dplyr]{group_by} for more information of how grouping works.
#'
#' @return A data frame with data grouped for the \code{groupings} column(s) and
#'   a nested column for the economic value with each row containing a data
#'   frame with columns: \code{cl} for cost loss, and \code{value} for the
#'   economic value. Use \link[tidyr]{unnest} to unnest to the nested column.
#' @export
#'
#' @examples
ens_value <- function(.fcst, parameter, thresholds, groupings = "leadtime", show_progress = FALSE) {
  UseMethod("ens_value")
}

#' @export
ens_value.default <- function(.fcst, parameter, thresholds, groupings = "leadtime", show_progress = FALSE) {

  if (!is.list(groupings)) {
    groupings <- list(groupings)
  }

  groupings <- purrr::map(groupings, union, "threshold")

  parameter  <- rlang::enquo(parameter)

  if (!inherits(.fcst, "harp_ens_probs")) {
    .fcst   <- ens_probabilities(.fcst, thresholds, !! parameter)
  }

  if (show_progress) {
    progress_total <- sum(
      sapply(
        groupings,
        function(x) length(dplyr::group_rows(dplyr::group_by(.fcst, !!! rlang::syms(intersect(x, names(.fcst))))))
      )
    )
    value_progress <- progress::progress_bar$new(format = "  Value [:bar] :percent eta: :eta", total = progress_total)
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
    fcst_df %>%
      dplyr::transmute(
        !!! compute_group_sym,
        economic_value = purrr::map(
          .data$grouped_fcst,
          ~ value_function(.x$obs_prob, .x$fcst_prob, prog_bar = show_progress)
        )
      )
  }

  purrr::map_dfr(groupings, compute_value, .fcst) %>%
    fill_group_na(groupings)

}

#' @export
ens_value.harp_fcst <- function(.fcst, parameter, thresholds, groupings = "leadtime", show_progress = FALSE) {

  parameter   <- rlang::enquo(parameter)
  if (!inherits(try(rlang::eval_tidy(parameter), silent = TRUE), "try-error")) {
    if (is.character(rlang::eval_tidy(parameter))) {
      parameter <- rlang::eval_tidy(parameter)
      parameter <- rlang::ensym(parameter)
    }
  }

  list(
    ens_summary_scores = NULL,
    ens_threshold_scores = purrr::map(.fcst, ens_value, !! parameter, thresholds, groupings, show_progress) %>%
      dplyr::bind_rows(.id = "mname")
  ) %>%
    add_attributes(.fcst, !! parameter)
}

