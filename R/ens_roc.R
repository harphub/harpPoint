#' Relative Operating Characteristics for an ensemble.
#'
#' @param .fcst A \code{harp_fcst} object with tables that have a column for
#'   observations, or a single forecast table.
#' @param parameter The name of the column for the observed data.
#' @param thresholds A numeric vector of thresholds for which to compute the
#'   ROC.
#' @param groupings The groups for which to compute the ROC. See
#'   \link[dplyr]{group_by} for more information of how grouping works.
#'
#' @return A data frame with data grouped for the \code{groupings} column(s) and
#'   a nested column for the ROC with each row containing a data frame with
#'   columns: \code{prob} for the forecast probability bin, \code{HR} for the
#'   hit rate and \code{FAR} for the false alarm rate. Use \link[tidyr]{unnest}
#'   to unnest to the nested column.
#' @export
#'
#' @examples
ens_roc <- function(.fcst, parameter, thresholds, groupings = "leadtime", show_progress = FALSE) {
  UseMethod("ens_roc")
}

#' @export
ens_roc.default <- function(.fcst, parameter, thresholds, groupings = "leadtime", show_progress = FALSE) {

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
    roc_progress <- progress::progress_bar$new(format = "  ROC [:bar] :percent eta: :eta", total = progress_total)
  }

  roc_function <- function(obs_vector, prob_vector, prog_bar) {
    res <- harp_roc(obs_vector, prob_vector)
    if (prog_bar) {
      roc_progress$tick()
    }
    res
  }

  compute_roc <- function(compute_group, fcst_df) {
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
        roc_output = purrr::map(
          .data$grouped_fcst,
          ~ roc_function(.x$obs_prob, .x$fcst_prob, prog_bar = show_progress)
        )
      ) %>%
      sweep_roc()
  }

  purrr::map_dfr(groupings, compute_roc, .fcst) %>%
    fill_group_na(groupings)

}

#' @export
ens_roc.harp_fcst <- function(.fcst, parameter, thresholds, groupings = "leadtime", show_progress = FALSE) {
  parameter <- rlang::enquo(parameter)
  list(
    ens_summary_scores = NULL,
    ens_threshold_scores = purrr::map(.fcst, ens_roc, !! parameter, thresholds, groupings, show_progress) %>%
      dplyr::bind_rows(.id = "mname")
  )%>%
    add_attributes(.fcst, !! parameter)
}

sweep_roc <- function(roc_df) {
  roc_col <- rlang::quo(roc_output)
  roc_df %>%
    dplyr::mutate(
      roc      = purrr::map(!! roc_col, "roc_data"),
      roc_area = purrr::map_dbl(!! roc_col, "roc_area")
    ) %>%
    dplyr::select(- !! roc_col)
}
