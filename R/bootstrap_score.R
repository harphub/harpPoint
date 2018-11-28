#' Bootstrap a score
#'
#' Compute the confidence intervals of verification scores using a bootstrap
#' method. If there is more than one forecast the confidence for the differences
#' between forecasts is also computed.
#'
#' @param .fcst A \code{harp_fcst} object.
#' @param score The name of the verification function to bootstrap for.
#' @param parameter The parameter that gives the name of the observations
#'   columns.
#' @param n Th number of bootstrap replicants.
#' @param groupings The groups to compute verification scores for. The default
#'   is leadtime.
#' @param ... Other arguments to the score function. e.g. thresholds.
#'
#' @return
#' @export
#'
#' @examples
bootstrap_score <- function(.fcst, score_function, parameter, n, groupings = "leadtime", confidence_interval = 0.95, ...) {

  parameter <- rlang::enquo(parameter)

  bootstrap_func <- function(df, n) {
    modelr::bootstrap(df, n)
  }

  replicants <- purrr::map(.fcst, bootstrap_func, n)

  map_strap <- function(.fcst, score_func, parameter, groupings, ...) {
    parameter     <- rlang::enquo(parameter)
    groupings_sym <- rlang::syms(groupings)
    res <- score_func(.fcst$data[.fcst$idx, ], !! parameter, groupings = groupings, ...) %>%
      dplyr::arrange(!!! groupings_sym)
    pb$tick()
    res
  }

  map_replicants <- function(.fcst, score_func, parameter, groupings, ...){
    parameter <- rlang::enquo(parameter)
    purrr::map(.fcst$strap, map_strap, score_func, !! parameter, groupings = groupings, ...)
  }

  num_calls_to_score_func <- sum(purrr::map_int(replicants, nrow))
  pb <- progress::progress_bar$new(format = "  Bootstrapping [:bar] :percent eta: :eta", total = num_calls_to_score_func)
  replicants <- purrr::map(replicants, map_replicants, score_function, !! parameter, groupings = groupings, ...)

  # Compute the confidence limits

  if (confidence_interval > 1) confidence_interval <- confidence_interval / 100
  upper_bound <- confidence_interval + (1 - confidence_interval) / 2
  lower_bound <- (1 - confidence_interval) / 2

  quantile_func <- function(df, groupings, qtile, suffix) {
    groupings_sym <- rlang::syms(groupings)
    res <- dplyr::bind_rows(df) %>%
      dplyr::group_by(!!! groupings_sym) %>%
      dplyr::summarise_all(.funs = quantile, qtile)
    data_cols <- rlang::syms(names(res)[!names(res) %in% groupings])
    dplyr::rename_at(res, dplyr::vars(!!!data_cols), ~ paste0(., "_", suffix))
  }

  confidence_upper <- purrr::map(replicants, quantile_func, groupings, upper_bound, "upper")
  confidence_lower <- purrr::map(replicants, quantile_func, groupings, lower_bound, "lower")

  confidence_limits <- purrr::map2(confidence_lower, confidence_upper, dplyr::inner_join, by = groupings) %>%
    dplyr::bind_rows(.id = "mname")

  # Compute the confidence of differences between forecast models

  bound_replicants <- purrr::map(replicants, dplyr::bind_rows, .id = "replicant") %>%
    purrr::map(tidyr::gather, -.data$replicant, -.data$leadtime, key = "score", value = "value")

  for (m in 1:length(bound_replicants)) {
    for (n in 1:length(bound_replicants)) {
      if (n != m) {
        col_name <- paste0("diff_", names(bound_replicants)[n])
        col_name_sym <- rlang::sym(col_name)
        bound_replicants[[m]] <- bound_replicants[[m]] %>%
          dplyr::mutate(
            !! col_name := value - bound_replicants[[n]]$value
          )
      }
    }
  }

  proportion_func <- function(x) {
    prop <- length(which(x > 0)) / length(x)
    ifelse(prop >= 0.5, prop, prop - 1)
  }

  summarise_func <- function(df) {
    df %>%
      dplyr::group_by(.data$leadtime, .data$score) %>%
      dplyr::summarise_at(dplyr::vars(dplyr::starts_with("diff_")), dplyr::funs(proportion_func)) %>%
      dplyr::ungroup()
  }

  confidence_of_differences <- purrr::map(bound_replicants, summarise_func)

  list(confidence_limits = confidence_limits, confidence_of_differences = confidence_of_differences)

}
