#' Bootstrap a score
#'
#' Compute the confidence intervals of verification scores using a bootstrap
#' method. If there is more than one forecast the confidence for the differences
#' between forecasts is also computed.
#'
#' @param .fcst A \code{harp_fcst} object.
#' @param score_function The name of the verification function to bootstrap for.
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

  fun_name  <- as.character(substitute(score_function))
  if (fun_name == "ens_crps") {
    .fcst <- bind_crps_vars(.fcst, !!parameter)
  }

  dots <- rlang::dots_list(...)
  if(is.element("thresholds", names(dots))) {
      groupings <- c(groupings, "threshold")
  }

  bootstrap_func <- function(df, n) {
    modelr::bootstrap(df, n)
  }

  replicants <- purrr::map(.fcst, bootstrap_func, n)

  map_strap <- function(.fcst, score_func, parameter, groupings, ...) {
    parameter     <- rlang::enquo(parameter)
    groupings_sym <- rlang::syms(unique(unlist(groupings)))
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
    groupings_sym <- rlang::syms(unique(unlist(groupings)))
    res <- dplyr::bind_rows(df) %>%
      dplyr::group_by(!!! groupings_sym) %>%
      dplyr::summarise_if(~ !is.list(.), .funs = quantile, qtile)
    data_cols <- rlang::syms(names(res)[!names(res) %in% unique(unlist(groupings))])
    dplyr::rename_at(res, dplyr::vars(!!!data_cols), ~ paste0(., "_", suffix))
  }

  confidence_upper <- purrr::map(replicants, quantile_func, groupings, upper_bound, "upper")
  confidence_lower <- purrr::map(replicants, quantile_func, groupings, lower_bound, "lower")

  confidence_limits <- purrr::map2(confidence_lower, confidence_upper, dplyr::inner_join, by = unique(unlist(groupings))) %>%
    dplyr::bind_rows(.id = "mname")

  # Compute the confidence of differences between forecast models

  gather_function <- function(df, donotgather) {
    gather_cols <- rlang::syms(setdiff(names(df), donotgather))
    tidyr::gather(df, !!!gather_cols, key = "score", value = "value")
  }

  bound_replicants <- purrr::map(replicants, dplyr::bind_rows, .id = "replicant") %>%
    purrr::map(dplyr::select_if, ~ !is.list(.)) %>%
    purrr::map(gather_function, c(unique(unlist(groupings)), "replicant"))

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

  summarise_func <- function(df, groupings) {
    groupings_sym <- rlang::syms(c(unique(unlist(groupings)), "score"))
    df %>%
      dplyr::group_by(!!! groupings_sym) %>%
      dplyr::summarise_at(dplyr::vars(dplyr::starts_with("diff_")), dplyr::funs(proportion_func)) %>%
      dplyr::ungroup()
  }

  confidence_of_differences <- purrr::map(bound_replicants, summarise_func, groupings)
  confidence_of_differences <- confidence_of_differences %>%
    dplyr::bind_rows(.id = "master_fcst") %>%
    tidyr::gather(
      dplyr::starts_with("diff_"),
      key   = "compared_with",
      value = "pc_diff"
    ) %>%
    dplyr::mutate(
      compared_with = gsub("diff_", "", .data$compared_with),
      symbol        = dplyr::case_when(
        pc_diff >= confidence_interval        ~ "+",
        pc_diff <= (-1) * confidence_interval ~ "-",
        TRUE                                  ~ "."
      )
    )

  confidence_of_differences <- confidence_of_differences %>%
    dplyr::filter(!is.na(.data$pc_diff)) %>%
    dplyr::mutate(
      y = as.numeric(factor(paste(.data$master_fcst, .data$compared_with)))
    )

  list(confidence_limits = confidence_limits, confidence_of_differences = confidence_of_differences)

}
