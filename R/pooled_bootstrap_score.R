#' Title
#'
#' @param .fcst
#' @param score_function
#' @param parameter
#' @param n
#' @param pooled_by
#' @param groupings
#' @param confidence_interval
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
pooled_bootstrap_score <- function(
  .fcst,
  score_function,
  parameter,
  n,
  pooled_by           = "fcdate",
  groupings           = "leadtime",
  confidence_interval = 0.95,
  ...
) {

  if (!is.function(score_function)) {
    stop("'score_function' must be a function.", call. = FALSE)
  }

  score_function_name <- as.character(substitute(score_function))

  score_table <- switch(
    score_function_name,
    "ens_crps"             = "ens_summary_scores",
    "ens_spread_and_skill" = "ens_summary_scores",
    "ens_roc"              = "ens_threshold_scores",
    NA
  )

  if (is.na(score_table)) {
    stop(paste("pooled_bootstrap_score does not work for", score_function_name), call. = FALSE)
  }

  parameter <- rlang::enquo(parameter)

  pooled_by_sym <- rlang::sym(pooled_by)

  dots <- rlang::dots_list(...)

  if(is.element("thresholds", names(dots))) {
      groupings <- c(groupings, "threshold")
  }

  pooled_groupings <- c(pooled_by, groupings)

  pooled_score <- score_function(.fcst, !! parameter, groupings = pooled_groupings, ...)
  pooled_score <- pooled_score[[score_table]] %>%
    dplyr::group_by(!! pooled_by_sym) %>%
    tidyr::nest()

  replicates <- modelr::bootstrap(pooled_score, n)

  map_strap <-function(.verif, group_cols, pool_cols) {
    .verif            <- tidyr::unnest(.verif$data[.verif$idx, ])
    grouping_cols     <- rlang::syms(setdiff(group_cols, pool_cols))
    .verif            <- dplyr::select_if(.verif, function(x) !is.list(x))
    cols_to_summarise <- setdiff(colnames(.verif), group_cols)

    res <- .verif %>%
      dplyr::group_by(!!! grouping_cols) %>%
      dplyr::summarise_at(cols_to_summarise, mean)

    pb$tick()
    res
  }

  pb <- progress::progress_bar$new(
    format = "  Bootstrapping [:bar] :percent eta: :eta",
    total = n
  )

  replicates <- purrr::map(replicates$strap, map_strap, c("mname", pooled_groupings), pooled_by)

  model_names <- unique(unlist(purrr::map(replicates, ~ unique(.x$mname))))

  replicates_to_model_list <- function(df, filter_val, filter_col) {

    keep_cols  <- setdiff(names(df), filter_col)
    filter_col <- rlang::sym(filter_col)

    df %>%
      dplyr::ungroup() %>%
      dplyr::filter(
        !!filter_col == filter_val
      ) %>%
      dplyr::select_at(keep_cols)
  }

  replicates <- purrr::map(model_names, ~ purrr::map(replicates, replicates_to_model_list, .x, "mname")) %>%
    purrr::set_names(model_names)

  # Compute the confidence limits

  if (confidence_interval > 1) confidence_interval <- confidence_interval / 100
  upper_bound <- confidence_interval + (1 - confidence_interval) / 2
  lower_bound <- (1 - confidence_interval) / 2

  quantile_func <- function(df, groupings, qtile, suffix) {
    groupings_sym <- rlang::syms(groupings)
    res <- dplyr::bind_rows(df) %>%
      dplyr::group_by(!!! groupings_sym) %>%
      dplyr::summarise_if(~ !is.list(.), .funs = quantile, qtile)
    data_cols <- rlang::syms(names(res)[!names(res) %in% groupings])
    dplyr::rename_at(res, dplyr::vars(!!!data_cols), ~ paste0(., "_", suffix))
  }

  confidence_upper <- purrr::map(replicates, quantile_func, groupings, upper_bound, "upper")
  confidence_lower <- purrr::map(replicates, quantile_func, groupings, lower_bound, "lower")

  confidence_limits <- purrr::map2(confidence_lower, confidence_upper, dplyr::inner_join, by = groupings) %>%
    dplyr::bind_rows(.id = "mname")

  # Compute the confidence of differences between forecast models

  gather_function <- function(df, donotgather) {
    gather_cols <- rlang::syms(setdiff(names(df), donotgather))
    tidyr::gather(df, !!!gather_cols, key = "score", value = "value")
  }

  bound_replicates <- purrr::map(replicates, dplyr::bind_rows, .id = "replicate") %>%
    purrr::map(dplyr::select_if, ~ !is.list(.)) %>%
    purrr::map(gather_function, c(groupings, "replicate"))

  for (m in 1:length(bound_replicates)) {
    for (n in 1:length(bound_replicates)) {
      if (n != m) {
        col_name <- paste0("diff_", names(bound_replicates)[n])
        col_name_sym <- rlang::sym(col_name)
        bound_replicates[[m]] <- bound_replicates[[m]] %>%
          dplyr::mutate(
            !! col_name := value - bound_replicates[[n]]$value
          )
      }
    }
  }

  proportion_func <- function(x) {
    prop <- length(which(x > 0)) / length(x)
    ifelse(all(x == 0) | prop >= 0.5, prop, prop - 1)
  }

  summarise_func <- function(df) {
    df %>%
      dplyr::group_by(.data$leadtime, .data$score) %>%
      dplyr::summarise_at(dplyr::vars(dplyr::starts_with("diff_")), dplyr::funs(proportion_func)) %>%
      dplyr::ungroup()
  }

  confidence_of_differences <- purrr::map(bound_replicates, summarise_func)
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
