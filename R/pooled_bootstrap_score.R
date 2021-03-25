#' Title
#'
#' @param .fcst
#' @param score_function
#' @param parameter
#' @param n
#' @param pooled_by
#' @param groupings
#' @param confidence_interval
#' @param show_progress
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
  min_cases           = 25,
  perfect_scores      = perfect_score(),
  show_progress       = TRUE,
  ...
) {

  warning(
    "pooled_boostrap_score is DEPRECATED.",
    " Use `bootstrap_verify()` with `pool_by = x` instead."
  )

  if (!is.function(score_function)) {
    stop("'score_function' must be a function.", call. = FALSE)
  }

  score_function_name <- as.character(substitute(score_function))

  score_table <- switch(
    score_function_name,
    "ens_crps"             = "ens_summary_scores",
    "ens_spread_and_skill" = "ens_summary_scores",
    "ens_roc"              = "ens_threshold_scores",
    "ens_brier"            = "ens_threshold_scores",
    "ens_verify"           = c("ens_summary_scores", "ens_threshold_scores"),
    "det_verify"           = c("det_summary_scores", "det_threshold_scores"),
    NA
  )

  if (any(is.na(score_table))) {
    stop(paste("pooled_bootstrap_score does not work for", score_function_name), call. = FALSE)
  }

  parameter  <- rlang::enquo(parameter)

  if (!inherits(try(rlang::eval_tidy(parameter), silent = TRUE), "try-error")) {
    parameter  <- rlang::eval_tidy(parameter)
    param_name <- parameter
    parameter  <- rlang::ensym(parameter)
  } else {
    param_name <- rlang::quo_name(parameter)
  }

  if (score_function_name == "ens_crps") {
    .fcst <- bind_crps_vars(.fcst, !!parameter)
  }

  if (score_function_name == "ens_verify") {
    raw_score <- ens_verify(.fcst, !!parameter, groupings = groupings, verify_members = FALSE, ...)
  } else {
    raw_score <- score_function(.fcst, !!parameter, groupings = groupings, ...)
  }

  summary_scores <- unique(unlist(lapply(raw_score[grep("summary", names(raw_score))], colnames)))

  bind_scores <- function(score_list) {
    score_list <- score_list[which(sapply(score_list, function(x) !is.null(x)))]
    score_list <- score_list[which(sapply(score_list, nrow) > 0)]
    score_list <- lapply(score_list, dplyr::select_if, function(x) !is.list(x))
    if (length(score_list) == 1) {
      score_list <- score_list[[1]]
    } else {
      score_list <- Reduce(function(x, y) suppressMessages(dplyr::inner_join(x, y)), score_list)
    }
    score_list
  }

  raw_score <- dplyr::filter(
    bind_scores(raw_score),
    .data[["num_cases"]] >= min_cases
  )

  if (nrow(raw_score) < 1) {
    stop("Not enough cases. Try reducing 'min_cases'.", call. = FALSE)
  }

  pooled_by_sym <- rlang::sym(pooled_by)

  dots <- rlang::dots_list(...)

  if (!is.list(groupings)) groupings <- list(groupings)

  pooled_groupings <- lapply(groupings, function(x) c(pooled_by, x))

  if (score_function_name == "ens_verify") {
    pooled_score <- ens_verify(.fcst, !!parameter, groupings = pooled_groupings, verify_members = FALSE, ...)
  } else {
    pooled_score <- score_function(.fcst, !! parameter, groupings = pooled_groupings, ...)
  }
  pooled_score <- bind_scores(pooled_score)
  if (harpIO:::tidyr_new_interface()) {
    pooled_score <- tidyr::nest(pooled_score, data = -tidyr::one_of(pooled_by))
  } else {
    pooled_score <- pooled_score %>%
      dplyr::group_by(!! pooled_by_sym) %>%
      tidyr::nest()
  }

  groupings <- unique(unlist(groupings))

  replicates <- modelr::bootstrap(pooled_score, n)

  map_strap <- function(.verif, group_cols, pool_cols) {
    if (harpIO:::tidyr_new_interface()) {
      .verif          <- tidyr::unnest(.verif$data[.verif$idx, ], tidyr::one_of("data"))
    } else {
      .verif          <- tidyr::unnest(.verif$data[.verif$idx, ])
    }
    grouping_cols     <- rlang::syms(setdiff(group_cols, pool_cols))
    .verif            <- dplyr::select_if(.verif, function(x) !is.list(x))
    cols_to_summarise <- setdiff(colnames(.verif), c(group_cols, pool_cols))

    res <- .verif %>%
      dplyr::group_by(!!! grouping_cols) %>%
      dplyr::summarise_at(cols_to_summarise, mean)

    if (show_progress) {
      pb$tick()
    }

    res
  }

  if (show_progress) {
    pb <- progress::progress_bar$new(
      format = "  Bootstrapping [:bar] :percent eta: :eta",
      total = n
    )
  }

  if (is.element("thresholds", names(dots))) {
    groupings <- c(groupings, "threshold")
  }

  replicates <- purrr::map(replicates$strap, map_strap, c("mname", groupings), pooled_by)

  model_names <- unique(unlist(purrr::map(replicates, ~ unique(.x$mname))))

  replicates <- lapply(replicates, function(x) split(x, x[["mname"]]))
  replicates <- lapply(
    model_names,
    function(x) lapply(
      replicates,
      function(y) y[[x]][colnames(y[[x]]) != "mname"]
    )
  )
  names(replicates) <- model_names

  if (confidence_interval > 1) confidence_interval <- confidence_interval / 100
  upper_bound <- confidence_interval + (1 - confidence_interval) / 2
  lower_bound <- (1 - confidence_interval) / 2

  quantile_func <- function(df, groupings, qtile, suffix) {
    groupings_sym <- rlang::syms(groupings)
    res <- dplyr::bind_rows(df) %>%
      dplyr::group_by(!!! groupings_sym) %>%
      dplyr::summarise_if(~ !is.list(.), .funs = quantile, qtile, na.rm = TRUE)
    data_cols <- rlang::syms(names(res)[!names(res) %in% groupings])
    dplyr::rename_at(res, dplyr::vars(!!!data_cols), ~ paste0(., "_", suffix))
  }

  confidence_upper <- purrr::map(replicates, quantile_func, groupings, upper_bound, "upper")
  confidence_lower <- purrr::map(replicates, quantile_func, groupings, lower_bound, "lower")

  confidence_limits <- purrr::map2(confidence_lower, confidence_upper, dplyr::inner_join, by = groupings) %>%
    dplyr::bind_rows(.id = "mname")

  confidence_limits <- suppressMessages(dplyr::inner_join(confidence_limits, raw_score))

  # Compute the confidence of differences between forecast models

  gather_function <- function(df, donotgather) {
    gather_cols <- rlang::syms(setdiff(names(df), donotgather))
    tidyr::gather(df, !!!gather_cols, key = "score", value = "value")
  }

  bound_replicates <- purrr::map(replicates, dplyr::bind_rows, .id = "replicate") %>%
    purrr::map(dplyr::select_if, ~ !is.list(.)) %>%
    purrr::map(gather_function, c(groupings, "replicate"))

  raw_score <- gather_function(raw_score, c("mname", groupings))
  raw_score <- split(raw_score, raw_score[["mname"]])
  raw_score <- lapply(raw_score, function(x) x[colnames(x) != "mname"])

  diff_func <- function(score_list) {
    score_list <- lapply(score_list, dplyr::ungroup)
    for (m in 1:length(score_list)) {
      for (n in 1:length(score_list)) {
        if (n != m) {
          col_name <- paste0("diff_", names(score_list)[n])
          col_name_sym <- rlang::sym(col_name)
          score_list[[m]] <- score_list[[m]] %>%
            dplyr::mutate(
              !! col_name := .data$value - score_list[[n]]$value
            )
        }
      }
    }
    score_list
  }

  raw_score        <- diff_func(raw_score)
  bound_replicates <- diff_func(bound_replicates)

  proportion_func <- function(x) {
    prop <- length(which(x > 0)) / length(x)
    ifelse(all(x == 0) | prop >= 0.5, prop, prop - 1)
  }

  summarise_func <- function(df, grp) {
    grp <- rlang::syms(c(grp, "score"))
    df %>%
      dplyr::group_by(!!!grp) %>%
      dplyr::summarise_at(dplyr::vars(dplyr::starts_with("diff_")), dplyr::funs(proportion_func)) %>%
      dplyr::ungroup()
  }

  confidence_of_differences <- purrr::map(bound_replicates, summarise_func, groupings)

  gather_func <- function(score_list, diff_name) {
    diff_name <- rlang::sym(diff_name)
    dplyr::bind_rows(score_list, .id = "fcst_model") %>%
      tidyr::gather(
        dplyr::starts_with("diff_"),
        key   = "ref_model",
        value = !!diff_name
      )
  }

  confidence_of_differences <- gather_func(confidence_of_differences, "pc_diff") %>%
    dplyr::mutate(
      ref_model = gsub("diff_", "", .data$ref_model),
      symbol        = dplyr::case_when(
        pc_diff >= confidence_interval        ~ "+",
        pc_diff <= (-1) * confidence_interval ~ "-",
        TRUE                                  ~ "."
      )
    )

  raw_score <- dplyr::mutate(
    gather_func(raw_score, "diff"),
    ref_model = gsub("diff_", "", .data[["ref_model"]])
  )

  confidence_of_differences <- confidence_of_differences %>%
    dplyr::filter(!is.na(.data$pc_diff)) %>%
    dplyr::mutate(
      y = as.numeric(factor(paste(.data$fcst_model, .data$ref_model)))
    )

  confidence_of_differences <- suppressMessages(dplyr::inner_join(
    confidence_of_differences, raw_score
  )) %>%
    dplyr::filter(.data[["fcst_model"]] != .data[["ref_model"]]) %>%
    dplyr::mutate(ref_value = .data[["value"]] - .data[["diff"]])

  if (is.element("threshold", colnames(confidence_of_differences))) {
    confidence_of_differences[["threshold"]][confidence_of_differences[["score"]] %in% summary_scores] <- NA
    confidence_of_differences <- dplyr::distinct(confidence_of_differences)
  }

  if (is.element("threshold", colnames(confidence_limits))) {
    reg_exp        <- paste(paste0("^", summary_scores), collapse = "|")
    summary_scores <- grep(
      "threshold",
      grep(
        reg_exp,
        colnames(confidence_limits),
        value = TRUE
      ),
      value  = TRUE,
      invert = TRUE
    )
    confidence_limits <- gather_function(confidence_limits, c("mname", groupings))
    confidence_limits[["threshold"]][confidence_limits[["score"]] %in% summary_scores] <- NA
    confidence_limits <- dplyr::distinct(confidence_limits) %>%
      tidyr::spread(key = .data[["score"]], value = .data[["value"]]) %>%
      dplyr::ungroup()
  }

  unwanted_scores <- paste(
    c(
      "climatology",
      "uncertainty",
      "std_error",
      "degrees_of_freedom"
    ),
    collapse = "|"
  )

  confidence_limits <- confidence_limits[
    grep(
      unwanted_scores,
      colnames(confidence_limits),
      invert = TRUE
    )
  ] %>%
    dplyr::mutate(parameter = param_name)

  confidence_of_differences <- dplyr::filter(
    confidence_of_differences,
    !grepl(unwanted_scores, .data[["score"]])
  ) %>%
    dplyr::mutate(
      better = as.numeric(abs(.data[["value"]] - perfect_scores[.data[["score"]]]) <
        abs(.data[["ref_value"]] - perfect_scores[.data[["score"]]])) * 2 - 1,
      parameter = param_name
    )

  new_harp_bootstrap(
    list(
      confidence_limits         = confidence_limits,
      confidence_of_differences = confidence_of_differences
    )
  )



}
