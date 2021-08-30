#' Bootstrap a verification function
#'
#' \code{bootstrap_verify} is used to compute verification scores with
#' confidence intervals. if more than one \code{fcst_model} exists in the input
#' \code{harp_fcst} object, the statistical significance of the differences in
#' verification scores between \code{fcst_model}s is computed. The statistical
#' testing is done using the bootstrap method whereby the scores are computed
#' repeatedly for random samples of the input data.
#'
#' For data that are auto-correlated a block bootstrap may be used, whereby data
#' are pooled into groups in which the serial dependencies are maintained.
#' Rather than sampling individual data points randomly, pools of data points
#' are sampled randomly. The pools are taken from the column passed to the
#' \code{pool_by} argument. To use an overlapping block bootstrap a data frame
#' should be passed to \code{pool_by}, with one column that is common to the
#' \code{harp_fcst} object input and a column named "pool" that labels what
#' pool a row is in. This ensures that the correct number of overlapping pools
#' are used in each bootstrap replicate. \link{make_bootstrap_pools} can be used
#' to get a data frame of overlapping pools.
#'
#' Bootstrapping can be quite slow since many replicates are computed. In order
#' to speed the process up, \code{bootstrap_verify} also works in parallel
#' whereby replicates are computed by individual cores in parallel rather than
#' in serial. This can be achieved by setting \code{parallel = TRUE}. The
#' default behaviour is to use all cores, but the number of cores can be set by
#' the \code{num_cores} argument.
#'
#' @param .fcst A \code{harp_fcst} object with a column for observations.
#' @param verif_func The \code{harpPoint} verification function to bootstrap.
#' @param obs_col The observations column in the \code{harp_fcst} object. Must
#'   be unquoted.
#' @param n The number of bootstrap replicates.
#' @param groupings The groups for which to compute the scores. See
#'   \link[dplyr]{group_by} for more information of how grouping works.
#' @param pool_by For a block bootstrap, the quoted column name to use to pool
#'   the data into blocks. For overlapping blocks this should be a data frame
#'   with a column that is common to the \code{harp_fcst} object input and a
#'   column named "pool" for which pool the data belong to. See Details.
#' @param conf The confidence interval to compute.
#' @param min_cases The minimum number of cases required in a group. For block
#'   bootstrapping this is the minimum number of blocks.
#' @param perfect_scores The values for that each score has to be a perfect
#'   score.
#' @param parallel Set to TRUE to use parallel processing for the bootstrapping.
#'   Requires the parallel package.
#' @param num_cores If parallel = TRUE, the number of cores to use in the
#'   parallel processing. If NULL, the number of cores detected by
#'   \code{parallel::detectCores()} are used.
#' @param show_progress Logical. Set to TRUE to show a progress bar. This
#'   feature is not available if \code{parallel = TRUE}
#' @param ... Other arguments to \code{verif_func}
#'
#' @return A harp_point_verif object with extra columns for upper and lower
#'   confidence bounds of scores and the percent of replicates that are "better"
#'   where there are more than one \code{fcst_model}s in the input
#'   \code{harp_fcst_object}
#' @export
#'
#' @examples
bootstrap_verify <- function(
  .fcst,
  verif_func,
  obs_col,
  n,
  groupings      = "leadtime",
  pool_by        = NULL,
  conf           = 0.95,
  min_cases      = 4,
  perfect_scores = perfect_score(),
  parallel       = FALSE,
  num_cores      = NULL,
  show_progress  = TRUE,
  ...
) {

  # Check that verif_func is a function
  if (!is.function(verif_func)) {
    stop("`verif_func` must be a function.")
  }

  .fcst <- common_cases(.fcst)

  q_lower <- (1 - conf) / 2
  q_upper <- conf + q_lower
  grps    <- c("mname", groupings)

  # Set up parallel processing
  if (parallel) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      stop("Please install the `parallel` package for `parallel = TRUE`.")
    }
    if (is.null(num_cores)) {
      num_cores <- parallel::detectCores()
    } else {
      stopifnot(is.numeric(num_cores))
      num_cores <- min(num_cores, parallel::detectCores())
    }
    show_progress <- FALSE
    message("Bootstrapping on ", num_cores, " cores.")
  }

  # Function to call function to compute a replicate for unpooled data
  call_verif <- function(
    rep, .fcst, verif_func, obs_col, grp, pool_by, min_cases, ...
  ) {

    res <- mutate_list(
      sample_verif(
        .fcst, !!rlang::enquo(obs_col), verif_func, grp, pool_by, min_cases, ...
      ),
      replicate = rep
    )

    if (show_progress) pb$tick()

    res

  }

  # Add coefficient columns for CRPS
  if (grepl("ens_verify|ens_crps", suppressWarnings(methods(verif_func)[1]))) {
    .fcst <- bind_crps_vars(.fcst, !!rlang::enquo(obs_col))
  }

  # Initialize progress bar
  if (show_progress) {
    pb <- progress::progress_bar$new(
      format = " bootstrapping [:bar] :percent ETA: :eta",
      total  = n
    )
  }

  # Compute replicates

  if (parallel) {

    replicates <- bind_point_verif(
      parallel::mclapply(
        1:n, call_verif, .fcst, verif_func,
        !!rlang::enquo(obs_col), groupings, pool_by, min_cases, ...,
        mc.cores = num_cores
      )
    )

  } else {

    replicates <- bind_point_verif(
      lapply(
        1:n, call_verif, .fcst, verif_func,
        !!rlang::enquo(obs_col), groupings, pool_by, min_cases, ...
      )
    )
  }

  # Compute confidences from replicates
  if (length(.fcst) == 1) {

    lapply(replicates, calc_confidence, groupings, conf)


  } else {

    list_names <- names(replicates)
    list_attrs <- attributes(replicates)

    res <- lapply(
      replicates, calc_diff_confidence, names(.fcst),
      groupings, perfect_scores, conf
    )

    names(res)      <- list_names
    attributes(res) <- list_attrs

    res

  }

}


# Function to select random rows / pools and compute scores
# for one bootstrap replicate
sample_verif <- function(
  .fcst, obs_col, verif_func, grp, pool_by, min_cases, ...
) {

  grp_sym <- rlang::syms(grp)

  if (!is.null(pool_by)) {

    if (is.data.frame(pool_by)) { # Pools passed as data frame

      if (!is.element("pool", colnames(pool_by))) {
        stop(
          "If `pool_by` is a data frame it must have a column named `pool`.",
          call. = FALSE
        )
      }

      # Determine column(s) that define pools in fcst and check
      pool_cols <- colnames(pool_by[colnames(pool_by) != "pool"])
      if (!identical(
        pool_cols,
        intersect(pool_cols, Reduce(union, lapply(.fcst, colnames)))
      )) {
        stop(
          "If `pool_by` is a data frame it must have at least one column\n",
          "that has the same name as a column in `.fcst`.",
          call. = FALSE
        )
      }

      # Find number of pools -> number of unique values in pool columns /
      #  the maximum size of a pool
      max_pool_size <- max(
        dplyr::group_by(pool_by, .data[["pool"]]) %>%
          dplyr::summarise(count = dplyr::n()) %>%
          dplyr::pull(.data[["count"]])
      )

      num_pools <- round(
        nrow(dplyr::group_nest(pool_by, !!!rlang::syms(pool_cols))) /
          max_pool_size
      )

      fcst_grps <- purrr::map_dfr(
        .fcst, dplyr::select, !!!rlang::syms(c(grp, pool_cols))
      ) %>%
        dplyr::distinct() %>%
        dplyr::group_nest(!!!grp_sym)

      sample_pools <- function(pool_by, num_pools) {

        pool_samples <- sample(pool_by[["pool"]], num_pools, replace = TRUE)

        dplyr::inner_join(
          pool_by,
          data.frame(pool = pool_samples),
          by = "pool"
        )

      }

      pool_by <- dplyr::mutate(
        fcst_grps,
        sample = purrr::map(
          1:nrow(fcst_grps), ~sample_pools(pool_by, num_pools)
        ),
        data = purrr::map2(
          .data[["data"]], .data[["sample"]],
          dplyr::inner_join, by = pool_cols
        )
      ) %>%
        dplyr::select(-.data[["sample"]]) %>%
        tidyr::unnest(.data[["data"]])


      .fcst <- lapply(.fcst, dplyr::inner_join, pool_by, by = c(grp, pool_cols))

    } else { # pools passed as column name

      nest_sym <- rlang::syms(c(grp, pool_by))
      .fcst     <- lapply(.fcst, dplyr::group_nest, !!!nest_sym)

    }

  }

  if (!is.data.frame(pool_by)) { # No pools or pools passed as a column

    row_numbers <- dplyr::mutate(
      .fcst[[1]],
      row_num = dplyr::row_number()
    ) %>%
      dplyr::group_by(!!!grp_sym) %>%
      dplyr::slice_sample(prop = 1, replace = TRUE) %>%
      dplyr::pull(.data[["row_num"]])

    .fcst <- purrr::map(.fcst, dplyr::slice, row_numbers)
    # counts <- purrr::map(
    #   .fcst,
    #   ~dplyr::summarise(
    #     dplyr::group_by(.x, !!!grp_sym),
    #     count = dplyr::n()
    #   )
    # )
    #
    # stopifnot(length(unique(counts)) == 1)
    #
    # counts <- dplyr::filter(counts[[1]], count >= min_cases)
    # stopifnot(nrow(counts) > 0)
    # counts <- dplyr::mutate(
    #   counts,
    #   rows = purrr::map(count, ~sample(1:.x, replace = TRUE))
    # )
    #
    # .fcst <- purrr::map(
    #   .fcst,
    #   ~#tidyr::unnest(
    #     dplyr::select(
    #       dplyr::mutate(
    #         dplyr::inner_join(dplyr::group_nest(.x, !!!grp_sym), counts, by = grp),
    #         data = purrr::map2(.data[["data"]], .data[["rows"]], ~.x[.y, ])
    #       ),
    #       -.data[["rows"]], -.data[["count"]]
    #     ),
    #     .data[["data"]]
    #   #)
    # )
    #
    # .fcst <- purrr::map(.fcst, tidyr::unnest, .data[["data"]])

  }

  if (!is.null(pool_by) && !is.data.frame(pool_by)) {
    # Unnest pooled data when pools passed as a column
    .fcst <- lapply(.fcst, tidyr::unnest, .data[["data"]])
  }

  # Make sure we have a harp_fcst object and call verification
  .fcst <- structure(.fcst, class = "harp_fcst")

  if (grepl("ens_verify", suppressWarnings(methods(verif_func)[1]))) {

    res <- suppressMessages(ens_verify(
      .fcst, !!rlang::enquo(obs_col), groupings = grp,
      verify_members = FALSE, show_progress = FALSE, ...
    ))

  } else {

    res <- verif_func(
      .fcst, !!rlang::enquo(obs_col), groupings = grp,
      show_progress = FALSE, ...
    )

  }

  # Remove empty score elements from output list
  res_names <- names(res)
  used_elements <- which(
    sapply(res, function(x) !is.null(x) && nrow(x) > 0)
  )
  res <- unname(res)
  res_attrs <- attributes(res)
  res <- res[used_elements]
  attributes(res) <- res_attrs
  names(res) <- res_names[used_elements]
  res

}

### CONFIDENCE FUNCTIONS

# Function to calculate confidence intervals from replicates
# when only 1 fcst_model
calc_confidence <- function(replicates, grps, conf) {

  q_lower <- (1 - conf) / 2
  q_upper <- conf + q_lower

  dplyr::group_by(replicates, !!!rlang::syms(grps)) %>%
    dplyr::summarise(
      dplyr::across(
        where(is.double),
        list(
          mean   = ~mean(.x, na.rm = TRUE),
          median = ~stats::median(.x, na.rm = TRUE),
          upper  = ~stats::quantile(.x, q_upper, na.rm = TRUE),
          lower  = ~stats::quantile(.x, q_lower, na.rm = TRUE)
        )
      )
    ) %>%
    dplyr::ungroup()

}


# Function to compute confidence intervals and confidence of differnces
# between fcst_models when there is more than one fcst_model
calc_diff_confidence <- function(
  replicates, fcst_models, groupings, perfect_scores, conf
) {

  q_lower <- (1 - conf) / 2
  q_upper <- conf + q_lower

  if (is.element("threshold", colnames(replicates))) {
    groupings = c(groupings, "threshold")
  }

  replicates <- dplyr::mutate(
    replicates,
    dplyr::across(dplyr::contains("num_cases"), as.double)
  )

  replicates <- tidyr::pivot_longer(
    dplyr::select(replicates, -tidyselect::ends_with("_std_error")),
    where(is.double) & tidyselect::any_of(names(perfect_scores)),
    names_to = "score"
  )

  diff_list <- list()

  for (i in 1:length(fcst_models)) {

    join_cols <- c(groupings, "replicate", "score")

    diff_df <- dplyr::inner_join(
      dplyr::rename(
        dplyr::filter(replicates, .data[["mname"]] == fcst_models[i]),
        ref_score = .data[["value"]], ref_model = .data[["mname"]]
      ),
      dplyr::rename(
        dplyr::filter(replicates, .data[["mname"]] != fcst_models[i]),
        fcst_score = .data[["value"]], fcst_model = .data[["mname"]]
      ),
      by = join_cols
    )

    diff_df <- dplyr::mutate(
      diff_df,
      difference = .data[["fcst_score"]] - .data[["ref_score"]],
      better     = as.numeric(
        abs(.data[["fcst_score"]] - perfect_scores[.data[["score"]]]) <
          abs(.data[["ref_score"]] - perfect_scores[.data[["score"]]])
      )
    )

    grps <- rlang::syms(c("fcst_model", "ref_model", groupings, "score"))

    diff_list[[i]] <- dplyr::group_by(diff_df, !!!grps) %>%
      dplyr::summarise(
        dplyr::across(
          where(is.double) &
            (tidyselect::ends_with("_score") |
                tidyselect::all_of(c("difference", "better"))),
          list(
            mean   = ~mean(.x, na.rm = TRUE),
            median = ~stats::median(.x, na.rm = TRUE),
            upper  = ~stats::quantile(.x, q_upper, na.rm = TRUE),
            lower  = ~stats::quantile(.x, q_lower, na.rm = TRUE)
          )
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::rename(percent_better = .data[["better_mean"]]) %>%
      dplyr::select(-tidyselect::starts_with("better_"))

  }

  dplyr::bind_rows(diff_list)

}



