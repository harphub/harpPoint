#' Continuous Rank Probability Score (CRPS) for an ensemble.
#'
#' The CRPS and its decomposition are computed as columns in a \code{harp_list},
#' or `harp_ens_grid_df` object. Typically the scores are aggregated over lead
#' time, but other grouping variables cam be chosen.
#'
#' @inheritParams ens_verify
#' @return An object of the same format as the inputs but with data grouped for
#'   the \code{groupings} column(s) and columns for \code{crps}, \code{crps_pot}
#'   and \code{crps_rel}.
#' @export
ens_crps <- function(
  .fcst,
  parameter,
  groupings        = "lead_time",
  num_ref_members  = NA,
  keep_full_output = FALSE,
  show_progress    = TRUE,
  ...
) {
  if (missing(parameter)) {
    cli::cli_abort(
      "Argument {.arg parameter} is missing with no default."
    )
  }
  # Set progress bar to false for batch running
  if (!interactive()) show_progress <- FALSE
  UseMethod("ens_crps")
}

#' @param fcst_model The name of the forecast model to use in the `fcst_model`
#'  column of the output. If the function is dispatched on a `harp_list`
#'  object, the names of the `harp_list` are automatically used.
#' @rdname ens_crps
#' @export
ens_crps.harp_ens_point_df <- function(
  .fcst,
  parameter,
  groupings        = "lead_time",
  num_ref_members  = NA,
  keep_full_output = FALSE,
  show_progress    = TRUE,
  fcst_model       = NULL,
  ...
) {

  if (!is.list(groupings)) {
    groupings <- list(groupings)
  }

  fcst_model <- parse_fcst_model(.fcst, fcst_model)
  .fcst[["fcst_model"]] <- fcst_model

  col_names   <- colnames(.fcst)
  parameter   <- rlang::enquo(parameter)
  chr_param   <- rlang::quo_name(parameter)
  crps_output <- rlang::sym("crps_output")
  if (length(grep(chr_param, col_names)) < 1) {
    stop(paste("No column found for", chr_param), call. = FALSE)
  }

  .fcst <- bind_crps_vars(.fcst, !!parameter)

  crps_function <- function(df, show_progress) {
    res <- verification::crpsFromAlphaBeta(
      do.call(rbind, df[["alpha"]]),
      do.call(rbind, df[["beta"]]),
      df[["h0"]],
      df[["hN"]]
    )
    # if (show_progress) {
    #   crps_progress$tick()
    # }
    res
  }

  fair_crps <- function(df, parameter, R_new, show_progress) {
    parameter <- rlang::enquo(parameter)
    obs       <- dplyr::pull(df, !!parameter)
    fcst      <- as.matrix(
      df %>%
      dplyr::select(dplyr::contains("_mbr")) %>%
      dplyr::select_if(~sum(!is.na(.)) > 0)
    )
    res       <- mean(SpecsVerification::enscrps_cpp(fcst, obs, R_new = R_new))
    # if (show_progress) {
    #   fair_crps_progress$tick()
    # }
    res
  }

  if (show_progress) {
    # progress_total <- sum(
    #   sapply(
    #     groupings,
    #     function(x) length(dplyr::group_rows(dplyr::group_by(.fcst, !!! rlang::syms(intersect(x, names(.fcst))))))
    #   )
    # )
    # crps_progress      <- progress::progress_bar$new(format = "  CRPS [:bar] :percent eta: :eta", total = progress_total)
    # fair_crps_progress <- progress::progress_bar$new(format = "  Fair CRPS [:bar] :percent eta: :eta", total = progress_total)

  }

  compute_crps <- function(compute_group, fcst_df, show_progress) {

    fcst_df <- group_without_threshold(fcst_df, compute_group)
    group_vars <- dplyr::group_vars(fcst_df)
    group_names <- glue::glue_collapse(group_vars, sep = ", ", last = " & ")
    score_text <- cli::col_blue(glue::glue("CRPS for {group_names}"))
    if (show_progress) {
      pb_name <- score_text
    } else {
      pb_name <- FALSE
      cat(score_text)
      score_text <- ""
    }
    if (harpIO:::tidyr_new_interface()) {
      fcst_df <- tidyr::nest(fcst_df, grouped_fcst = -tidyr::one_of(compute_group)) %>%
        dplyr::ungroup()
    } else {
      fcst_df <- tidyr::nest(fcst_df, .key = "grouped_fcst")
    }
    fcst_df <- fcst_df %>%
      dplyr::mutate(
        num_cases       = purrr::map_int(.data$grouped_fcst, nrow),
        num_stations = {
          if (is.element("SID", group_vars)) {
            1L
          } else {
            purrr::map_int(.data[["grouped_fcst"]], ~length(unique(.x[["SID"]])))
          }
        },
        !! crps_output := purrr::map(
          .data$grouped_fcst, crps_function, .progress = pb_name
        )
      )
    cat(score_text, cli::col_green(cli::symbol[["tick"]]), "\n")

    if (!is.na(num_ref_members)) {
      score_text <- cli::col_blue(glue::glue("Fair CRPS for {group_names}"))
      if (show_progress) {
        pb_name <- score_text
      } else {
        pb_name <- FALSE
        cat(score_text)
        score_text <- ""
      }
      fcst_df <- dplyr::mutate(
        fcst_df,
        fair_crps = purrr::map_dbl(
          .data$grouped_fcst, fair_crps, !! parameter, num_ref_members,
          .progress = pb_name
        )
      )
      cat(score_text, cli::col_green(cli::symbol[["tick"]]), "\n")
    }

    fcst_df %>%
      dplyr::select(-.data$grouped_fcst) %>%
      sweep_crps(crps_output, keep_full_output)
  }

  res <- list()
  res[["ens_summary_scores"]] <- purrr::map(groupings, compute_crps, .fcst, show_progress) %>%
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
ens_crps.harp_list <- function(
  .fcst,
  parameter,
  groupings        = "lead_time",
  num_ref_members  = NA,
  keep_full_output = FALSE,
  show_progress    = TRUE,
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
      ~ens_crps(.x, !! parameter, groupings, num_ref_members, keep_full_output,
        show_progress, fcst_model = .y
      )
    )
  )
}

# Internal function to extract scores from the list output and add as columns to a data frame.
sweep_crps <- function(crps_df, crps_col, keep_full_output) {
  crps_col <- rlang::sym(crps_col)
  crps_df  <- crps_df %>%
    dplyr::mutate(
      crps             = purrr::map_dbl(!! crps_col, "CRPS"),
      crps_potential   = purrr::map_dbl(!! crps_col, "CRPSpot"),
      crps_reliability = purrr::map_dbl(!! crps_col, "Reli")
    )
  if (!keep_full_output) {
    crps_df <- dplyr::select(crps_df, - !! crps_col)
  }
  crps_df
}

bind_crps_vars <- function(.fcst, parameter) {

  parameter <- rlang::enquo(parameter)

  crps_func <- function(df, col) {

    col            <- rlang::enquo(col)
    crps_data_cols <- c("alpha", "beta", "h0", "hN")

    if (!identical(crps_data_cols, intersect(crps_data_cols, colnames(df)))) {
      crps_data     <- harp_crps(df, !!col)
      df[["alpha"]] <- unname(split(crps_data[["alpha"]], 1:nrow(crps_data[["alpha"]])))
      df[["beta"]]  <- unname(split(crps_data[["beta"]], 1:nrow(crps_data[["beta"]])))
      df[["h0"]]    <- crps_data[["heaviside0"]]
      df[["hN"]]    <- crps_data[["heavisideN"]]
    }

    df

  }

  if (inherits(.fcst, "harp_fcst")) {
    new_harp_fcst(lapply(.fcst, crps_func, !!parameter))
  } else {
    crps_func(.fcst, !!parameter)
  }

}
