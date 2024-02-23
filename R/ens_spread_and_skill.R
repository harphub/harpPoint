#' Compute the skill (RMSE) and spread of an ensemble forecast
#'
#' The ensemble mean and spread are computed as columns in a \code{harp_list}
#' object. Typically the scores are aggregated over lead time by other grouping
#' variables cam be chosen. The mean bias is also computed.
#'
#' @inheritParams ens_verify
#'
#' @return An object of the same format as the inputs but with data grouped for
#'   the \code{groupings} column(s) and columns for \code{rmse}, \code{spread}
#'   and \code{mean_bias}.
#' @export
ens_spread_and_skill <- function(
  .fcst,
  parameter,
  groupings          = "lead_time",
  circle             = NULL,
  spread_drop_member = NULL,
  jitter_fcst        = NULL,
  show_progress      = TRUE,
  ...
) {
  if (missing(parameter)) {
    cli::cli_abort(
      "Argument {.arg parameter} is missing with no default."
    )
  }
  check_circle(circle)
  # Set progress bar to false for batch running
  if (!interactive()) show_progress <- FALSE
  UseMethod("ens_spread_and_skill")
}

#' @param fcst_model The name of the forecast model to use in the `fcst_model`
#'  column of the output. If the function is dispatched on a `harp_list`
#'  object, the names of the `harp_list` are automatically used.
#' @rdname ens_spread_and_skill
#' @export
ens_spread_and_skill.harp_ens_point_df <- function(
  .fcst,
  parameter,
  groupings          = "lead_time",
  circle             = NULL,
  spread_drop_member = NULL,
  jitter_fcst        = NULL,
  show_progress      = TRUE,
  fcst_model         = NULL,
  ...
) {

  if (!is.list(groupings)) {
    groupings <- list(groupings)
  }

  fcst_model <- parse_fcst_model(.fcst, fcst_model)
  .fcst[["fcst_model"]] <- fcst_model

  if (!is.null(spread_drop_member)) {
    if (!is.numeric(spread_drop_member)) {
      stop("`spread_drop_member` must be numeric.", call. = FALSE)
    }
  }

  col_names  <- colnames(.fcst)
  parameter  <- rlang::enquo(parameter)
  chr_param  <- rlang::quo_name(parameter)
  if (length(grep(chr_param, col_names)) < 1) {
    stop(paste("No column found for", chr_param), call. = FALSE)
  }

  if (is.function(jitter_fcst)) {
    .fcst <- dplyr::mutate_at(
      .fcst,
      dplyr::vars(dplyr::contains("_mbr")),
      ~purrr::map_dbl(., jitter_fcst)
    )
  }

  ens_mean <- "ss_mean"
  ens_var  <- "ss_var"

  .fcst <- harpCore::ens_mean_and_var(
    .fcst, mean_name = ens_mean, var_name = ens_var,
    var_drop_member = spread_drop_member
  )

  compute_spread_skill <- function(compute_group, fcst_df) {

    if (!any(grepl("dropped_members", colnames(fcst_df)))) {
      fcst_df[[paste0("dropped_members_", ens_var)]] <- fcst_df[[ens_var]]
    }

    fcst_df <- dplyr::mutate(
      fcst_df,
      fcst_bias = bias(.data[[ens_mean]], .data[[chr_param]], circle)
    )

    fcst_df <- group_without_threshold(fcst_df, compute_group)
    group_vars <- dplyr::group_vars(fcst_df)
    group_names <- glue::glue_collapse(group_vars, sep = ", ", last = " & ")
    message(
      cli::col_blue(glue::glue("Spread; Skill for {group_names}")),
      appendLF = FALSE
    )

    res <- fcst_df %>%
      dplyr::summarise(
        num_cases              = dplyr::n(),
        num_stations           = {
          if (is.element("SID", group_vars)) {
            1L
          } else {
            length(unique(.data[["SID"]]))
          }
        },
        mean_bias              = mean(.data[["fcst_bias"]]),
        stde                   = stats::sd(.data[["fcst_bias"]]),
        rmse                   = sqrt(mean(.data[["fcst_bias"]] ^ 2)),
        spread                 = sqrt(mean(.data[[ens_var]])),
        dropped_members_spread = sqrt(mean(.data[[paste0("dropped_members_", ens_var)]]))
      ) %>%
      dplyr::mutate(
        spread_skill_ratio                 = .data[["spread"]] / .data[["rmse"]],
        dropped_members_spread_skill_ratio = .data[["dropped_members_spread"]] / .data[["rmse"]]
      )

    message("", cli::col_green(cli::symbol[["tick"]]))

    res
  }

  res <- list()
  res[["ens_summary_scores"]] <- purrr::map(
    groupings, compute_spread_skill, .fcst
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
ens_spread_and_skill.harp_list <- function(
  .fcst,
  parameter,
  groupings          = "lead_time",
  circle             = NULL,
  spread_drop_member = NULL,
  jitter_fcst        = NULL,
  show_progress      = TRUE,
  ...
) {

  parameter   <- rlang::ensym(parameter)
#  if (!inherits(try(rlang::eval_tidy(parameter), silent = TRUE), "try-error")) {
#    if (is.character(rlang::eval_tidy(parameter))) {
#      parameter <- rlang::eval_tidy(parameter)
#      parameter <- rlang::ensym(parameter)
#    }
#  }

  spread_drop_member <- parse_member_drop(spread_drop_member, names(.fcst))

  list_to_harp_verif(
    purrr::pmap(
      list(.fcst, names(.fcst), spread_drop_member),
      function(x, y, z) ens_spread_and_skill(
        x, !! parameter, groupings, circle, z, jitter_fcst, fcst_model = y
      )
    )
  )
}

parse_member_drop <- function(x, nm) {

  if (!is.null(names(x))) {
    x <- as.list(x)
  }

  if (!is.list(x)) {
    if (is.null(x)) {
      return(sapply(nm, function(x) NULL, simplify = FALSE))
    }
    if (length(x) == 1) {
      return(sapply(nm, function(.x) x, simplify = FALSE))
    }
    if (length(x) == length(nm)) {
      x <- as.list(x)
      names(x) <- nm
      return(x)
    }
    stop("Bad input for `spread_exclude_member`", call. = FALSE)
  }

  if (is.null(names(x))) {

    if (length(x) == length(nm)) {
      names(x) <- nm
      return(x)
    }

    stop(
      "If `spread_exclude_member` is a list ",
      "it must be the same length as `.fcst` or have names",
      call. = FALSE
    )

  }

  if (identical(sort(names(x)), sort(nm))) {
    return(x[nm])
  }

  if (length(intersect(names(x), nm)) < 1) {
    stop(
      "spread_exclude_member: ",
      paste(names(x), collapse = ", "),
      " not found in `.fcst`.",
      call. = FALSE
    )
  }

  if (length(setdiff(names(x), nm)) > 0) {
    stop(
      "spread_exclude_member: ",
      paste(setdiff(names(x), nm), collapse = ", "),
      " not found in `.fcst`.",
      call. = FALSE
    )
  }

  x <- c(x, sapply(setdiff(nm, names(x)), function(x) NULL, simplify = FALSE))

  x[nm]

}


