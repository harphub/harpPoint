#' Compute all verification scores for an ensemble.
#'
#' @param .fcst A \code{harp_fcst} object with tables that have a column for
#'   observations, or a single forecast table.
#' @param parameter The name of the column for the observed data.
#' @param verify_members Whether to verify the individual members of the
#'   ensemble. Even if thresholds are supplied, only summary scores are
#'   computed. If you wish to compute categorical scores, the separate
#'   \link[harpPoint]{det_verify} function must be used.
#' @param thresholds A numeric vector of thresholds for which to compute the
#'   threshold based scores. Set to NULL (the default) to only compute summary
#'   scores.
#' @param groupings The groups for which to compute the scores. See
#'   \link[dplyr]{group_by} for more information of how grouping works.
#' @param rel_probs Probabilities to use for reliability diagrams. Set to NA
#'   (the default) to select automatically.
#' @param num_ref_members For "fair" scores, the score is scaled to be valid for
#'   this number of ensemble members. Set to NA (the default) to not modify the
#'   score.
#' @param spread_drop_member Which members to drop for the calculation of the
#'   ensemble variance and standard deviation. For harp_fcst objects, this can
#'   be a numeric scalar - in which case it is recycled for all forecast models;
#'   a list or numeric vector of the same length as the harp_fcst object, or a
#'   named list with the names corresponding to names in the harp_fcst object.
#' @param jitter_fcst A function to perturb the forecast values by. This is used
#'   to account for observation error in the rank histogram. For other
#'   statistics it is likely to make little difference since it is expected that
#'   the observations will have a mean error of zero.
#' @param climatology The climatology to use for the Brier Skill Score. Can be
#'   "sample" for the sample climatology (the default), a named list with
#'   elements eps_model and member to use a member of an eps model in the
#'   harp_fcst object for the climatology, or a data frame with columns for
#'   threshold and climatology and also optionally leadtime.
#' @param show_progress Logical - whether to show progress bars. Defaults to
#'   TRUE.
#'
#' @return A list containting three data frames: \code{ens_summary_scores},
#'   \code{ens_threshold_scores} and \code{det_summary_scores}.
#' @export
#'
#' @examples
ens_verify <- function(
  .fcst,
  parameter,
  verify_members     = TRUE,
  thresholds         = NULL,
  groupings          = "lead_time",
  rel_probs          = NA,
  num_ref_members    = NA,
  spread_drop_member = NULL,
  jitter_fcst        = NULL,
  climatology        = "sample",
  rank_hist          = TRUE,
  crps               = TRUE,
  brier              = TRUE,
  roc                = TRUE,
  econ_val           = TRUE,
  show_progress      = TRUE,
  ...
) {
  UseMethod("ens_verify")
}

#' @export
ens_verify.harp_ens_point_df <- function(
  .fcst,
  parameter,
  verify_members     = TRUE,
  thresholds         = NULL,
  groupings          = "lead_time",
  rel_probs          = NA,
  num_ref_members    = NA,
  spread_drop_member = NULL,
  jitter_fcst        = NULL,
  climatology        = "sample",
  rank_hist          = TRUE,
  crps               = TRUE,
  show_progress      = TRUE,
  brier              = TRUE,
  roc                = TRUE,
  econ_val           = TRUE,
  fcst_model         = NULL,
  ...
) {

  col_names  <- colnames(.fcst)
  parameter  <- rlang::enquo(parameter)
  chr_param  <- rlang::quo_name(parameter)


  if (!is.list(groupings)) {
    groupings <- list(groupings)
  }

  fcst_model <- parse_fcst_model(.fcst, fcst_model)
  .fcst[["fcst_model"]] <- fcst_model

  lead_time_col <- intersect(c("lead_time", "leadtime"), colnames(.fcst))

  groupings <- lapply(
    groupings,
    function(x) gsub("lead_time|leadtime", lead_time_col, x)
  )

  if (length(grep(chr_param, col_names)) < 1) {
    stop(paste("No column found for", chr_param), call. = FALSE)
  }

  if (is.function(jitter_fcst)) {
    .fcst <- dplyr::mutate_at(.fcst,  dplyr::vars(dplyr::matches("_mbr[[:digit]]+")), ~ purrr::map_dbl(., jitter_fcst))
  }

  if (verify_members) {
    det_summary_scores <- det_verify(.fcst, !! parameter, groupings = groupings, show_progress = show_progress) %>%
      purrr::pluck("det_summary_scores")
  } else {
    det_summary_scores <- tibble::tibble()
  }

  num_members <- length(grep("_mbr[[:digit:]]+", colnames(.fcst)))

  if (num_members <= 1) {

    warning("Not enough members to do ensemble verification", immediate. = TRUE, call. = FALSE)
    ens_summary_scores <- tibble::tibble()

  } else if (inherits(.fcst, "harp_ens_probs")) {

    warning("Cannot compute summary scores for probabilities", immediate. = TRUE, call. = FALSE)
    ens_summary_scores <- tibble::tibble()

  } else {

    .fcst <- harpCore::ens_stats(
      .fcst, sd = FALSE, var = TRUE, keep_members = TRUE
    )

    ens_summary_scores <- list()

    ens_summary_scores[["ss"]] <- ens_spread_and_skill(
      .fcst, !!parameter, groupings = groupings,
      spread_drop_member = spread_drop_member
    )[["ens_summary_scores"]]

    if (rank_hist) {
      ens_summary_scores[["rh"]] <- ens_rank_histogram(
        .fcst, !! parameter, groupings = groupings
      )[["ens_summary_scores"]]
    }

    if (crps) {
      ens_summary_scores[["crps"]] <- ens_crps(
        .fcst, !! parameter, groupings = groupings,
        num_ref_members = num_ref_members, show_progress = show_progress
      )[["ens_summary_scores"]]
    }

    ens_summary_scores <- Reduce(
      function(x, y) suppressMessages(dplyr::inner_join(x, y)),
      ens_summary_scores
    )

  }

  if (is.numeric(thresholds) && num_members > 1) {

    if (!inherits(.fcst, "harp_ens_probs")) {
      .fcst <- ens_probabilities(.fcst, thresholds, !! parameter)
    }

    ens_threshold_scores <- list()

    if (brier) {
      ens_threshold_scores[["brier"]] <- ens_brier(
        .fcst,
        groupings       = groupings,
        climatology     = climatology,
        rel_probs       = rel_probs,
        num_ref_members = num_ref_members,
        show_progress   = show_progress
      )[["ens_threshold_scores"]]
    }

    if (roc) {
      ens_threshold_scores[["roc"]] <- ens_roc(
        .fcst, groupings = groupings, show_progress = show_progress
      )[["ens_threshold_scores"]]
    }

    if (econ_val) {
      ens_threshold_scores[["val"]] <- ens_value(
        .fcst, groupings = groupings, show_progress = show_progress
      )[["ens_threshold_scores"]]
    }

    ens_threshold_scores <- Reduce(
      function(x, y) suppressMessages(dplyr::inner_join(x, y)),
      ens_threshold_scores
    )

  } else {

    ens_threshold_scores <- tibble::tibble()

  }

  if (!is.null(ens_summary_scores)) {
    ens_summary_scores <- dplyr::mutate(
      ens_summary_scores,
      fcst_model = fcst_model, .before = dplyr::everything()
    )
  }
  if (!is.null(ens_threshold_scores)) {
    ens_threshold_scores <- dplyr::mutate(
      ens_threshold_scores,
      fcst_model = fcst_model, .before = dplyr::everything()
    )
  }
  if (!is.null(det_summary_scores)) {
      det_summary_scores <- dplyr::mutate(
      det_summary_scores,
      fcst_model = fcst_model, .before = dplyr::everything()
    )
  }

  res <- list(
    ens_summary_scores   = ens_summary_scores,
    ens_threshold_scores = ens_threshold_scores,
    det_summary_scores   = det_summary_scores
  )

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
ens_verify.harp_list <- function(
  .fcst,
  parameter,
  verify_members     = TRUE,
  thresholds         = NULL,
  groupings          = "lead_time",
  rel_probs          = NA,
  num_ref_members    = NA,
  spread_drop_member = NULL,
  jitter_fcst        = NULL,
  climatology        = "sample",
  rank_hist          = TRUE,
  crps               = TRUE,
  brier              = TRUE,
  roc                = TRUE,
  econ_val           = TRUE,
  show_progress      = TRUE,
  ...
) {
  parameter   <- rlang::enquo(parameter)
  if (!inherits(try(rlang::eval_tidy(parameter), silent = TRUE), "try-error")) {
    if (is.character(rlang::eval_tidy(parameter))) {
      parameter <- rlang::eval_tidy(parameter)
      parameter <- rlang::ensym(parameter)
    }
  }

  spread_drop_member <- parse_member_drop(spread_drop_member, names(.fcst))

  if (!is.null(thresholds)) climatology <- get_climatology(
    .fcst, !! parameter, thresholds, climatology
  )

  list_to_harp_verif(
    purrr::pmap(
      list(.fcst, names(.fcst), spread_drop_member),
      function(x, y, z) ens_verify(
        x, !!parameter, verify_members, thresholds, groupings, rel_probs,
        num_ref_members, z, jitter_fcst, climatology,
        rank_hist, crps, brier, roc, econ_val,
        show_progress, fcst_model = y
      )
    )
  )


}


# Internal function to get climatology for Brier Skill Score
get_climatology <- function(.fcst, parameter, thresholds, climatology) {

  if (inherits(climatology, "data.frame")) {

    if (!all(c("threshold", "climatology") %in% names(climatology))) {
      stop("climatology must at least contain columns named threshold and climatology", call. = FALSE)
    }
    if (is.element("leadtime", names(climatology))) {
      if (!all(.fcst$leadtime %in% climatology$leadtime)) {
        stop("Not all leadtimes for the data exist in climatology", call. = FALSE)
      }
    }
    return(climatology)

  } else if (is.list(climatology)) {

    if (!all(c("eps_model", "member") %in% names(climatology))) {
      stop("When supplying climatology as a list it must have names 'eps_model' and 'member'", call. = FALSE)
    }
    if (!is.character(climatology$eps_model) | length(climatology$eps_model) > 1) {
      stop("When supplying climatology as a list 'eps_model' must be a single string", call. = FALSE)
    }
    if (!is.numeric(climatology$member) | length(climatology$member) > 1) {
      stop("When supplying climatology as a list 'member' must be a single number", call. = FALSE)
    }
    if (!is.element(climatology$eps_model, names(.fcst))) {
      stop("eps_model ", climatology$eps_model, " given in climatology not found in .fcst", call. = FALSE)
    }
    member_name <- paste0(climatology$eps_model, "_mbr", formatC(climatology$member, width = 3, flag = "0"))
    if (!is.element(member_name, names(.fcst[[climatology$eps_model]]))) {
      stop("Member ", climatology$member, " given in climatology not found for ", climatology$eps_model, call. = FALSE)
    }

    member_col   <- rlang::sym(member_name)
    list_element <- which(names(.fcst) == climatology$eps_model)

  } else if (climatology == "sample") {

    if (!missing(parameter)) {
      member_col   <- rlang::enquo(parameter)
    }
    list_element <- 1

  } else {

    stop(
      paste(
        "climatology must be 'sample', a data frame with columns 'threshold' and 'climatology'\n",
        "  or a list with named elements 'eps_model' and 'member'."
      ),
      call. = FALSE
    )

  }

  if (!inherits(.fcst[[list_element]], "harp_ens_probs")) {
    if (missing(parameter) | missing(thresholds)) {
      stop("parameter and thresholds must be passed as arguments", call. = FALSE)
    } else {
      climatol <- ens_probabilities(.fcst[[list_element]], thresholds, !! member_col)
    }
  } else {
    climatol <- .fcst[[list_element]]
  }

  climatol %>%
    dplyr::group_by(.data$threshold, .data$lead_time) %>%
    dplyr::summarise(climatology = mean(.data$obs_prob))

}


# Internal function to add forecast attributes to a verification output
add_attributes <- function(.verif, dttm, parameter, stations, groupings) {
  parameter <- rlang::enquo(parameter)

  attr(.verif, "parameter") <- rlang::quo_name(parameter)
  attr(.verif, "dttm") <- dttm
  attr(.verif, "stations") <- stations
  attr(.verif, "group_vars") <- groupings

  .verif
}

# Print method for harp_verif
#' @export
print.harp_verif <- function(x, n = NULL, ...) {
  invisible(
    mapply(
      function(x, y) {
        cat(cli::col_green(paste0("::", y, ":: ")))
        print(x, n = n)
        cat("\n")
      },
      x, names(x)
    )
  )
  parameter <- paste(Reduce(union, attr(x, "parameter")), collapse = ", ")
  dttm <- Reduce(union, attr(x, "dttm"))
  dttm_range <- format(
    harpCore::as_dttm(range(dttm)),
    "%R %Z %d %b %Y"
  )
  stations <- Reduce(union, attr(x, "stations"))
  num_stations <- length(stations)
  groupings <- attr(x, "group_vars")
  if (!is.list(groupings)) {
    groupings <- list(groupings)
  }
  if (all(vapply(groupings, is.list, logical(1)))) {
    groupings <- purrr::flatten(groupings)
  }
  groupings <- lapply(
    groupings,
    function(g) g[vapply(g, nchar, integer(1)) > 0]
  )
  groupings <- groupings[vapply(groupings, length, integer(1)) > 0]
  cat(
    cli::col_cyan("--harp verification for "),
    cli::col_magenta(parameter),
    cli::col_cyan("--"),
    sep = ""
  )
  cat(
    "\n",
    cli::col_cyan("# for forecasts from"),
    cli::col_magenta(dttm_range[1]),
    cli::col_cyan("to"),
    cli::col_magenta(dttm_range[2])
  )
  if (num_stations > 0) {
    cat(
      "\n",
      cli::col_cyan("# using"),
      cli::col_magenta(num_stations),
      cli::col_cyan("observation stations")
    )
  }
  if (length(groupings) > 0) {
    cat("\n", cli::col_cyan("# for verification groups: "))
    invisible(
      lapply(
        groupings,
        function(g) {
          g <- glue::glue_collapse(g, sep = ", ", last = " & ")
          cat("\n   ", cli::col_cyan("->"), cli::col_magenta(g))
        }
      )
    )
    cat("\n")
    cli::cli_inform(c(
      "i" = cli::col_silver("use `attributes()` to see detailed metadata")
    ))
  }

}

# Compare attributes returning a list if they're different and the only value
# if they're the same
compare_attrs <- function(x) {
  if (length(x) < 2) {
    return(x[[1]])
  }
  same <- TRUE
  i <- 1
  while (same && i < length(x)) {
    same <- identical(sort_attr(x[[i]]), sort_attr(x[[(i + 1)]]))
    i <- i + 1
  }
  if (same) {
    return(x[[1]])
  }
  x
}

sort_attr <- function(x) {
  if (is.list(x)) {
    return(lapply(x, sort))
  }
  sort(x)
}

# Parse the fcst_model input
parse_fcst_model <- function(.fcst, fcst_model, caller = rlang::caller_env()) {
  if (!is.null(fcst_model)) {
    cat(
      cli::col_green(
        glue::glue("::Computing verification for fcst_model `{fcst_model}`::")
      ),
      "\n"
    )
  } else {
    fcst_model <- unique(.fcst[["fcst_model"]])
  }

  fcst_model_err(fcst_model, caller)

  fcst_model
}

# Error message when there is more than one fcst_model in a data frame
fcst_model_err <- function(fcst_model, caller) {
  if (length(fcst_model) == 1) return()

  if (length(fcst_model) < 1) {
    cli::cli_abort(c(
      "No {.var fcst_model} column found in data frame.",
      "x" = "Data frame must have a {.var fcst_model} column.",
      "i" = paste(
        "Use a `harpIO::read_*` function to ensure data",
        "are in the correct form."
      )
    ), call = caller)

  }

  err <- paste(fcst_model)
  cli::cli_abort(c(
    "More than one {.var fcst_model} found in data frame.",
    "x" = "Data frame has {fcst_model} in the {.var fcst_model} column.",
    "i" = "Split data using e.g. `as_harp_list(split(fcst, fcst$fcst_model)).`"
  ), call = caller)
}

# Convert a list created by a verification method for harp_list to a
# an object of class harp_verif
list_to_harp_verif <- function(.l) {

  res <- list(

    ens_summary_scores = purrr::list_rbind(
      purrr::map(.l, "ens_summary_scores")
    ),

    ens_threshold_scores = purrr::list_rbind(
      purrr::map(.l, "ens_threshold_scores")
    ),

    det_summary_scores = purrr::list_rbind(
      purrr::map(.l, "det_summary_scores")
    ),

    det_threshold_scores = purrr::list_rbind(
      purrr::map(.l, "det_threshold_scores")
    )
  )

  res <- res[vapply(res, function(x) !is.null(x), logical(1))]

  res_attrs <- lapply(.l, attributes)

  param_attr <- compare_attrs(
    lapply(res_attrs, function(x) x[["parameter"]])
  )
  dttm_attr <- compare_attrs(
    lapply(res_attrs, function(x) x[["dttm"]])
  )
  stations_attr <- compare_attrs(
    lapply(res_attrs, function(x) x[["stations"]])
  )
  group_vars_attr <- compare_attrs(
    lapply(res_attrs, function(x) x[["group_vars"]])
  )

  structure(
    res[which(vapply(res, nrow, integer(1)) > 0)],
    class      = "harp_verif",
    parameter  = param_attr,
    dttm       = dttm_attr,
    stations   = stations_attr,
    group_vars = group_vars_attr
  )



}
