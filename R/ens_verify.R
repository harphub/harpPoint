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
#' @param num_ref_members For "fair" scores, the score is scaled to be valid for
#'   this number of ensemble members. Set to NA (the default) to not modify the
#'   score.
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
  verify_members  = TRUE,
  thresholds      = NULL,
  groupings       = "leadtime",
  num_ref_members = NA,
  jitter_fcst     = NULL,
  climatology     = "sample",
  show_progress   = TRUE
) {
  UseMethod("ens_verify")
}

#' @export
ens_verify.default <- function(
  .fcst,
  parameter,
  verify_members  = TRUE,
  thresholds      = NULL,
  groupings       = "leadtime",
  num_ref_members = NA,
  jitter_fcst     = NULL,
  climatology     = "sample",
  show_progress   = TRUE
) {

  if (length(groupings) == 1 && groupings == "threshold") {
    .fcst                <- dplyr::mutate(.fcst, group_col = NA_real_)
    summary_score_groups <- "group_col"
  } else {
    summary_score_groups <- groupings[groupings != "threshold"]
  }

  col_names  <- colnames(.fcst)
  parameter  <- rlang::enquo(parameter)
  chr_param  <- rlang::quo_name(parameter)
  group_sym  <- rlang::syms(summary_score_groups)
  crps_out   <- rlang::sym("crps_output")

  if (length(grep(chr_param, col_names)) < 1) {
    stop(paste("No column found for", chr_param), call. = FALSE)
  }

  if (is.function(jitter_fcst)) {
    .fcst <- dplyr::mutate_at(.fcst,  dplyr::vars(dplyr::matches("_mbr[[:digit]]+")), ~ purrr::map_dbl(., jitter_fcst))
  }

  if (verify_members) {
    det_summary_scores <- det_verify(.fcst, !! parameter, groupings = summary_score_groups, show_progress = show_progress) %>%
      purrr::pluck("det_summary_scores")
  } else {
    det_summary_scores <- NULL
  }

  num_members <- length(grep("_mbr[[:digit]]+", colnames(.fcst)))

  if (num_members <= 1) {

    warning("Not enough members to do ensemble verification", immediate. = TRUE, call. = FALSE)
    ens_summary_scores <- NULL

  } else if (inherits(.fcst, "harp_ens_probs")) {

    warning("Cannot compute summary scores for probabilities", immediate. = TRUE, call. = FALSE)
    ens_summary_scores <- NULL

  } else {

    select_non_missing_members <- function(df) {
      dplyr::select_if(df, ~ !all(is.na(.)))
    }

    grouped_fcst <- .fcst %>%
      dplyr::group_by(!!! group_sym)

    grouped_fcst <- tidyr::nest(grouped_fcst, .key = "grouped_fcst") %>%
      dplyr::mutate(
        grouped_fcst = purrr::map(.data$grouped_fcst, select_non_missing_members),
        grouped_fcst = purrr::map(.data$grouped_fcst, ens_mean_and_var, mean_name = "ens_mean", var_name = "ens_var"),
        grouped_fcst = purrr::map(.data$grouped_fcst, tidyr::drop_na)
      )
    ens_summary_scores <- grouped_fcst %>%
      dplyr::transmute(
        !!! group_sym,
        num_cases      = purrr::map_int(grouped_fcst, nrow),
        ss             = purrr::map(grouped_fcst, ens_spread_and_skill, !! parameter, groupings = NULL),
        rank_histogram = purrr::map(grouped_fcst, ens_rank_histogram, !! parameter, groupings = NULL),
        crps           = purrr::map(grouped_fcst, ens_crps, !! parameter, groupings = NULL, num_ref_members = num_ref_members, show_progress = show_progress)
      ) %>%
      tidyr::unnest(.data$ss, .data$crps, .data$rank_histogram) %>%
      dplyr::select(-dplyr::matches("^num_cases[[:digit:]]+$"))

  }

  if (is.numeric(thresholds) && num_members > 1) {

    groupings  <- union("threshold", groupings)
    group_sym  <- rlang::syms(groupings)
    join_cols  <- c("SID", "fcdate", "leadtime", "validdate", "threshold")
    meta_cols  <- rlang::syms(c("SID", "fcdate", "leadtime", "validdate"))

    if (!inherits(.fcst, "harp_ens_probs")) {
      .fcst <- ens_probabilities(.fcst, thresholds, !! parameter)
    }

    brier_function <- function(df, climatology, show_progress) {
      res <- ens_brier(df, climatology = NULL, groupings = NULL)
      if (show_progress) {
        brier_progress$tick()
      }
      res
    }

    value_function <- function(df, show_progress) {
      res <- ens_value(df, groupings = NULL)
      if (show_progress) {
        value_progress$tick()
      }
      res
    }

    roc_function <- function(df, show_progress) {
      res <- ens_roc(df, groupings = NULL)
      if (show_progress) {
        roc_progress$tick()
      }
      res
    }

    if (inherits(climatology, "data.frame")) {
      if (all(c("leadtime", "threshold") %in% names(climatology))) {
        join_cols <- c("leadtime", "threshold")
      } else {
        join_cols <- "threshold"
      }
      .fcst <- dplyr::inner_join(.fcst, climatology, by = join_cols) %>%
        dplyr::rename(bss_ref_climatology = .data$climatology)
    }

    grouped_fcst <- .fcst %>%
      dplyr::group_by(!!! group_sym) %>%
      tidyr::nest(.key = "grouped_fcst")

    if (inherits(.fcst, "harp_ens_probs")) {
      grouped_fcst$grouped_fcst <- purrr::map(
        grouped_fcst$grouped_fcst,
        ~ structure(.x, class = c("harp_ens_probs", class(.x)))
      )
    }

    if (show_progress) {
      brier_progress <- progress::progress_bar$new(format = "  Brier [:bar] :percent eta: :eta", total = nrow(grouped_fcst))
      value_progress <- progress::progress_bar$new(format = "  Value [:bar] :percent eta: :eta", total = nrow(grouped_fcst))
      roc_progress   <- progress::progress_bar$new(format = "  ROC   [:bar] :percent eta: :eta", total = nrow(grouped_fcst))
    }

    ens_threshold_scores <- grouped_fcst %>%
      dplyr::transmute(
        !!! group_sym,
        brier_output = purrr::map(
          .data$grouped_fcst,
          brier_function,
          climatology,
          show_progress
        ),
        economic_value = purrr::map(
          .data$grouped_fcst,
          value_function,
          show_progress
        ),
        roc_output = purrr::map(
          .data$grouped_fcst,
          roc_function,
          show_progress
        )
      ) %>%
      tidyr::unnest(.data$brier_output, .data$economic_value, .data$roc_output)

  } else {

    ens_threshold_scores <- NULL

  }

  list(
    ens_summary_scores   = ens_summary_scores,
    ens_threshold_scores = ens_threshold_scores,
    det_summary_scores   = det_summary_scores
  )

}

#' @export
ens_verify.harp_fcst <- function (
  .fcst,
  parameter,
  verify_members  = TRUE,
  thresholds      = NULL,
  groupings       = "leadtime",
  num_ref_members = NA,
  jitter_fcst     = NULL,
  climatology     = "sample",
  show_progress   = TRUE
) {
  parameter   <- rlang::enquo(parameter)
  if (!is.null(thresholds)) climatology <- get_climatology(.fcst, !! parameter, thresholds, climatology)
  list_result <- purrr::map(.fcst, ens_verify, !! parameter, verify_members, thresholds, groupings, num_ref_members, jitter_fcst, climatology, show_progress)
  list(
    ens_summary_scores   = dplyr::bind_rows(
      purrr::map(list_result, "ens_summary_scores"),
      .id = "mname"
    ),
    ens_threshold_scores = dplyr::bind_rows(
      purrr::map(list_result, "ens_threshold_scores"),
      .id = "mname"
    ),
    det_summary_scores = dplyr::bind_rows(
      purrr::map(list_result, "det_summary_scores"),
      .id = "mname"
    )

  ) %>% add_attributes(.fcst, !! parameter)

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
    dplyr::group_by(.data$threshold, .data$leadtime) %>%
    dplyr::summarise(climatology = mean(.data$obs_prob))

}


# Internal function to add forecast attributes to a verification output
add_attributes <- function(.verif, .fcst, parameter) {
  parameter <- rlang::enquo(parameter)

  dates     <- unlist(purrr::map(.fcst, "fcdate"))
  SIDs      <- unlist(purrr::map(.fcst, "SID"))

  attr(.verif, "parameter")    <- rlang::quo_name(parameter)
  attr(.verif, "start_date")   <- harpIO::unixtime_to_str_datetime(min(dates), harpIO::YMDh)
  attr(.verif, "end_date")     <- harpIO::unixtime_to_str_datetime(max(dates), harpIO::YMDh)
  attr(.verif, "num_stations") <- length(unique(SIDs))

  .verif
}


