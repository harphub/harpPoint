#' Compute binary probabilities for deterministic forecasts
#'
#' @param .fcst A \code{harp_list} object with `harp_det_point_df` deta frames,
#'   or a `harp_det_point_df` data frame.
#' @param parameter The name of the column for the observed data.
#' @param thresholds A numeric vector of thresholds for which to compute
#'   probabilities.
#' @param obs_probabilities A logical indicating whether or not to compute the
#'   binary probabilities for the observations.
#'
#' @return An object of the same class as `.fcst` with columns for threshold,
#' fcst_prob and optionally obs_prob instead of the raw forecast column.
det_probabilities <- function(
  .fcst,
  parameter,
  thresholds,
  comparator        = c("ge", "gt", "le", "lt", "eq", "between", "outside"),
  include_low       = TRUE,
  include_high      = TRUE,
  obs_probabilities = TRUE
) {
  UseMethod("det_probabilities")
}

#' @export
det_probabilities.harp_det_point_df <- function(
  .fcst,
  parameter,
  thresholds,
  comparator        = c("ge", "gt", "le", "lt", "eq", "between", "outside"),
  include_low       = TRUE,
  include_high      = TRUE,
  obs_probabilities = TRUE
) {

  parameter  <- rlang::enquo(parameter)
  chr_param  <- rlang::quo_name(parameter)
  col_names  <- colnames(.fcst)
  if (length(grep(chr_param, col_names)) < 1 && obs_probabilities) {
    stop(paste("No column found for", chr_param), call. = FALSE)
  }

  comparator <- match.arg(comparator)

  thresholds <- check_thresholds(thresholds, comparator)

  comparator_func <- get_comparator_func(comparator)

  harpCore::bind(lapply(
    thresholds,
    function(x) {
      res <- dplyr::mutate(
        .fcst,
        threshold = make_threshcol(x, comparator, include_low, include_high),
        fcst_prob = comparator_func(
          .data[["fcst"]], x, include_low, include_high
        )
      )
      if (obs_probabilities) {
        res <- dplyr::mutate(
          res,
          obs_prob = comparator_func(
            .data[[chr_param]], x, include_low, include_high
          )
        )
      }
      res
    }
  ))

}

#' @export
det_probabilities.harp_list <- function(
  .fcst,
  parameter,
  thresholds,
  comparator        = c("ge", "gt", "le", "lt", "eq", "between", "outside"),
  include_low       = TRUE,
  include_high      = TRUE,
  obs_probabilities = TRUE
) {

  parameter   <- rlang::enquo(parameter)
  if (!inherits(try(rlang::eval_tidy(parameter), silent = TRUE), "try-error")) {
    if (is.character(rlang::eval_tidy(parameter))) {
      parameter <- rlang::eval_tidy(parameter)
      parameter <- rlang::ensym(parameter)
    }
  }

  comparator <- match.arg(comparator)

  thresholds <- check_thresholds(thresholds, comparator)

  purrr::map(
    .fcst, det_probabilities, !!parameter, thresholds, comparator,
    include_low, include_high, obs_probabilities
  ) %>%
    new_harp_fcst()
}

make_threshcol <- function(th, comp, il, ih) {
  if (length(th) == 1) {
    return(paste(comp, th, sep = "_"))
  }
  comp1 <- ifelse(il, "ge", "gt")
  comp2 <- ifelse(ih, "le", "lt")
  if (comp == "outside") {
    comp1 <- ifelse(il, "le", "lt")
    comp2 <- ifelse(ih, "ge", "gt")
  }
  paste(comp1, min(th), comp2, max(th), sep = "_")
}

get_comparator_func <- function(comparator) {

  switch(
    comparator,
    "ge" = function(x, y, ...) as.numeric(x >= y),
    "gt" = function(x, y, ...) as.numeric(x > y),
    "le" = function(x, y, ...) as.numeric(x <= y),
    "lt" = function(x, y, ...) as.numeric(x < y),
    "eq" = function(x, y, ...) as.numeric(x == y),
    "between" = function(x, y, include_low, include_high) {
      lower <- min(y)
      upper <- max(y)
      if (include_low && include_high) {
        return(as.numeric(x >= lower & x <= upper))
      }
      if (include_low && !include_high) {
        return(as.numeric(x >= lower & x < upper))
      }
      if (!include_low && include_high) {
        return(as.numeric(x > lower & x <= upper))
      }
      if (!include_low && !include_high) {
        return(as.numeric(x > lower & x < upper))
      }
    },
    "outside" = function(x, y, include_low, include_high) {
      lower <- min(y)
      upper <- max(y)
      if (include_low && include_high) {
        return(as.numeric(x <= lower | x >= upper))
      }
      if (include_low && !include_high) {
        return(as.numeric(x <= lower | x > upper))
      }
      if (!include_low && include_high) {
        return(as.numeric(x < lower | x >= upper))
      }
      if (!include_low && !include_high) {
        return(as.numeric(x < lower | x > upper))
      }
    }
  )
}


