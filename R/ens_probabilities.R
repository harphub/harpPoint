#' Compute probabilities of threshold exceedence for ensemble forecasts
#'
#' @inheritParams ens_verify
#' @return A \code{harp_list} object with each data frame having columns for threshold,
#' fcst_prob and obs_prob instead of the columns for each member forecast.
#' @export
ens_probabilities <- function(
  .fcst,
  thresholds,
  comparator   = c("ge", "gt", "le", "lt", "eq", "between", "outside"),
  include_low  = TRUE,
  include_high = TRUE,
  parameter    = NULL
) {
  UseMethod("ens_probabilities")
}

#' @export
ens_probabilities.default <- function(
  .fcst,
  thresholds,
  comparator   = c("ge", "gt", "le", "lt", "eq", "between", "outside"),
  include_low  = TRUE,
  include_high = TRUE,
  parameter    = NULL
) {

  num_members <- length(harpCore::member_colnames(.fcst))

  comparator <- match.arg(comparator)

  ens_probs <- harpCore::ens_prob(
    .fcst, thresholds, comparator, include_low, include_high,
    obs_col = !!rlang::enquo(parameter)
  )

  ens_probs <- ens_probs[c(
    grep(
      "fcst_prob|threshold|obs_prob", colnames(ens_probs),
      value = TRUE, invert = TRUE
    ),
    intersect(c("threshold", "fcst_prob", "obs_prob"), colnames(ens_probs))
  )]

  class(ens_probs)               <- c("harp_ens_probs", class(ens_probs))
  attr(ens_probs, "num_members") <- num_members

  ens_probs

}

#' @export
ens_probabilities.harp_list <- function(
  .fcst,
  thresholds,
  comparator   = c("ge", "gt", "le", "lt", "eq", "between", "outside"),
  include_low  = TRUE,
  include_high = TRUE,
  parameter    = NULL
) {

  parameter   <- rlang::enquo(parameter)

  comparator <- match.arg(comparator)
  thresholds <- check_thresholds(thresholds, comparator)

  purrr::map(
    .fcst, ens_probabilities, thresholds, comparator, include_low, include_high,
    !!parameter
  ) %>%
    lapply(function(x) {
      x <- harpCore::as_harp_df(x)
      class(x) <- c("harp_ens_probs", class(x))
      x
    }) %>%
    harpCore::as_harp_list()
}

