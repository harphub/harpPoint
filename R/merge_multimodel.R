#' Merge sub models of a multimodel ensemble into a single ensemble
#'
#' When multimodel ensembles are read in only the sub models are stored. If the
#' whole ensemble is needed, this function will merge all of the sub models into
#' a single ensemble. Note that there is no renaming of the members so the sub
#' model names are retained. If there are no multi model ensembles, the input is
#' silently returned unaltered.
#'
#' @param .fcst An object of class \code{harp_fcst} as read in by
#'   \link{read_point_forecast}
#' @param keep_sub_models Set to FALSE to discard the sub models as separate
#'   elements in the \code{harp_fcst} list. The default behaviour is to keep
#'   them.
#'
#' @return A \code{harp_fcst} object with one layer - each element is a table of
#'   forecast data for a model.
#' @export
#'
#' @examples
merge_multimodel <- function(.fcst, keep_sub_models = TRUE) {

  is_multimodel  <- unlist(purrr::map(.fcst, inherits, "harp_fcst"))
  num_multimodel <- length(which(is_multimodel))

  if (num_multimodel > 0) {

    merge_submodels <- function(x) {
      purrr::reduce(
        x,
        dplyr::inner_join,
        by = c("SID", "fcdate", "leadtime", "validdate")
      ) %>%
        tibble::as_tibble()
    }

    multimodel <- purrr::map(.fcst[is_multimodel], merge_submodels)

    if (keep_sub_models) {
      multimodel <- c(multimodel, purrr::flatten(.fcst[is_multimodel]))
    }

    if (length(.fcst[!is_multimodel]) > 0) {
      multimodel <- c(multimodel, .fcst[!is_multimodel])
    }

    new_harp_fcst(multimodel)

  } else {

    .fcst

  }

}
