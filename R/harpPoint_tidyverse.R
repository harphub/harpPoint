#' Filter a \code{harp_fcst} object.
#'
#' Works on each table in the \code{harp_fcst} object in the same way as \link[dplyr]{filter}
#'
#' @param .fcst a harp_fcst object.
#' @param ... Arguments as in \link[dplyr]{filter}
#' @import dplyr
#' @export
filter.harp_fcst <- function(.fcst, ...) {
  new_harp_fcst(purrr::map(.fcst, dplyr::filter, ...))
}

#' Select columns from tables in a \code{harp_fcst} object.
#'
#' Works in the same way as \link[dplyr]{select}, except runs on all tables in
#' the \code{harp_fcst} object. This means that only common columns between the
#' objects can safely be selected.
#'
#' @param .fcst A harp_fcst object
#' @param ... Arguments as in \link[dplyr]{select}
#'
#' @export
select.harp_fcst <- function(.fcst, ...) {
  new_harp_fcst(purrr::map(.fcst, dplyr::select, ...))
}

#' Arrange columns from tables in a \code{harp_fcst} object.
#'
#' Works in the same way as \link[dplyr]{arrange}, except runs on all tables in
#' the \code{harp_fcst} object. This means that only common columns between the
#' objects can safely be arranged.
#'
#' @param .fcst A harp_fcst object
#' @param ... Arguments as in \link[dplyr]{arrange}
#'
#' @export
arrange.harp_fcst <- function(.fcst, ...) {
  new_harp_fcst(purrr::map(.fcst, dplyr::arrange, ...))
}

#' Mutate columns from tables in a \code{harp_fcst} object.
#'
#' Works in the same way as \link[dplyr]{mutate}, except runs on all tables in
#' the \code{harp_fcst} object. This means that only common columns between the
#' objects can safely be arranged.
#'
#' @param .fcst A harp_fcst object
#' @param ... Arguments as in \link[dplyr]{mutate}
#'
#' @export
mutate.harp_fcst <- function(.fcst, ...) {
  new_harp_fcst(purrr::map(.fcst, dplyr::mutate, ...))
}

#' Transmute columns from tables in a \code{harp_fcst} object.
#'
#' Works in the same way as \link[dplyr]{transmute}, except runs on all tables in
#' the \code{harp_fcst} object. This means that only common columns between the
#' objects can safely be arranged.
#'
#' @param .fcst A harp_fcst object
#' @param ... Arguments as in \link[dplyr]{transmute}
#'
#' @export
transmute.harp_fcst <- function(.fcst, ...) {
  new_harp_fcst(purrr::map(.fcst, dplyr::transmute, ...))
}

#' Join all models into a single ensemble.
#'
#' The function is most useful for finding common cases between models.
#'
#' @param .fcst A harp_fcst object with any multimodel data merged with
#'   \link{merge_multimodel}.
#' @param join_type The type of join to perform. See \link[dplyr]{join}.
#' @param name The name of the resulting model.
#' @param ... Other arguments to \link[dplyr]{join}.
#'
#' @return
#' @export
#'
#' @examples
join_models <- function(.fcst, join_type = "inner", name = "joined_models", ...) {

  valid_joins <- c("inner", "left", "right", "full", "semi", "anti")

  if (length(intersect(join_type, valid_joins)) < 1) {
    stop(
      paste(
        "Invalid join_type:", join_type[1], "\n ",
        "Must be one of: 'inner', 'left', 'right', 'full', 'semi', 'anti'"
      )
    )
  }

  is_multimodel  <- unlist(purrr::map(.fcst, inherits, "harp_fcst"))
  num_multimodel <- length(which(is_multimodel))

  if (num_multimodel > 0) {
    stop(
      "Multi model ensemble detected. Run merge_multimodel on .fcst first",
      call. = FALSE
    )
  }

  UseMethod("join_models")

}

#' @export
join_models.harp_fcst <- function(
  .fcst,
  join_type = "inner",
  name = "joined_models",
  by = c("SID", "fcdate", "validdate", "leadtime"),
  ...
) {
  join_func <- get(paste0(join_type, "_join"), envir = asNamespace("dplyr"))
  out <- list()
  out[[name]] <- purrr::reduce(.fcst, join_func, by = by, ...) %>%
    tibble::as_tibble()
  new_harp_fcst(out)
}
