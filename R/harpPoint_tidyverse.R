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

#' Rename columns from tables in a \code{harp_fcst} object.
#'
#' Works in the same way as \link[dplyr]{rename}, except runs on all tables in
#' the \code{harp_fcst} object. This means that only common columns between the
#' objects can safely be renamed.
#'
#' @param .fcst A harp_fcst object
#' @param ... Arguments as in \link[dplyr]{rename}
#'
#' @export
rename.harp_fcst <- function(.fcst, ...) {
  new_harp_fcst(purrr::map(.fcst, dplyr::rename, ...))
}

#' Rename multiple columns from tables in a \code{harp_fcst} object.
#'
#' Works in the same way as \link[dplyr]{rename_with}, except runs on all tables
#' in the \code{harp_fcst} object. This means that only common columns between
#' the objects can safely be renamed.
#'
#' @param .fcst A harp_fcst object
#' @param ... Arguments as in \link[dplyr]{rename_with}
#'
#' @export
rename_with.harp_fcst <- function(.fcst, ...) {
  new_harp_fcst(purrr::map(.fcst, dplyr::rename_with, ...))
}

#' Pull columns from tables in a \code{harp_fcst} object.
#'
#' Works in the same way as \link[dplyr]{pull}, except runs on all tables in
#' the \code{harp_fcst} object. This means that only common columns between the
#' objects can safely be pulled.
#'
#' @param .fcst A harp_fcst object
#' @param ... Arguments as in \link[dplyr]{pull}
#'
#' @export
pull.harp_fcst <- function(.fcst, ...) {
  new_harp_fcst(purrr::map(.fcst, dplyr::pull, ...))
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
#' the \code{harp_fcst} object.
#'
#' @param .fcst A harp_fcst object
#' @param ... Arguments as in \link[dplyr]{mutate}
#'
#' @export
mutate.harp_fcst <- function(.fcst, ...) {
  new_harp_fcst(purrr::map(.fcst, dplyr::mutate, ...))
}

#' Mutate selected columns from tables in a \code{harp_fcst} object.
#'
#' Works in the same way as \link[dplyr]{mutate_at}, except runs on all tables
#' in the \code{harp_fcst} object.
#'
#' @param .fcst A harp_fcst object
#' @param ... Arguments as in \link[dplyr]{mutate}
#'
#' @export
mutate_at.harp_fcst <- function(.fcst, .mutate_vars, .mutate_funs, ...) {
  new_harp_fcst(
    purrr::map(
      .fcst,
      dplyr::mutate_at,
      dplyr::vars(.mutate_vars),
      dplyr::funs(.mutate_funs),
      ...
    )
  )
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
#' @param .fcst A harp_list object with any multimodel data merged with
#'   \link{merge_multimodel}.
#' @param join_type The type of join to perform. See \link[dplyr]{join}.
#' @param name The name of the resulting model.
#' @param ... Other arguments to \link[dplyr]{join}.
#'
#' @return A `harp_df` data frame.
#' @export
#'
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
join_models.harp_list <- function(
  .fcst,
  join_type = "inner",
  name = "joined_models",
  by = c("SID", "fcst_dttm", "valid_dttm", "lead_time"),
  ...
) {
  # Make sure deterministic models get unique forecast columns
  .fcst <- purrr::imap(
    .fcst,
    ~{
      colnames(.x)[colnames(.x) == "fcst"] <- paste0(.y, "_fcst")
      colnames(.x)[colnames(.x) == "forecast"] <- paste0(.y, "_fcst")
      .x
    }
  )

  # Do the join
  join_func <- get(paste0(join_type, "_join"), envir = asNamespace("dplyr"))
  purrr::reduce(.fcst, join_func, by = by, ...) %>%
    tibble::as_tibble() %>%
    harpCore::as_harp_df()
}

#' @export
join_models.harp_df <- function(.fcst, ...) {
  .fcst
}

#' @export
bind_rows.harp_fcst <- function(..., .id = NULL) {
  NextMethod()
}

#' dplyr verbs for lists
#'
#' When you have a list of data frames, such as the output to a verification
#' function, you may want to wrangle data in those data frames at the same time.
#' This can be achieved using the dplyr verb followed by _list. For data frames
#' where the function is applicable the modified data frame is returned. If the
#' verb fails (e.g. because the specified columns don't exist), the data frame
#' is silently returned unmodified
#'
#' @param .list A list of data frames
#' @param ... Other arguments to the dplyr verb
#' @return A list with the same attrbutes as the input `.list`
#' @seealso /link[dplyr]{mutate}, /link[dplyr]{filter}, /link[dplyr]{select}
#' @name dplyr_list
NULL

#' @rdname dplyr_list
#' @export
mutate_list <- function(.list, ...) {

  stopifnot(is.list(.list))

  possibly_mutate <- function(df1, ...) {
    poss_func <- purrr::possibly(dplyr::mutate, otherwise = NA)
    df <- poss_func(df1, ...)
    if (!is.data.frame(df)) df <- df1
    df
  }

  list_attr <- attributes(.list)
  .list <- purrr::map(.list, dplyr::ungroup) %>%
    purrr::map(possibly_mutate, ...)
  attributes(.list) <- list_attr
  .list
}

#' @rdname dplyr_list
#' @export
filter_list <- function(.list, ...) {

  stopifnot(is.list(.list))

  possibly_filter <- function(df1, ...) {
    poss_func <- purrr::possibly(dplyr::filter, otherwise = NA)
    df <- poss_func(df1, ...)
    if (!is.data.frame(df)) df <- df1
    df
  }

  list_attr <- attributes(.list)
  .list <- purrr::map(.list, dplyr::ungroup) %>%
    purrr::map(possibly_filter, ...)
  attributes(.list) <- list_attr
  .list
}

#' @rdname dplyr_list
#' @export
select_list <- function(.list, ...) {

  stopifnot(is.list(.list))

  possibly_select <- function(df1, ...) {
    poss_func <- purrr::possibly(dplyr::select, otherwise = NA)
    df <- poss_func(df1, ...)
    if (!is.data.frame(df)) df <- df1
    df
  }

  list_attr <- attributes(.list)
  .list <- purrr::map(.list, dplyr::ungroup) %>%
    purrr::map(possibly_select, ...)
  attributes(.list) <- list_attr
  .list
}


