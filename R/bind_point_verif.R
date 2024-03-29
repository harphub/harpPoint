#' Bind harp verification objects into a single object
#'
#' For plotting it may be desirable to combine various different verifications
#' into a single object. For example, verification for different parameters for
#' the same set of forecast models.
#'
#' @param ... Verification objects that are to be combined. Can be individual
#'   objects or a list.
#'
#' @return A harp verification object with all attributes moved up to columns
#'   and combined in the new attributes.
#' @export
#'
bind_point_verif <- function(...) {
  UseMethod("bind_point_verif")
}

#' @export
bind_point_verif.list <- function(...) {



  # Check attributes
  expected_attributes <- c("parameter", "start_date", "end_date", "num_stations")
  dots <- list(...)
  if (length(dots) == 1 && !any(sapply(dots[[1]], inherits, "data.frame"))) {
    dots <- dots[[1]]
  }
  if (all(sapply(dots, inherits, "harp_verif"))) {
    dots <- lapply(dots, param_to_col)
    res <- list_to_harp_verif(dots)
    attrs <- attributes(res)
    groupings <- unlist(purrr::list_flatten(attrs[["group_vars"]]))
    res <- lapply(
      res,
      dplyr::mutate,
      dplyr::across(
        dplyr::any_of(groupings),
        ~dplyr::case_when(is.na(.x) ~ "All", TRUE ~ as.character(.x))
      )
    )
    attributes(res) <- attrs
    return(structure(res, class = "harp_verif"))
  }
  dots_check <- purrr::map_lgl(
    dots,
    ~ length(intersect(names(attributes(.x)), expected_attributes)) == length(expected_attributes)
  )
  if (!any(dots_check)) {
    check_failed <- TRUE
    if (length(dots) == 1) {
      dots_check <- purrr::map_lgl(
        dots[[1]],
      ~ length(intersect(names(attributes(.x)), expected_attributes)) == length(expected_attributes)
      )
      if (all(dots_check)) {
        check_failed <- FALSE
        dots         <- dots[[1]]
      }
    }
    if (check_failed) {
      stop("One or more of the inputs does not appear to be a harp point verification object.", call. = FALSE)
    }
  }

  # Combine attributes for output
  parameter    <- paste(unique(purrr::map_chr(dots, attr, "parameter")), collapse = ", ")
  start_date   <- as.character(min(as.numeric(purrr::map_chr(dots, attr, "start_date"))))
  end_date     <- as.character(max(as.numeric(purrr::map_chr(dots, attr, "end_date"))))
  num_stations <- paste(
    unique(
      purrr::map_chr(dots, ~as.character(attr(.x, "num_stations")))
    ),
    collapse = ", "
  )

  # Bring attributes up to columns
  verif <- purrr::map(
    dots,
    ~ purrr::map(
      .x,
      dplyr::mutate,
      parameter    = attr(.x, "parameter"),
      dates        = paste(attr(.x, "start_date"), attr(.x, "end_date"), sep = "-"),
      num_stations = attr(.x, "num_stations")
    )
  )
  rm(dots)

  # Bind the data frames
  list_names <- c("ens_summary_scores", "ens_threshold_scores", "det_summary_scores", "det_threshold_scores")
  list_names <- intersect(list_names, unlist(lapply(verif, names)))
  verif      <- purrr::map(
    list_names,
    ~ purrr::map_dfr(verif, .x)
  )
  names(verif)       <- list_names
  elements_with_data <- which(purrr::map_lgl(verif, ~ nrow(.x) > 0))
  verif              <- verif[elements_with_data]

  # Add attributes back to the output
  attr(verif, "parameter")    <- parameter
  attr(verif, "start_date")   <- start_date
  attr(verif, "end_date")     <- end_date
  attr(verif, "num_stations") <- num_stations

  verif

}

#' @export
bind_point_verif.harp_verif <- function(...) {
  dots <- list(...)
  dots <- lapply(dots, param_to_col)
  res <- list_to_harp_verif(dots)
  attrs <- attributes(res)
  groupings <- unlist(purrr::list_flatten(attrs[["group_vars"]]))
  res <- lapply(
    res,
    dplyr::mutate,
    dplyr::across(
      dplyr::any_of(groupings),
      ~dplyr::case_when(is.na(.x) ~ "All", TRUE ~ as.character(.x))
    )
  )
  attributes(res) <- attrs
  structure(res, class = "harp_verif")
}

param_to_col <- function(x) {
  attrs <- attributes(x)
  x <- lapply(x, function(d) {
    if (!is.element("parameter", colnames(d))) {
      d[["parameter"]] <- attrs[["parameter"]]
    }
    d
  })
  attributes(x) <- attrs
  x
}
