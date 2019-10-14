#' Bind harp verification objects into a single object
#'
#' For plotting it may be desirable to combine various differen verifications
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
#' @examples
bind_point_verif <- function(...) {

  # Check attributes
  expected_attributes <- c("parameter", "start_date", "end_date", "num_stations")
  dots <- list(...)
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
  start_date   <- paste(unique(purrr::map_chr(dots, attr, "start_date")), collapse = ", ")
  end_date     <- paste(unique(purrr::map_chr(dots, attr, "end_date")), collapse = ", ")
  num_stations <- paste(unique(purrr::map_chr(dots, attr, "num_stations")), collapse = ", ")

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
