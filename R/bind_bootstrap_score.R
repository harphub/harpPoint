#' Bind output of bootstrap_score / pooled_bootstrap_score
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param ... A list of outputs from `bootstrap_score()` /
#'   `pooled_bootstrap_score()`
#'
#' @return An object of class `harp_bootstrap`
#' @export
#'
bind_bootstrap_score <- function(...) {

  lifecycle::deprecate_stop(
    "0.1.0",
    "bind_bootstrap_score()",
    "bind_point_verif()"
  )

  check_class <- function(x) {
    inherits(x, "harp_bootstrap")
  }

  dots <- list(...)
  if (length(dots) == 1) dots <- dots[[1]]

  dots <- dots[sapply(dots, function(x) !is.null(x))]
  if (!all(sapply(dots, check_class))) {
    bad_classes <- which(!sapply(dots, check_class))
    bad_classes <- paste0("..", paste(bad_classes, sep = ", .."))
    stop(
      bad_classes, " are not 'harp_bootstrap' objects.\n",
      "Only outputs of 'bootstrap_score()' and 'pooled_bootstrap_score()' can be used."
    )
  }

  if (length(dots) == 1) return(dots[[1]])

  list_names <- unique(unlist(lapply(dots, names)))

  res <- lapply(
    list_names,
    function(x) Reduce(rbind, lapply(dots, function(y) y[[x]]))
  )

  names(res) <- list_names

  new_harp_bootstrap(res)

}
