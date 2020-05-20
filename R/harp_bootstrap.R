# Constructer for harp_bootstrap objects

new_harp_bootstrap <- function(x) {
  stopifnot(is.list(x))
  stopifnot(sort(names(x)) == c("confidence_limits", "confidence_of_differences"))
  structure(
    x,
    class = "harp_bootstrap"
  )
}

#' @export
print.harp_bootstrap <- function(x, ...) {
  .name <- names(x)
  print_fun <- function(.x, .y, ...) {
    cli::cat_bullet(.x, col = "#32527B", bullet_col = "#32527B", bullet = "circle_dotted")
    print(.y, ...)
    cat("\n")
  }
  purrr::walk2(.name, x, print_fun, ...)
}

