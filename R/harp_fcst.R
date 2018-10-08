# Constructor for harp_fcst objects

new_harp_fcst <- function(x) {
  stopifnot(is.list(x))
  structure(
    x,
    class = "harp_fcst"
  )
}

#' @export
`[.harp_fcst` <- function(x, i, ...) {
  new_harp_fcst(NextMethod())
}

#' @export
c.harp_fcst <- function(x, ...) {
  new_harp_fcst(NextMethod())
}
