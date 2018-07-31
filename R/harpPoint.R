##' @useDynLib harpPoint
##' @importFrom Rcpp sourceCpp
.onUnload <- function (libpath) {
  library.dynam.unload("harpPoint", libpath)
}
