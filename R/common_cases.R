#' Filter to common cases
#'
#' For a fair comparison of models, the verification should only be done for
#' dates and locations that are common to all models. \code{common_cases} takes
#' a harp_fcst object as input and then identifies and filters to only those
#' cases that are common to all of the forecast models in the harp_fcst object.
#' By default this is done with the SID, fcdate and leadtime columns, but extra
#' columns can be added via ...
#'
#' @param .fcst A harp_fcst object
#' @param ... Extra columns from which to determine the common cases. To remove
#'   one of the default columns from the test use -<col>.
#'
#' @return The input data frame with only the common stations and forecast dates
#'   for each forecast model selected.
#' @export
#'
#' @examples
common_cases <- function(.fcst, ...) {
  common_rows <- lapply(
    .fcst,
    function(x) {
      dplyr::arrange(
        dplyr::distinct(
          dplyr::select(
            x,
            .data[["SID"]],
            .data[["fcdate"]],
            .data[["leadtime"]],
            ...
          )
        ),
        .data[["SID"]],
        .data[["fcdate"]],
        .data[["leadtime"]],
        ...
      )
    }
  )

  all_identical <- all(
    purrr::map2_lgl(
      1:(length(common_rows) -1),
      2:length(common_rows),
      ~identical(common_rows[[.x]], common_rows[[.y]])
    )
  )

  if (all_identical) {
    return(.fcst)
  }

  common_rows <- Reduce(
    function(x, y) suppressMessages(dplyr::inner_join(x, y)),
    common_rows
  )

  suppressMessages(
    suppressWarnings(
      join_to_fcst(.fcst, common_rows, force_join = TRUE)
    )
  )

}
