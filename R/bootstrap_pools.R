#' Make pools for block bootstrapping
#'
#' For doing a block bootstrap using \link{bootstrap_verify}, the blocks can be
#' passed as a data frame with a "pool" column telling \link{bootstrap_verify}
#' how to pool the data into blocks. \code{make_bootstrap_pools} is a function
#' to make such a data frame.
#'
#' Typically block bootstrapping would be used if there are serial
#' auto-correlations in the data. If for example auto-correlations are suspected
#' between forecasts, pools could be defined from the \code{fcdate} column to
#' create blocks of data where those auto-correlations are maintained.
#'
#' Pools may be set to overlap, whereby a new pool is created beginning at each
#' new value in \code{pool_col}. The length of a pool should be defined in the
#' units used in \code{pool_col} - if \code{pool_col} is a date-time column,
#' then \code{pool_length} is assumed to be in hours, though the units can be
#' set by adding a qualifier letter: "s" = seconds, "m" = minutes, "h" = hours,
#' "d" = days.
#'
#' @param .fcst A \code{harp_fcst} object
#' @param pool_col The column used to define the pools. Can be the column name,
#'   quoted, or unquoted. If a variable it should be embraced - i.e. wrapped in
#'   `{{}}`
#' @param pool_length The length of a pool. Numeric or a character with a unit
#'   qualifier if \code{pool_col} is in date-time format. The unit qualifier can
#'   be : "s" = seconds, "m" = minutes, "h" = hours, "d" = days.
#' @param overlap Logical. Whether the pools should overlap.
#'
#' @return A data frame with columns from \code{pool_col} and "pool".
#' @export
#'
#' @examples
#' make_bootstrap_pools(ens_point_df, lead_time, 2)
#' make_bootstrap_pools(ens_point_df, lead_time, 2, overlap = TRUE)
#'
#' # pool_col as a variable
#' my_col <- "lead_time"
#' make_bootstrap_pools(ens_point_df, {{my_col}}, 2)
make_bootstrap_pools <- function(
  .fcst, pool_col, pool_length, overlap = FALSE
) {

  pool_col_sym <- rlang::ensym(pool_col)

  input_type <- strsplit(class(.fcst)[1], "_")[[1]][2]

  pools_df <- switch(
    input_type,
    "ens"  = ,
    "det"  = remove_harp_class(dplyr::select(.fcst, !!pool_col_sym)),
    "list" = dplyr::bind_rows(
      lapply(
        .fcst,
        function(x) remove_harp_class(dplyr::select(x, !!pool_col_sym))
      )
    )
  )

  pool_col_is_date <- FALSE

  if (inherits(dplyr::pull(pools_df, !!pool_col_sym), "POSIXct")) {
    pool_col_is_date <- TRUE
    pools_df         <- dplyr::mutate(
      pools_df, dplyr::across(dplyr::everything(), as.numeric)
    )

    if (is.numeric(pool_length)) {
      pool_length <- paste0(pool_length, "h")
    }

    pool_length <- as.numeric(
      gsub("[[:alpha:]]|[[:punct:]]", "", pool_length)
    ) *
      harpIO:::units_multiplier(pool_length)

  }

  if (overlap) {

    last_pool_start <- min(
      which(
        dplyr::pull(pools_df, !!pool_col_sym) >
          max(dplyr::pull(pools_df, !!pool_col_sym)) - pool_length
      )
    )

    pools_df <- purrr::map_dfr(
      seq(1, last_pool_start),
      ~dplyr::mutate(
        overlapping_pool(pools_df, .x, !!pool_col_sym, pool_length),
        pool = .x
      )
    )

    pools_df <- dplyr::distinct(pools_df)

  } else {

    breaks = get_breaks(dplyr::pull(pools_df, !!pool_col_sym), pool_length)

    pools_df <- dplyr::mutate(
      pools_df,
      pool = as.numeric(
        cut(
          !!pool_col_sym, breaks = breaks,
          include.lowest = TRUE, right = FALSE
        )
      )
    )

  }

  if (pool_col_is_date) {
    pools_df <- dplyr::mutate(
      pools_df, {{pool_col}} := harpIO::unix2datetime(!!pool_col_sym)
    )
  }

  pools_df

}

get_breaks <- function(x, break_length) {

  x_range <- range(x)

  breaks <- seq(min(x_range), max(x_range), by = break_length)

  if (max(breaks) < max(x_range)) {
    breaks[(length(breaks) + 1)] <- max(breaks) + break_length
  }

  breaks

}

overlapping_pool <- function(x, row_start, col, res) {

  x       <- x[row_start:nrow(x), ]
  col     <- rlang::enquo(col)
  max_val <- dplyr::pull(x, !!col)[1] + res
  dplyr::filter(x, !!col < max_val)

}

remove_harp_class <- function(x) {
  structure(x, class = grep("harp", class(x), value = TRUE, invert = TRUE))
}
