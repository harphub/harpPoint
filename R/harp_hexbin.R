#' Create a bi-variate histogram of forecast, observation pairs
#'
#' Values of forecasts and observations are binned into bands whereby the
#' density of forecast, observation pairs for each bin is calculated. Under the
#' hood, the data are binned into hexagons using \code{\link[hexbin]{hexbin}}.
#' Hexagons are used since they have symmetry with nearest neighbours unlike
#' square bins, and at plot time they are the polygon with the maximum number of
#' sides that tessellate.
#'
#' @param .fcst A `harp_df` data frame or a `harp_list`.
#' @param parameter The column containing the parameter. Can be unquoted, a
#'   quoted string, or an embraced variable name (i.e {{var}}).
#' @param groupings The groupings for which to compute the binned densities.
#'   Must be a vector of strings, or a list of vectors of strings.
#' @param num_bins The number of bins into which to partition the observations.
#' @param show_progress Logical. Whether to show a progress bar.
#' @param ... Arguments for methods.
#'
#' @return A `harp_verif` list.
#' @export
#'
bin_fcst_obs <- function(
  .fcst,
  parameter,
  groupings     = "lead_time",
  num_bins      = 30,
  show_progress = TRUE,
  ...
) {
  UseMethod("bin_fcst_obs")
}

#' @rdname bin_fcst_obs
#' @param fcst_model The name of the forecast model. If `.fcst` does not
#'   contain a `fcst_model`, a new column is created and populated with this
#'   value. If a `fcst_model` column exists, the value in the column is replaced
#'   with this value.
#'
#' @export
bin_fcst_obs.harp_det_point_df  <- function(
  .fcst,
  parameter,
  groupings     = "lead_time",
  num_bins      = 30,
  show_progress = TRUE,
  fcst_model    = NULL,
  ...
) {

  if (!is.list(groupings)) {
    groupings <- list(groupings)
  }

  col_names <- colnames(.fcst)
  parameter <- rlang::ensym(parameter)
  chr_param <- rlang::as_name(parameter)

  if (length(grep(chr_param, col_names)) < 1) {
    cli::cli_abort("No column found for {chr_param}")
  }

  fcst_model <- parse_fcst_model(.fcst, fcst_model)
  .fcst[["fcst_model"]] <- fcst_model

  res <- list()
  res[["det_summary_scores"]] <- purrr::map(
    groupings, compute_hexbin, .fcst, chr_param, num_bins, show_progress
  ) %>%
    purrr::list_rbind() %>%
    fill_group_na(groupings) %>%
    dplyr::mutate(fcst_model = fcst_model, .before = dplyr::everything())

  structure(
    add_attributes(
      res[which(vapply(res, nrow, numeric(1)) > 0)],
      harpCore::unique_fcst_dttm(.fcst),
      !!parameter,
      harpCore::unique_stations(.fcst),
      groupings
    ),
    class = "harp_verif"
  )

}

#' @export
bin_fcst_obs.harp_ens_point_df_long  <- function(
  .fcst,
  parameter,
  groupings     = "lead_time",
  num_bins      = 30,
  show_progress = TRUE,
  fcst_model    = NULL,
  ...
) {

  if (!is.list(groupings)) {
    groupings <- list(groupings)
  }

  # groupings <- lapply(groupings, function(x) c(x, "member"))

  col_names <- colnames(.fcst)
  parameter <- rlang::ensym(parameter)
  chr_param <- rlang::as_name(parameter)

  if (length(grep(chr_param, col_names)) < 1) {
    cli::cli_abort("No column found for {chr_param}")
  }

  fcst_model <- parse_fcst_model(.fcst, fcst_model)
  .fcst[["fcst_model"]] <- fcst_model

  res <- list()
  res[["ens_summary_scores"]] <- purrr::map(
    groupings, compute_hexbin, .fcst, chr_param, num_bins, show_progress
  ) %>%
    purrr::list_rbind() %>%
    fill_group_na(groupings) %>%
    dplyr::mutate(fcst_model = fcst_model, .before = dplyr::everything())

  structure(
    add_attributes(
      res[which(vapply(res, nrow, numeric(1)) > 0)],
      harpCore::unique_fcst_dttm(.fcst),
      !!parameter,
      harpCore::unique_stations(.fcst),
      groupings
    ),
    class = "harp_verif"
  )

}

#' @rdname bin_fcst_obs
#' @export
bin_fcst_obs.harp_ens_point_df <- function(
  .fcst,
  parameter,
  groupings     = "lead_time",
  num_bins      = 30,
  show_progress = TRUE,
  fcst_model    = NULL,
  ...
) {

 parameter <- rlang::ensym(parameter)

 bin_fcst_obs(
   harpCore::pivot_members(.fcst),
   !!parameter,
   groupings,
   num_bins,
   show_progress,
   fcst_model,
   ...
 )

}

#' @export
bin_fcst_obs.harp_list <- function(
  .fcst,
  parameter,
  groupings     = "lead_time",
  num_bins      = 30,
  show_progress = TRUE,
  ...
) {

  parameter <- rlang::ensym(parameter)

  list_to_harp_verif(
    purrr::imap(
      .fcst,
      ~bin_fcst_obs(
        .x, !!parameter, groupings, num_bins, show_progress, fcst_model = .y
      )
    )
  )
}


compute_hexbin <- function(
  compute_group, fcst_df, parameter, num_bins, show_progress
) {

  fcst_df     <- group_without_threshold(fcst_df, compute_group, nest = TRUE)
  group_vars  <- grep(
    "grouped_data", colnames(fcst_df), invert = TRUE, value = TRUE
  )
  group_names <- glue::glue_collapse(group_vars, sep = ", ", last = " & ")

  score_text <- cli::col_blue(glue::glue("Hexbin for {group_names}"))
  if (show_progress) {
    pb_name <- score_text
  } else {
    pb_name <- FALSE
    message(score_text, appendLF = FALSE)
    score_text <- ""
  }

  res <- dplyr::transmute(
    fcst_df,
    dplyr::across(group_vars, ~.x),
    num_cases    = purrr::map_int(.data[["grouped_data"]], nrow),
    num_stations = {
      if (is.element("SID", group_vars)) {
        1L
      } else {
        purrr::map_int(.data[["grouped_data"]], ~length(unique(.x[["SID"]])))
      }
    },
    hexbin = purrr::map(
      .data[["grouped_data"]],
      ~hexbin_df(.x[[parameter]], .x[["fcst"]], num_bins, parameter),
      .progress = pb_name
    )
  )

  message(score_text, cli::col_green(cli::symbol[["tick"]]))

  res
}


hexbin_df <- function(x, y, nbins, prm) {
  xrange <- range(x)
  yrange <- range(y)
  if (diff(xrange) == 0) {
    xrange[1] <- xrange[1] - abs(xrange[1]) * 0.01
    xrange[2] <- xrange[2] + abs(xrange[2]) * 0.01
  }
  if (diff(yrange) == 0) {
    yrange[1] <- yrange[1] - abs(yrange[1]) * 0.01
    yrange[2] <- yrange[2] + abs(yrange[2]) * 0.01
  }
  hexes <- hexbin::hexbin(x, y, xbins = nbins, xbnds = xrange, ybnds = yrange)
  dplyr::rename(
    dplyr::mutate(
      tibble::as_tibble(hexbin::hcell2xy(hexes)),
      count = hexes@count
    ),
    obs  = .data[["x"]],
    fcst = .data[["y"]]
  )
}
