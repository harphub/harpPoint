group_without_threshold <- function(df, group_col, nest = FALSE) {
  if (length(group_col) == 1 && group_col == "threshold") {
    return(df)
  }
  group_col <- group_col[group_col != "threshold"]
  if (nest) {
    return(
      dplyr::group_nest(
        df, dplyr::across(dplyr::all_of(group_col)),
        .key = "grouped_data"
      )
    )
  }
  dplyr::group_by(df, dplyr::across(dplyr::all_of(group_col)))
}

fill_group_na <- function(df, groupings) {

  df <- dplyr::ungroup(df)

  all_groups     <- unique(unlist(groupings))
  group_count    <- sapply(all_groups, function(x) sum(unlist(groupings) == x))
#  groups_to_fill <- names(group_count)[group_count < length(all_groups)]
#  groups_to_fill <- groups_to_fill[groups_to_fill != "threshold"]
  groups_to_fill <- all_groups[all_groups != "threshold"]
  if (length(groups_to_fill) < 1) {
    return(df)
  }

  # fill_fun <- function(group_col, df) {
  #   if (!any(is.na(df[[group_col]]))) return(df)
  #   all_values <- sort(unique(stats::na.omit(df[[group_col]])))
  #   df[[group_col]] <- as.character(df[[group_col]])
  #   if (length(all_values) < 5) {
  #     fill_value <- paste0(" ", paste(all_values, collapse = "; "))
  #   } else {
  #     fill_value <- "All"
  #   }
  #   group_sym <- rlang::sym(group_col)
  #   dplyr::mutate(
  #     df,
  #     !! group_sym := dplyr::case_when(
  #       is.na(!! group_sym) ~ fill_value,
  #       TRUE                ~ !! group_sym
  #     )
  #   )
  # }

  fill_fun <- function(x) {
    if (!any(is.na(x))) {
      return(x)
    }
    all_values <- sort(unique(stats::na.omit(x)))
    x <- as.character(x)
    if (length(all_values) < 5) {
      fill_value = paste(all_values, collapse = "; ")
    } else {
      fill_value <- "All"
    }
    x[is.na(x)] <- fill_value
    x
  }

  dplyr::mutate(df, dplyr::across(dplyr::any_of(groups_to_fill), fill_fun))

  # if (length(groups_to_fill) > 0) {
  #   for(fill_group in groups_to_fill) {
  #     df <- fill_fun(fill_group, df)
  #   }
  # }
  # df
}

bias <- function(fcst, obs, circle) {

  if (missing(circle) || is.null(circle)) {
    return(fcst - obs)
  }

  if (!is.numeric(circle) || length(circle) != 1) {
    cli::cli_abort(c(
      "{.arg circle} must be a length 1 numeric vector.",
      "x" = "You supplied a length {length(circle)} {.cls {class(circle)}}."
    ))
  }

  half <- abs(circle) / 2
  idx <- intersect(which(fcst > half), which(obs < half))
  fcst[idx] <- fcst[idx] - circle

  idx <- intersect(which(obs > half), which(fcst < half))
  obs[idx] <- obs[idx] - circle

  res <- fcst - obs

  idx <- which(res > half)
  res[idx] <- res[idx] - circle

  idx <- which(res < -half)
  res[idx] <- res[idx] + circle

  res
}

#' @export
`[.harp_verif` <- function(x, i, ...) {
  attrs <- attributes(x)
  if (is.character(i)) {
    i <- which(names(x) == i)
  }
  attrs[["names"]] <- names(x)[i]
  x <- NextMethod()
  attributes(x) <- attrs
  x
}


# CIRCULAR STATISTICS functions

#' Circular statistics
#'
#' These functions compute statistics for circular data. They would typically
#' be used when calculating statistics from wind direction
#'
#' @param x,y A numeric vector in degrees or radians.
#' @param degrees Logical. Set to `TRUE` for x (and y) in degrees (the default).
#'   If set to FALSE x (and y) will be assumed to be in radians.
#'
#' @return A numeric value or vector in the same units as the input.
#'
#' @name circle_stats
NULL

#' @rdname circle_stats
#' @export
#'
#' @examples
#' # Doesn't cross zero
#' circle_bias(180, 90)
#' #
#' # Crosses zero
#' circle_bias(355, 5)
#' #
#' # Crosses zero the other way
#' circle_bias(5, 355)
circle_bias <- function(x, y, degrees = TRUE) {
  if (degrees) {
    x <- deg_to_rad(x)
    y <- deg_to_rad(y)
  }
  res <- atan2(sin(x - y), cos(x - y))
  if (degrees) {
    return(rad_to_deg(res))
  }
  res
}

#' @rdname circle_stats
#' @export
#'
#' @examples
#' # All in the 0 - 180 degrees range
#' circle_mean(c(20, 40, 60, 120, 140))
#' #
#' # Crossing zero
#' circle_mean(c(340, 30, 40, 320, 275))
circle_mean <- function(x, degrees = TRUE) {
  if (degrees) {
    x <- deg_to_rad(x)
  }
  res <- atan2(mean(sin(x)), mean(cos(x)))
  if (res < 0) {
    res <- res + 2 * pi
  }
  if (degrees) {
    return(rad_to_deg(res))
  }
  res
}

circle_row_mean <- function(x, degrees = TRUE) {
  if (is.data.frame(x)) {
    numeric_check <- all(vapply(x, is.numeric, logical(1)))
  } else if (is.matrix(x)) {
    numeric_check <- is.numeric(x)
  } else {
    cli::cli_abort(c(
      "Invalid class for {.arg x}",
      "i" = "{.arg x} must be a {.cls matrix} or a {.cls data.frame}.",
      "x" = "You supplied {.arg x} with class {.cls class(x)}."
    ))
  }
  if (!numeric_check) {
    cli::cli_abort(c(
      "Invalid type for {.arg x}",
      "i" = "{.arg x} must be {.type {numeric(1)}.",
      "x" = "You supplied {.arg x} as {.type {typeof(x)}}."
    ))
  }

  if (degrees) {
    x <- deg_to_rad(x)
  }
  res <- atan2(rowMeans(sin(x)), rowMeans(cos(x)))
  if (res < 0) {
    res <- res + 2 * pi
  }
  if (degrees) {
    return(rad_to_deg(res))
  }
  res
}

#' @rdname circle_stats
#' @export
#'
#' @examples
#' # Clustered angles
#' circle_var(c(340, 10, 350, 5, 2, 355, 352, 8))
#' #
#' # Spread angles
#' circle_var(c(0, 90, 180, 270))
circle_var <- function(x, degrees = TRUE) {
  if (degrees) {
    x <- deg_to_rad(x)
  }
  res <- 1 - sqrt(mean(cos(x)) ^ 2 + mean(sin(x)) ^ 2)
  if (degrees) {
    return(rad_to_deg(res))
  }
  res
}

circle_sd <- function(x, degrees = TRUE) {
  if (degrees) {
    x <- deg_to_rad(x)
  }
  R <- sqrt(mean(cos(x)) ^ 2 + mean(sin(x)) ^ 2)
  res <- sqrt(-2 * log(R))
  if (degrees) {
    return(rad_to_deg(res))
  }
  res
}

circle_sd_sq <- function(x, degrees = TRUE) {
  if (degrees) {
    x <- deg_to_rad(x)
  }
  R <- sqrt(mean(cos(x)) ^ 2 + mean(sin(x)) ^ 2)
  res <- -2 * log(R)
  if (degrees) {
    return(rad_to_deg(res))
  }
  res
}

circle_spread <- function(x, degrees = TRUE) {
  if (degrees) {
    x <- deg_to_rad(x)
  }
  res <- sqrt(mean(x))
  if (degrees) {
    return(rad_to_deg(res))
  }
  res
}

deg_to_rad <- function(x) {
  x * pi / 180
}

rad_to_deg <- function(x) {
  x * 180 / pi
}
