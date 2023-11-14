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
