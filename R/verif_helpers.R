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
  groups_to_fill <- names(group_count)[group_count < length(all_groups)]
  groups_to_fill <- groups_to_fill[groups_to_fill != "threshold"]

  fill_fun <- function(group_col, df) {
    if (!any(is.na(df[[group_col]]))) return(df)
    all_values <- sort(unique(stats::na.omit(df[[group_col]])))
    df[[group_col]] <- as.character(df[[group_col]])
    if (length(all_values) < 5) {
      fill_value <- paste0(" ", paste(all_values, collapse = "; "))
    } else {
      fill_value <- "All"
    }
    group_sym <- rlang::sym(group_col)
    dplyr::mutate(
      df,
      !! group_sym := dplyr::case_when(
        is.na(!! group_sym) ~ fill_value,
        TRUE                ~ !! group_sym
      )
    )
  }

  if (length(groups_to_fill) > 0) {
    for(fill_group in groups_to_fill) {
      df <- fill_fun(fill_group, df)
    }
  }
  df
}
