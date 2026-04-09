#' @title concatenate columns
#'
#' @description combine the information in the specified columns to create a
#' single column. information from the different columns are separated with a
#' hyphen. If more than one grouping is required, group_count can be used to
#' show where the split is.
#'
#' @param dat dataframe with the columns specified by columns_to_combine_patterns.
#' @param columns_to_combine_patterns  vector of character strings. Each
#' string matches the name of a column in dat.
#' @param combined_names character string vector giving the names of the columns
#' that will hold the newly combined strings. Must be the same length as
#' group_counts, unless group_counts is NA, in which case it must contain a
#' single value. In pub sec this variable is specified by
#' columns_to_combine_combined_names.
#' @param group_counts integer vector. Default is NA. The first element of
#' group_count is used to supply the number of elements of columns_to_combine_patterns
#' that should be used to create the first of combined_names, the second to
#' create the second of combined_names etc.
#'
#' @returns dat with columns concatenated into a single column. If
#' columns_to_combine_patterns is Na, dat is returned unmodified
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'     "group" = c("Fox", "Fox"),
#'     "common_name" = c("Red", "Arctic"),
#'     "age" = c("cub", "adult"),
#'     "cats" = c("lion", "lion"),
#'     "species" = c("African", "Asiatic")
#'     )
#' columns_to_combine_patterns <- c("common_name", "group", "age", "species", "cats")
#' combined_names <- c("Non-cats", "Cats")
#' group_count <- c(3, 2)
#' newdat <- combine_columns(
#'     dat, columns_to_combine_patterns, combined_names, group_count)
#' }
#' @export
combine_columns <- function(
    dat, columns_to_combine_patterns, combined_names, group_counts=NA
) {

  if (all(
    is.na(columns_to_combine_patterns), is.na(combined_names), is.na(group_counts)
  )) {
    return (dat)
  } else if (any(is.na(columns_to_combine_patterns), is.na(combined_names))) {
    stop(
      "One of columns_to_combine_patterns and columns_to_combine_combined_names ",
      "has not been supplied. If one is given, the other must also be ",
      "supplied in the settings."
    )
  }

  if (all(is.na(group_counts))) {
    group_counts <- length(columns_to_combine_patterns)
  }

  if (any(group_counts == 1)) {
    stop(
      "More than one column name must be given in columns_to_combine_patterns for each ",
      "group."
    )
  }

  message("Combining column names.")

  remaining <- columns_to_combine_patterns
  headers_joined <- dat
  for (i in 1:length(group_counts)) {
    num_to_join <- as.numeric(group_counts[i])
    to_combine <- remaining[1:num_to_join]
    remaining <- remaining[(num_to_join + 1):length(remaining)]

    combined_name <- combined_names[i]

    columns_to_combine_patterns <- c()
    for (j in 1:length(to_combine)) {
      column <- get_matching_colnames(dat, to_combine[j])

      if (length(column) == 1) {
        columns_to_combine_patterns <- c(columns_to_combine_patterns, column)

      } else if (length(column) == 0) {
        stop(
          "'", to_combine[j], "' does not match any columns in the data, ",
          "Please contact a developer who will need to edit the ",
          "columns_to_combine_patterns setting."
        )
      } else {
        stop(
          "'", to_combine[j], "' matches more than one column in the data. ",
          "Please contact a developer who will need to edit the ",
          "columns_to_combine_patterns setting."
        )
      }
    }

    message(
      "Concatenating the values in '",
      paste0(columns_to_combine_patterns, collapse = "', '"), "'. Concatenated ",
      "strings will be given as '", combined_name, "'."
    )

    if (combined_name %in% names(dat) & ! combined_name %in% to_combine) {
      warning(
        "'", combined_name, "' will be overwritten by the concatenation ",
        "of '", paste0(to_combine, collapse = "', '"), "'. If it should not ",
        "be overwritten please contact a developer to update ",
        "columns_to_combine_combined_names in the settings"
      )

    }

    headers_joined <- headers_joined %>%
      unite(col = !!sym(combined_name),
            all_of(columns_to_combine_patterns),
            na.rm = TRUE,
            sep = " - ")

  }

  return (headers_joined)

}
