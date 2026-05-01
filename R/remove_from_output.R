#' @title Remove unwanted rows and columns from the output dataframe
#'
#' @description Remove blank rows, duplicated rows, and unwanted columns created
#' by xlsx_cells. These are columns such as 'row', 'col', 'local_format_id',
#' some of which will have been used as part of tidying the data, but others
#' are not used by this pipeline.
#'
#' @details
#' For more information and examples see documentation for remove_is_blank_rows,
#' drop_rows_with_values, deduplicate_data, and remove_unwanted_cols.
#'
#' @param dat dataframe.
#' @param col_patterns_with_values_to_drop character vector. Regular expressions
#' to match the names of the columns containing the values that match
#' value_patterns_to_drop.
#' @param value_patterns_to_drop character vector. Regular expressions to match
#' the values whose rows are to be dropped.
#' @param xlsx_cells_names character string vector. Column names of the source
#' data which will have been imported using xlsx_cells.
#'
#' @returns dataframe with specified rows, blank rows, repeat rows, and
#' unwanted columns from xlsx_cells removed. A warning is raised if repeat
#' rows are found.
#' @export
remove_from_output <- function(
    dat, col_patterns_with_values_to_drop, value_patterns_to_drop,
    xlsx_cells_names
) {

  blanks_removed <- remove_is_blank_rows(dat)

  value_rows_removed <- drop_rows_with_values(
    blanks_removed, col_patterns_with_values_to_drop, value_patterns_to_drop
  )
  ignore_cols <- xlsx_cells_names[xlsx_cells_names != "numeric"]
  arrange_by_cols <- c("row", "col")[
    c("row", "col") %in% names(value_rows_removed)
  ]
  duplicates_removed <- deduplicate_data(
    value_rows_removed, ignore_cols, arrange_by_cols
  )

  xlsx_cells_columns_removed <- remove_unwanted_cols(duplicates_removed)

  return(xlsx_cells_columns_removed)
}


#' @title Remove rows marked as blank
#'
#' @description Once data have been beheaded (unpivotted) any rows marked as
#' blank that are leftover from xlsx_cells data are no longer required. This
#' function removes those rows.
#'
#' @param dat dataframe that contains a boolean column called is_blank.
#'
#' @returns dat with is_blank == TRUE rows removed
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(is_blank = c(TRUE, FALSE), numeric = c(NA, 1))
#' remove_is_blank_rows(dat)
#' }
#' @export
remove_is_blank_rows <- function(dat) {
  if ("is_blank" %in% names(dat)){
    message("Removing 'is_blank' junk rows.")
    return(filter(dat, is_blank == FALSE))
  } else {
    return(dat)
  }
}


#' @title Remove rows with missing values in specified columns
#'
#' @description Remove rows with missing values in columns that match any
#' of the provided regular expressions. Rows are removed if any of the
#' specified columns contain blanks. In other words the only rows
#' to be kept are those that have a non-NA entry in ALL specified columns.
#' Note that this function is written to be performed on unpivotted (beheaded)
#' data.
#'
#' @param dat dataframe.
#' @param patterns vector of character strings. Each is a regular expression
#' that matches a column name that you want to check for blanks. In pub sec this
#' variable is specified by col_patterns_to_drop_NA_rows.
#'
#' @returns dataframe with rows containing missing values in the matched
#' columns removed. Warnings are given stating which rows have been removed,
#' or that there was not a single column name that matched a pattern.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   row = 4:6,
#'   col = 1,
#'   year = c(2020, NA, 2021),
#'   value = c(100, NA, NA),
#'   description = c("A", "B", NA),
#'   address = c("A4", "A5", "A6")
#' )
#' patterns <- c("year", "value")
#' clean_data <- drop_rows_with_NA(data, patterns)
#' print(clean_data)
#' }
#' @export
drop_rows_with_NA <- function(dat, patterns=NA) {

  if (any(is.null(patterns), length(patterns) == 0, is.na(patterns))) {
    return(dat)
  }

  if (!is.character(patterns)) {
    return(dat)
  }

  message(
    "Removing rows with blanks in the columns that match the patterns ",
    "specified in the 'col_patterns_to_drop_NA_rows' setting."
  )

  matched_columns <- c()

  for (pattern in patterns) {
    current_matches <- names(dat)[grepl(pattern, names(dat))]

    # get a list of the other patterns to use in warning messages
    other_patterns <- setdiff(pattern, patterns)
    if (length(other_patterns) == 0) {other_patterns <- "(none)"} else{
      other_patterns <- paste(other_patterns, collapse = "', ")
    }

    if (length(current_matches) > 1) {
      warning(
        "There is more than one column that matches the pattern '", pattern,
        "'. The intended column has been set to have rows with blanks removed ",
        "but this has not been done due to multiple matches. Rows with blanks ",
        "in the columns that match any other patterns: '", other_patterns,
        "' will still be removed. Some rows that should have been dropped may ",
        "have been retained and cause issues later on. If so, please ",
        "contact a developer to update the problematic pattern."
      )
      next()

    } else if (length(current_matches) == 0) {
      warning(
        "There are no columns that match the pattern '", pattern,
        "'. The intended column has been set to have rows with blanks removed ",
        "but this has not been done. Rows with blanks ",
        "in the columns that match any other patterns: '", other_patterns,
        "' will still be removed. Some rows that should have been dropped may ",
        "have been retained and cause issues later on. If so, please ",
        "contact a developer to update the problematic pattern."
      )
      next()
    } else {

      matched_columns <- c(matched_columns, current_matches)
    }
  }

  matched_columns <- unique(matched_columns)

  if (length(matched_columns) == 0) {
    "No rows removed."
    return(dat)
  }

  rows_with_na <- which(
    rowSums(
      is.na(select(dat, all_of(matched_columns)))
    ) > 0
  )

  if (length(rows_with_na) > 0) {

    cleaned_data <- dat %>%
      filter(rowSums(is.na(select(., all_of(matched_columns)))) == 0)

    removed <- dat %>%
      anti_join(cleaned_data, by = names(dat))

    full_rows <- setdiff(unique(dat$row), unique(cleaned_data$row))
    full_col_numbers <- setdiff(unique(dat$col), unique(cleaned_data$col))
    if (!is.null(full_col_numbers)) {
      full_cols <- get_col_letters_as_string(dat, full_col_numbers)
    }

    partials <- dat %>%
      mutate(
        full_row = row %in% full_rows,
        full_col = col %in% full_col_numbers
      ) %>%
      filter(full_row == FALSE & full_col == FALSE) %>%
      anti_join(cleaned_data, names(dat)) %>%
      pull(address)


    if (length(full_rows[!is.na(full_rows)]) > 0) {
      warning(
        "The following source data rows have been removed: ",
        paste0(full_rows, collapse = ", "), "."
      )
    }

    if (length(full_col_numbers[!is.na(full_col_numbers)]) > 0) {
      warning(
        "The following source data columns have been removed: ",
        full_cols, "."
      )
    }

    if (length(partials) > 0) {
      warning(
        "The following cells have been removed even though there are values ",
        "in cells in both the same column and row: '",
        paste0(partials, collapse = "', '"), "'."
      )
    }


    return(cleaned_data)

  } else {

    message(
      "No blanks found to remove in any of the following columns: '",
      paste0(matched_columns, collapse = "', '"), "'."
    )

    return(dat)
  }

}


#' @title Drop rows with a specified value in a specified column
#'
#' @description  Look for specified values in specified columns and drop rows
#' where a match is found.
#'
#' Multiple values can be specified but there must be a col_pattern value for
#' every value_pattern, even if they are all to be dropped from the same column.
#'
#' @param dat dataframe. Must contain only one column name that matches
#' col_pattern.
#' @param col_patterns character vector. Regular expressions to match the
#' names of the columns containing the values that match value_patterns. In pub
#' sec this variable is specified with col_patterns_with_values_to_drop.
#' @param value_patterns character vector. Regular expressions to match the
#' values whose rows are to be dropped. In pub sec this variable is specified
#' with value_patterns_to_drop.
#'
#' @returns datafame with rows removed.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'     a = c("ant", "apple", NA, "anger"),
#'     b = c("badger", "banana", "boredom", NA)
#'     )
#' drop_rows_with_values(dat, c("b", "a"), c("bad", "ang"))
#' }
#' @export
drop_rows_with_values <- function(dat, col_patterns, value_patterns) {

  if (all(is.na(col_patterns), is.na(value_patterns))) {
    return(dat)
  }
  message("Dropping rows with specific values indicated in the settings.")

  if (any(is.na(col_patterns), is.na(value_patterns))) {
    stop(
      "If either the setting for col_patterns_with_values_to_drop, or the ",
      "setting for value_patterns_to_drop are specified, they both must be. ",
      "One is present but one is missing. "
    )
  }

  if (length(col_patterns) != length(value_patterns)) {
    stop(
      "The setting for col_patterns_with_values_to_drop must have the same number of ",
      "elements as the setting for value_patterns_to_drop.")
  }


  for (i in 1:length(col_patterns)) {

    column <- get_matching_colnames(dat, col_patterns[i])

    if (length(column) == 0) {
      warning(
        "col_patterns_with_values_to_drop is specified as '", col_patterns[i], "' ",
        "in the settings but no matching ",
        "column has been found. Values matching the values_to_drop_pattern ('",
        paste0(value_patterns), "') will be retained. If this causes issues ",
        "please contact a developer."
      )
      next

    } else if (length(column) > 1) {
      warning(
        "multiple matching column names have been found for ",
        "col_patterns_with_values_to_drop in the settings ('", col_patterns[i], "'). ",
        "Rows with values matching value_patterns_to_drop ('",
        value_patterns[i], "') will be retained. If this causes issues please ",
        "contact a developer."
      )
      next
    }

    message(
      "Dropping rows where the value matches the pattern '", value_patterns[i],
      "' in column '", column, "'."
    )

    dat <- dat %>%
      filter(
        is.na(!!sym(column)) |
          str_detect(!!sym(column), value_patterns[i], negate=TRUE)
      )
  }
  return(dat)
}


#' @title Remove repeated data rows
#'
#' @description Remove repeated rows from a dataset based on the content of
#' columns that were not originally in the xlsx_cells data. i.e. these rows
#' look like duplicates in the Excel data but not the xlsx_cells data (which
#' has one row per cell).
#'
#' @details
#' Columns specified in ignore_cols are excluded from comparison with other
#' rows.
#'
#' A hash is generated for each row based on the content columns, and duplicates
#' are identified by comparing these hashes.
#'
#' @param dat dataframe.
#' @param ignore_cols character vector of column names to be ignored.
#' @param arrange_by character vector of column names by which to arrange the
#' output dataframe.
#'
#' @returns dataframe where only the first occurrence of each duplicate row is
#' retained.
#'
#' @examples
#' \dontrun{
#' dat <- tibble(
#'     row = rep(c(1:3), times = 2),
#'     col = rep(c(1, 2), each = 3),
#'     description = c(NA, NA, NA, "A", "A", "B"),
#'     value = c(NA, NA, NA, 1, 1, 2)
#'     )
#' deduplicate_data(dat)
#' }
#' @export
deduplicate_data <- function(dat, ignore_cols = NA, arrange_by = NA) {

  if (!is.data.frame(dat) & !is_tibble(dat)) {
    stop("Input data must be a tibble or dataframe.")
  }

  if (nrow(dat) == 0) {
    stop("Input data frame is empty. Returning the original dataframe.")
  }

  message("Removing duplicates of data rows")

  if (!all(is.na(ignore_cols))) {
    content_cols <- setdiff(names(dat), ignore_cols)
  } else {
    content_cols <- names(dat)
  }

  # Create a hash for each row - this will be the same string for rows where the
  # content_cols contain exactly the same information.
  dat <- dat %>%
    mutate(row_hash = apply(
      select(., all_of(content_cols)), 1, function(row) digest::digest(row)
    ))

  # Keep only the first row of each hash so that only repeated rows are removed
  # (It should not matter which of the replicates is kept).
  deduplicated <- dat %>%
    group_by(row_hash) %>%
    slice(1) %>%
    ungroup() %>%
    select(-row_hash)

  if(! all(arrange_by[!is.na(arrange_by)] %in% names(deduplicated))) {
    stop("Not all arrange_by columns are in the data.")
  }

  if (all(!is.na(arrange_by))) {
    arranged <- arrange(deduplicated, across(all_of(arrange_by)))
  } else {
    arranged <- deduplicated
  }

  # inform the user which rows and columns have been impacted so they can
  # more easily investigate.
  if (nrow(arranged) != nrow(dat)) {
    # row is always in xlsx_cells data, which this functin was designed for
    # but this function could equally be used on a normal datframe, in which
    # case the message sent to the user won't be quite so explicit
    if ("row" %in% names(dat)) {
      removed <- dat %>%
        group_by(row_hash) %>%
        filter(row_number() > 1) %>%
        ungroup()

      removed_rows <- pull(distinct(removed, row), row)
      removed_cols <- pull(distinct(removed, col), col)

      warning(
        nrow(dat)-nrow(arranged), " row(s)/cell(s) of duplicated data found ",
        "and removed. Affected rows of the Excel data: ",
        paste0(removed_rows, collapse = ", ")
      )
    } else {

      warning(
        nrow(dat)-nrow(arranged), " row(s) of duplicated data found and ",
        "removed."
      )
    }
  } else {
    message("No duplicated rows found.")
  }

  return(arranged)
}


#' @title Remove unwanted columns from xlsx_cells imported data
#'
#' @description xlsx_cells data always has the same column names. Many of these
#' are not required post processing, so this function removes them if they
#' exist.
#'
#' @param dat dataframe imported using tidyxl::xlsx_cells.
#'
#' @return dataframe with columns removed.
#'
#' @examples
#' \dontrun{
#' dat <- tibble::tibble(
#'     Year = c(2023, 2023),
#'     Value = c(1, 2),
#'     row = c(1, 1),
#'     local_format_id = NA
#' )
#' remove_unwanted_cols(dat)
#' }
#' @export
remove_unwanted_cols <- function(dat) {
  unwanted_cols <- c(
    "row", "col", "content", "is_blank", "data_type", "error", "logical",
    "date", "character_formatted", "formula", "is_array", "formula_ref",
    "formula_group", "comment", "height", "width", "style_format",
    "local_format_id", "row_outline_level", "col_outline_level"
  )

  cols_to_remove_indices <- which(unwanted_cols %in% names(dat))
  cols_to_remove <- unwanted_cols[cols_to_remove_indices]

  dat <- select(dat, -c(any_of(cols_to_remove)))

  return(dat)

}
