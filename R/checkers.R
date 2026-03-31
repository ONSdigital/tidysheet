#' @title Check for reserved words in settings
#'
#' @description Check if any reserved words are used in the settings.
#' The list of reserved words is taken from the column names created by
#' xlsx_cells. Using them for new column names can cause issues that
#' are hard to identify.
#' The variables that must not be assigned a reserved word at time of writing
#' are group_col, nested_col_1 , nested_col_2, group_row, nested_row_1,
#' nested_row_2, and left_headers. However a conservative approach
#' is taken so that we don't need to specify new variables here
#' if and when new settings are added that also must not use reserved words.
#'
#' @param dict dataframe. A dictionary-style dataframe with setting names in the
#' key column and the settings themselves in the value column.
#' @param exclude Dictionary keys that should not be checked because they are
#' expected to contain reserved words.
#'
#' @returns boolean. TRUE if no settings use reserved words, otherwise an error
#' is raised
#'
#' @examples
#' \dontrun{
#' dict_with_error <- tibble(
#'   "key" = c("header_identifier", "left_headers", "table_split_dirs"),
#'   "item" = list("safe", c("character", "numeric"), "row")
#'   )
#'
#' dict_no_error <- tibble(
#'   "key" = c("header_identifier", "table_split_dirs"),
#'   "item" = list("safe", "row")
#'   )
#'
#'  check_reserved_words(dict_with_error, c("table_split_dirs", NA))
#'  check_reserved_words(dict_no_error, c("table_split_dirs", NA))
#' }
#' @export
check_reserved_words <- function(dict, exclude) {
  reserved_words <- c(
    'date', 'sheet', 'address', 'row', 'col', 'is_blank', 'content', 'data_type',
    'error', 'logical', 'numeric', 'character', 'character_formatted', 'formula',
    'is_array', 'formula_ref', 'formula_group', 'comment', 'height', 'width',
    'col_outline_level', 'style_format', 'local_format_id'
  )

  if (all(is.na(exclude))) {
    exclude <- as.character()
  } else if (any(is.na(exclude))) {
    exclude <- exclude[!is.na(exclude)]
  }

  to_exclude <- exclude[exclude %in% dict$key]

  if (length(to_exclude) > 0) {
    dict_filtered <- filter(dict, !key %in% to_exclude)
  } else {
    dict_filtered <- dict
  }

  flattened_list <- unlist(dict_filtered["item"][[1]])

  found_words <- intersect(flattened_list, reserved_words)

  if (length(found_words) > 0) {
    stop(
      "Reserved word(s): '", paste(found_words, collapse="', '"),
      "' used in the settings. Please ask a developer to ",
      "replace reserved words with alternatives in the settings. If the ",
      "setting that uses the reserved word is safe, to suppress this error, ",
      "you can specify the name of the setting in ",
      "exclude_from_reserved_word_check. e.g. col_patterns_with_values_to_drop does ",
      "create a column or delete a column, so it can use a reserved word."
    )
  } else {
    return(TRUE)
  }

}


#' @title Check if given string is present in the character column
#'
#' @description Check if given string is present in the character column of
#' a dataframe improted using tidyxl::xlsx_cells(). Created for checking if
#' a given string is in a dropdown type cell (e.g. in MHCLG data 2024), but
#' could be used to check if a string exists anywhere in a dataset.
#'
#' @param dat dataframe imported using tidyxl::xlsx_cells
#' @param pattern string. A regular expression that only matches the
#' dropdown cell. In pub sec this variable is specified by dropdown_pattern.
#'
#' @returns message or warning
#'
#' @examples
#' \dontrun{#'
#' dat <- data.frame(
#'   row = c(1:3),
#'   col = rep(1,3),
#'   character = c("Title", "some other info", "England")
#' )
#'
#' check_dropdown("Eng", dat) # found
#' check_dropdown("Wales", dat) # not found
#' }
#' @export
check_dropdown <- function(pattern, dat) {
  # Check if there are regex instructions
  if (!is.na(pattern)) {

    dropdown_cell_matches <- grepl(pattern, dat$character)

    if (any(dropdown_cell_matches, na.rm = TRUE)) {
      message(
        "'", pattern, "'",
        " found in information above table, so it is assumed that the dropdown ",
        "contains the correct selection."
      )
    } else {
      stop(
        "'", pattern, "' is expected to be in ",
        "the dropdown selection (as set in the data dictionary ",
        "sheet_structure) and has not been found in the information above ",
        "the table. Please check the correct dropdown has been selected in ",
        "interim data. If not, change the selection, save it and re-run ",
        "pre-processing. If the correct selection is made already, contact a ",
        "developer."
      )
    }
  }

}


#' @title Check for duplicated column names
#'
#' @description Check for duplicated column names and raise an informative error
#' if any are found.
#'
#' @param dat dataframe to check for duplicated names.
#'
#' @returns Raises an error if duplicated names are found.
#'
#' @examples
#' \dontrun{
#'
#' dat <- data.frame(source = "scottish_gov", source = c("A", "B"), value = 1:2)
#' check_for_duplicate_names(dat)
#' }
check_for_duplicate_names <- function(dat) {
  ids <- str_to_lower(names(dat))
  duplicates <- unique(ids[duplicated(ids)])

  if (length(duplicates) > 0) {
    stop(
      "The following column names are duplicated when converted to ",
      "lowercase: '", paste0(duplicates, collapse = "', "), "'. Please either ",
      "set different column names, or use the columns_to_rename_patterns and ",
      "columns_to_rename_names settings to rename the offending column (this ",
      "could for example be a column in the left block that takes it's name ",
      "from the original data)."
    )
  }
}
