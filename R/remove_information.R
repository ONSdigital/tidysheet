#' @title Remove unwanted cells from xlsx_cells data
#'
#' @description Remove cells using specified cell addresses and cells containing
#' strings where the font is the same colour as the background.
#'
#' @details
#' The cells removed by this function are usually specified as needing removal
#' because not doing so results in errors during beheading (unpivotting).
#'
#' @param dat data that has been imported using xlsx_cells.
#' @param cells_to_remove character string.
#' @param input_filepath character string.
#' @param hidden_strings bool. TRUE, FALSE or NA. Default is NA. FALSE leads to
#' the same behaviour as NA. In pub sec this variable is specified by
#' remove_hidden_strings_bool.
#'
clean_xlsx_cells_data <- function(
    dat, cells_to_remove = NA, input_filepath, hidden_strings = NA
) {

  unwanted_cells_removed <- remove_unwanted_cells(dat, cells_to_remove)

  cells_removed <- remove_hidden_character_strings(
    unwanted_cells_removed, input_filepath, hidden_strings
  )

  return(cells_removed)

}


#' @title Remove cells from input data that are not needed.
#'
#' @description Remove rows from an xlsx_cells imported dataframe
#' using cell addresses to identify which rows to remove.
#'
#' @param dat dataframe imported using tidyxl::xlsx_cells
#' @param cells vector of character strings. The Excel addresses of
#' cells to be removed from data e.g. 'A1'. In pub sec this variable is
#' specified by cells_to_remove
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(address = c("A1", "A2", "A3"), numeric = 1:3)
#'
#' cells_to_remove <- c("A1", "A2")
#'
#' remove_unwanted_cells(dat, cells_to_remove)
#' }
#' @export
remove_unwanted_cells <- function(dat, cells = NA) {

  if (all(is.na(cells))) {return(dat)}

  message ("Removing cells specified by address.")

  filtered <- dat
  for (i in 1:length(cells)) {

    cell <- cells[i]

    if (! cell %in% dat$address) {
      warning(
        "Cell ", cell, " has not been found in the data, so cannot be removed."
      )
    } else {

      type <- dat$data_type[dat$address == cell]
      content <- dat[[type]][dat$address == cell]
      removed_content <- ifelse(
        is.null(content), "nothing", paste0("'", content, "'")
      )

      filtered <- filter(filtered, address != cell)

      warning(
        "Cell ", cell, " containing ", removed_content, " has been removed."
        )
    }
  }

  return(filtered)

}


#' @title Remove first cell from a dataset matching each regular expression
#'
#' @description Remove rows from an xlsx_cells imported dataframe if the
#' character value is the first match for the supplied regular expression.
#'
#' It should only be used when absolutely necessary as it carries the risk of
#' removing a cell that is actually wanted. To mitigate this risk only a given
#' number of non-blank rows are checked for the pattern using n_row.
#'
#' This function was written for cases where metadata (title or units) are given
#' in the same row as the headers, and therefore interfere with beheading.
#' If the cells to remove are metadata cells this function must be called after
#' the required information has been extracted from those cells.
#'
#' Non-metadata cells could theoretically be removed using this function, but
#' first check if there is a more appropriate function.
#'
#' @param dat A data frame imported using tidyxl::xlsx_cells.
#' @param patterns vector of character strings. Each must be a regular
#' expression used to identify the unwanted cells. More than one pattern may be
#' given, but there should only be one pattern for each cell to be removed:
#' Only the first match to each pattern will be removed. If two patterns are
#' given and the first match for each is the same cell, only that one cell will
#' be removed. In pub sec this variable is specified by
#' metadata_cells_to_remove_patterns.
#' @param n_row integer (optional, defaults to 5) An integer specifying the
#' number of rows to check for matches to the metadata_cells_to_remove_patterns.
#' This is based on the rows of dat, not original row number (stated in the
#' column 'row') i.e. if this function is called after blank rows have been
#' removed, it will only check that many rows. In pub sec this variable is
#' specified by populated_rows_to_check_for_metadata_to_remove.
#'
#' @returns A data frame with the specified metadata cells removed. If multiple
#' matches for the pattern are found, only the first instance is removed
#' and a warning is raised. If the pattern is not found, a warning is raised
#' and no rows are removed.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'  character = c("Title", "Units", "Data1", "Data2", "Data3"),
#'  address = c("A1", "A2", "B1", "B2", "B3")
#'  )
#' remove_metadata_cells(dat, "(?i)units")
#' }
#' @export
remove_metadata_cells <- function (dat, patterns=NA, n_row=5) {

  if (all(is.na(patterns))) {
    return (dat)
  }

  if (is.na(n_row)) {
    n_row <- 5
  }

  message(
    "Removing unwanted metadata cells from the first ", n_row,
    " remaining rows."
  )

  rows <- dat %>%
    distinct(row) %>%
    arrange(row) %>%
    pull(row)

  rows_to_check <- rows[1:n_row]

  rows_to_check <- filter(dat, row %in% rows_to_check)

  cells_to_remove <- c()
  for (i in 1:length(patterns)) {
    cell_to_remove <- rows_to_check %>%
      filter(str_detect(character, patterns[i])) %>%
      # only keep the first match - this relies on the data being in order
      # of row and col, which it automatically should be, but arrange just in
      # case.
      arrange(row, col) %>%
      filter(row_number() == 1) %>%
      pull(address)

    cells_to_remove <- c(cell_to_remove, cells_to_remove)
  }


  if (length(cells_to_remove) == 0) {

    original_patterns_string <- ifelse(
      length(patterns > 1),
      paste0( "['", paste0(patterns, collapse = "', '"), "']"),
      paste0("'", patterns, "'")
    )

    warning(
      "No cells were found to match the metadata_cells_to_remove_patterns: ",
      original_patterns_string, ". This may cause data to be unpivotted ",
      "incorrectly - you will get a lot of validation questions or further ",
      "errors. If this is the case please contact a developer. ",
      "If you do not get other errors, and the validation questions are not ",
      "unusual, you do not need to take further action."
    )
    return (dat)

  } else {

    filtered_dat <- filter(dat, !address  %in% cells_to_remove)
    warning(
      "The following cells have been removed (specified by ",
      "metadata_cells_to_remove_patterns): ",
      paste0(unique(cells_to_remove), collapse = ", ")
    )

    return(filtered_dat)
  }

}


#' @title Remove hidden character strings in the first header row.
#'
#' @description Remove cells whose text is the same colour as the background.
#' This function was created because in pub sec scot gov HRA 2021-22 there are
#' hidden character strings in the
#' first header row that are hidden by being the same colour as the background.
#' These need to be removed so that un-pivotting works correctly.
#'
#' @param dat dataframe imported using tidyxl::xlsx_cells()
#' @param filepath string. Filepath for input data to detect cell formats.
#' @param remove_cell bool. TRUE, FALSE or NA. Default is NA. FALSE leads to the
#' same behaviour as NA. In pub sec this variable is specified by
#' remove_hidden_strings_bool.
#'
#' @returns dataframe. dat with rows removed for the identified cells. A warning
#' is raised if rows are removed.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(address = c("A1"),
#'                   local_format_id = c(1))
#' filepath <- D:/.../interim/test_dat.xlsx
#' remove_hidden_strings_bool <- "TRUE"
#'
#' remove_hidden_character_strings(dat, filepath, remove_hidden_strings_bool)
#' }
#' @export
remove_hidden_character_strings <- function(dat, filepath, remove_cell=NA) {

  if (is.na(remove_cell)) {
    return(dat)
  } else if (remove_cell == FALSE) {
    return(dat)
  }

  message(
    "Removing cells where the text is the same colour as the background ",
    "(hidden info)."
  )

  # get all the formatting info from excel
  formats <- xlsx_formats(filepath)
  # we are only interested in the colour 'codes' for this situation
  tint <- formats$local$font$color$tint
  rgb <- formats$local$font$color$rgb
  white_text_codes <- which(rgb=="FFFFFFFF" & is.na(tint))

  hidden_characters_to_remove <- dat %>%
    filter(local_format_id %in% white_text_codes) %>%
    select(address) %>% pull()

  if (length(hidden_characters_to_remove) > 0) {
    dat <- dat %>%
      filter(local_format_id %in% white_text_codes == FALSE)

    warning(
      "The following cells have been removed as they have been flagged ",
      "as having hidden and text that is not required: ",
      paste(hidden_characters_to_remove, collapse = ", "), "."
    )
  }
  return(dat)
}


#' @title remove empty columns or empty rows from xlsx_cells data
#'
#' @description Remove rows from a dataframe imported using tidyxl::xlsx_cells
#' if they belongs to a row or column in the Excel data that is empty.
#'
#' If a cell contains a formula the result of which is 0, this is counted as a
#' blank row by default. If this is not the desired behaviour set
#' formula_zero_as_blank to FALSE.
#'
#' @param dat dataframe imported using tidyxl::xlsx_cells
#' @param direction character string. Either "row" or "col". If "row", rows
#' relating to cells in an empty row are removed. If "col", rows relating
#' to cells in an empty column are removed.
#' @param formula_zero_as_blank boolean. Defaults to TRUE. When TRUE if a cell
#' contains a formula the result of which is 0, this is counted as a blank.
#'
#' @returns dat with rows removed. A warning is given stating which rows or
#' columns have been removed.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'     address = c("A1", "A2", "B1", "B2"),
#'     row = rep(1:2, times = 2),
#'     col = rep(1:2, each = 2),
#'     is_blank = c(TRUE, TRUE, FALSE, TRUE),
#'     character = c(NA, NA, "hello", NA),
#'     data_type = c("blank", "blank", "character", "blank"),
#'     formula = NA,
#'     numeric = NA
#'     )
#'  remove_empty_lines(dat, "col")
#'  remove_empty_lines(dat, "row")
#' }
#' @export
remove_empty_lines <- function(dat, direction, formula_zero_as_blank=TRUE) {

  if (!direction %in% c("row", "col")) {
    stop("direction must be either 'row' or 'col', not ", direction)
  }

  required_cols <- c("data_type", "character", "formula", "numeric", direction)
  if (!all(required_cols %in% names(dat))) {
    stop(
      "At least one of the following columns is missing from dat: '",
      paste0(required_cols, collapse = "', '"), "'. All are required. Was dat ",
      "not imported using tidyxl::xlsx_cells?"
    )
  }

  message("Removing empty ", direction, "s.")

  blanks <- dat %>%
    mutate(
      no_data = case_when(
        data_type == "blank" ~ TRUE,
        data_type == "character" & stringr::str_trim(character) == "" ~ TRUE,
        TRUE ~ FALSE
      )
    )

  if (formula_zero_as_blank == TRUE) {
    blanks_updated <- blanks %>%
      mutate(
        no_data = case_when(
          !is.na(formula) & numeric == 0 ~ TRUE,
          TRUE ~ no_data
        )
      )
  } else {
    blanks_updated <- blanks
  }

  empty_lines_removed <- blanks_updated %>%
    group_by(!!sym(direction)) %>%
    mutate(col_all_blank = all(no_data == TRUE)) %>%
    filter(col_all_blank == FALSE) %>%
    select(-c(col_all_blank, no_data)) %>%
    ungroup()

  ncol_dat <- nrow(distinct(dat, !!sym(direction)))
  ncol_new_dat <- nrow(distinct(empty_lines_removed, !!sym(direction)))
  num_removed <- ncol_dat - ncol_new_dat

  # raise a warning to notify the user which rows or columns have been removed
  if (num_removed > 0){
    if (direction == "row") {
      remaining <- unique(empty_lines_removed$row)
      all <- unique(dat$row)
      removed <- setdiff(all, remaining)
      removed_for_message <- paste0(removed, collapse = ", ")
    } else if (direction == "col") {
      remaining <- unique(empty_lines_removed$col)
      all <- unique(dat$col)
      removed <- setdiff(all, remaining)
      removed_for_message <- get_col_letters_as_string(dat, removed)
    }
    warning(
      num_removed, " empty ", direction, "s removed from source data: ",
      removed_for_message
    )
  } else {
    message("No ", direction, "s to remove.")
  }

  return(empty_lines_removed)
}


#' @title Remove rows marked as blank
#'
#' @description Once data have been beheaded (unpivotted) any rows marked as
#' blank that are leftover from xlsx_cells data are no longer required. This
#' function removes those rows.
#'
#' @param dat dataframe
#'
#' @returns dat with is_blank == TRUE rows removed
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(is_blank = c(TRUE, FALSE), numeric = c(NA, 1))
#' remove_is_blank_rows(dat)
#' }
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
#'
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
#' @return dat dataframe without columns removed.
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
    "local_format_id", "table_title", "row_outline_level",
    "col_outline_level"
  )

  cols_to_remove_indices <- which(unwanted_cols %in% names(dat))
  cols_to_remove <- unwanted_cols[cols_to_remove_indices]

  dat <- select(dat, -c(any_of(cols_to_remove)))

  return(dat)

}


#' @title Remove lines that were unwanted columns in the Excel data
#'
#' @description Remove all rows relating to a column in data imported using
#' tidyxl::xlsx_cells if a pattern is matched by a character in that column.
#'
#' More than one column can be removed by providing more than one pattern.
#'
#' If the name of the column to be removed is blank, or if it is unstable (i.e.
#' writing a pattern to match it is hard/impossible), a pattern that matches a
#' nearby column can be used along with offset. This requires that you are
#' confident about the order of the columns.
#'
#' @param dat dataframe imported using tidyxl::xlsx_cells.
#' @param patterns vector of character strings that are regular expressions,
#' each one matching the name of a column in the raw data to be removed. In
#' pub sec this variable is specified by columns_to_remove_patterns.
#' @param offset vector of integers. Must be either NA or the same length as
#' patterns. If the column to remove is not named but it's location is known in
#' relation to a named column, specify the pattern for the named column and use
#' offset to move x columns to the left (negative) or right (positive). In pub
#' sec this variable is specified by columns_to_remove_offset.
#' @param first_row integer. The first row in which column headings are found.
#' @param header_count integer. The number of header rows.
#'
#' @returns dataframe. dat with rows relating to the matched columns removed.
#' If a pattern matches character strings in more than one column it is not
#' removed and a warning is given. This does not affect the use of any other
#' patterns provided in the patterns vector. If no match is found in the
#' data for a patterns pattern a warning is given.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   "row" = c(1, 1, 1, 2, 2, 2),
#'   "col" = c(1, 2, 3, 1, 2, 3),
#'   "address" = c("A1", "B1", "C1", "A2", "B2", "C2"),
#'   "numeric" = c(NA, NA, NA, NA, 1, 100),
#'   "character" = c("name", "id", "value", "first row", NA, NA),
#'   "data_type" = c(rep("character", 4), rep("numeric", 2))
#' )
#' # view as it would be in excel:
#' rectify(dat)
#'
#' output <- remove_columns(dat, "id", 0, 1, 1)
#' rectify(output)
#' }
#' @export
remove_columns <- function(dat, patterns, offset, first_row, header_count) {

  if (all(is.na(patterns))) {
    return(dat)
  }
  message(
    "Removing rows relating to columns specified as requiring removal in the ",
    "settings by columns_to_remove_patterns."
  )

  # offset has to be a number but may not have been specified for every pattern:
  if (all(is.na(offset))) {
    offset <- rep(0, length(patterns))
  } else if (any(is.na(offset))) {
    offset <- replace(offset, is.na(offset), 0)
  }
  offset <- as.integer(offset)

  if (length(patterns) != length(offset)) {
    stop("A column to remove offset must be provided for every pattern")
  }

  target_rows <- c(first_row:(first_row + header_count - 1))
  columns_to_remove <- identify_columns_to_remove(
    dat, patterns, offset, target_rows
    )

  columns_removed <- dat %>%
    filter(col %in% columns_to_remove == FALSE)

  if (length(columns_to_remove) > 0) {

    column_letters_removed <- get_col_letters_as_string(dat, columns_to_remove)
    warning(
      "The following column(s) of the source data have been removed: ",
      paste0(column_letters_removed, collapse = ", "),
      ". If these columns contain information you ",
      "require, please contact a developer to update the settings."
    )
  }
  return(columns_removed)
}


#' @title Identify header cells to remove from xlsx_cells data.
#'
#' @description Use regular expressions to identify which column of the data
#' they appear in. Only header rows are checked for matches.
#'
#' If a pattern is matched in more than one column none of those matches are
#' returned as columns to remove. This does not affect the use of any other
#' patterns provided.
#'
#' @param dat dataframe imported using xlsx_cells.
#' @param patterns vector of character strings that are regular expressions,
#' each one matching the name of a column in the raw data.
#' @param offset vector of integers. Must be either NA or the same length as
#' patterns. If the column to remove is not named but it's location is known in
#' relation to a named column, specify the pattern for the named column and use
#' offset to move x columns to the left (negative) or right (positive).
#' @param target_rows vector of integers. The numbers of the rows in which
#' column headings are given
#'
#' @returns vector of character strings giving the column numbers identified.
#' If no match is found in the data for a pattern a warning is given.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   "row" = c(1, 1, 1, 2, 2, 2),
#'   "col" = c(1, 2, 3, 1, 2, 3),
#'   "address" = c("A1", "B1", "C1", "A2", "B2", "C2"),
#'   "numeric" = c(NA, NA, NA, NA, 1, 100),
#'   "character" = c("name", "id", "value", "first row", NA, NA),
#'   "data_type" = c(rep("character", 4), rep("numeric", 2))
#' )
#' # view as it would be in excel:
#' rectify(dat)
#'
#' identify_columns_to_remove(dat, "id", NA, 1)
#' }
#' @export
identify_columns_to_remove <- function(dat, patterns, offset, target_rows) {

  if (all(is.na(offset))) {offset <- rep(0, length(patterns))}

  all_cols_to_remove <- c()

  for (i in 1:length(patterns)) {
    col_identified <- dat %>%
      filter(row %in% target_rows & str_detect(character, patterns[i])) %>%
      distinct(col) %>%
      pull(col)

    if (length(col_identified) == 1) {
      col_to_remove <- col_identified + offset[i]
      all_cols_to_remove <- c(all_cols_to_remove, col_to_remove)

    } else if (length(col_identified) > 1) {
      stop(
        "More than one column matched the pattern '", patterns[i],
        "' given for columns_to_remove_patterns in the settings. This setting ",
        "will therefore be ignored. Please contact a developer."
      )

    } else if (length(col_identified) == 0) {
      warning(
        "No columns matched the pattern '", patterns[i],
        "' given for columns_to_remove_patterns in the settings. ",
        "Please contact a developer if data do not look as expected."
      )
    }
  }
  return (all_cols_to_remove)
}


