#' @title Prepare table for un-pivotting
#'
#' @description Prepare data for use in unpivot_data. This includes removing
#' unwanted cells, rows, and columns, and completing partial information with
#' information from other cells.
#'
#' @details See documentation for convert_date_to_char, extend_row_value,
#' remove_empty_lines, remove_metadata_cells, combine_rows_by_column and
#' remove_columns for more information.
#'
#' @param dat Dataframe created using tidyxl::xlsx_cells(), in which metadata
#' above the table has been removed.
#' @param extend_row_pattern character string. Regular expressions used to
#' identify the rows that need added information from the row above or below.
#' @param extend_row_order character string. Either 'forward' or 'reverse'. Used
#' to determine if the added information is placed before or after original row
#' value.
#' @param extend_row_with  character string. Either 'above' or 'below'. Used to
#' determine if the added information is from above or below the row that
#' matches pattern.
#' @param cell_removal_patterns character string vector. Each must be a regular
#' expression used to identify unwanted cells. More than one pattern may be
#' given, but there should only be one pattern for each cell to be removed:
#' Only the first match to each pattern will be removed. If two patterns are
#' given and the first match for each is the same cell, only that one cell will
#' be removed. In pub sec this variable is specified by
#' metadata_cells_to_remove_patterns.
#' @param rows_to_check_for_removal integer. An integer specifying the
#' number of rows to check for matches to the metadata_cells_to_remove_patterns.
#' This is based on the rows of dat, not original row number (stated in the
#' column 'row') i.e. if this function is called after blank rows have been
#' removed, it will only check that many rows. If cell_removal_patterns are
#' specified, but rows_to_check_for_removal is NA, 5 will be used.
#' @param combine_start_row_identifier character string. A regular expression
#' that first appears in the first of the header rows that needs to be combined.
#' @param combine_end_row_identifiercharacter string. A word or pattern that
#' first appears in the last of the header rows that needs to be combined.
#' @param columns_to_remove_patterns character string vector. Each is a regular
#' expression that matches the name of a column in the raw data to be removed.
#' @param columns_to_remove_offset integer vector. Must be either NA or the same
#' length as columns_to_remove_patterns If the column to remove is not named but
#' it's location is known in relation to a named column, specify the pattern for
#' the named column and use offset to move x columns to the left (negative) or
#' right (positive).
#' @param columns_to_create character vector. The names of columns that will
#' be created during behead. Used to calculate the number of header rows.
#' @param first_header_row integer. the number of the first row in which there
#' are headers.
#'
#' @returns dataframe that is ready for un-pivotting.
#'
#' @export
clean_xlsx_cells_table_data <- function(
    dat, extend_row_pattern = NA, extend_row_order = NA, extend_row_with = NA,
    cell_removal_patterns = NA, rows_to_check_for_removal = NA,
    combine_start_row_identifier = NA, combine_end_row_identifier = NA,
    columns_to_remove_patterns = NA, columns_to_remove_offset = NA,
    columns_to_create = NA, first_header_row = NA
) {

  # Where dates are given as headings, and this info appears in the date
  # column of xlsx_cells data we need to be able to access it in the character
  # column. (eg in MHCLG/DLUHC borrowing and investments)
  date_as_character <- convert_date_to_char(dat)

  # extend_row_value should in the future be re-factored to work on un-pivotted
  # data so that it is consistent with other functions, then moved out of this
  # function (not a priority so not done yet).
  rownames_extended <- extend_row_value(
    date_as_character, extend_row_pattern, extend_row_order, extend_row_with
  )

  # without this step, if there is a blank column in the middle of the dataset,
  # the columns after the blank column will not get correctly beheaded.
  # This does not always remove 'blank' columns created through an editing
  # error by the suppliers (e.g. in dluhc revenue expenditure budget 2020-21),
  # because for some reason the cells are not seen as blank but as character
  # with a string of "..." - these are removed later by remove_no_data_rows
  no_empty_rows <- remove_empty_lines(rownames_extended, "row")
  no_empty_cols <- remove_empty_lines(no_empty_rows, "col")

  # Sometimes metadata cells are on or after the first header row rather than
  # above the data. Other unwanted non-metadata cells at the top of the data can
  # also be removed this way.
  metadata_cell_removed <- remove_metadata_cells(
    no_empty_cols, cell_removal_patterns, rows_to_check_for_removal
  )

  # Combine cells that have been artificially split by the supplier
  # e.g. in DLUHC (MHCLG) council tax and NNDR
  rows_combined <- combine_rows_by_column(
    metadata_cell_removed, combine_start_row_identifier,
    combine_end_row_identifier
  )

  header_row_count <- length(columns_to_create)

  cleaned <- remove_columns(
    rows_combined, columns_to_remove_patterns, columns_to_remove_offset,
    first_header_row, header_row_count
  )

  return(cleaned)

}


#' @title change dates to character strings
#'
#' @description Treat date strings as character string in xlsx_cells data.
#' If the data_type is 'date' and the date column is not NA, update
#' the data_type column to character and cut information from the date
#' column into the character column.
#'
#' @param dat dataframe imported using tidyxl::xlsx_cells()
#'
#' @returns dataframe dat with date strings set to be treated as character
#' strings.
#'
#' @examples
#' \dontrun{
#' convert_date_to_char(source_df)
#' }
#' @export
convert_date_to_char <- function(dat) {

  if (all(is.na(dat$date))) {
    return (dat)
  }
  newdat <- dat %>%
    mutate(
      character = case_when(
        data_type == "date" ~ as.character(date),
        .default = as.character(character)
      )
    ) %>%
    mutate(
      date = NA,
      data_type = case_when(
        data_type == "date" ~ "character",
        .default = as.character(data_type)
      )
    )

  return(newdat)
}


#' @title Add information from above or below a row into another row.
#'
#' @description Use a specified pattern to find values in the character column
#' of an xlsx_cells dataframe and extend the found value with information from
#' either the rows above or below.
#'
#' NOTE: If the pattern has matches in multiple columns, the change is only made
#' to the column where it occurs most. If this is not the desired behaviour, the
#' function will need to be updated - suggest making the change on data after it
#' has been unpivotted rather than on xlsx_cells data, as then the column name
#' to  which the changes are desired can be specified (e.g. using column name
#' pattern matching).
#'
#' This was written to address an issue in pub sec in the LA Dropdown tab of the
#' DLUHC (MHCLG) capital payments receipts data (see example~).
#'
#' @param dat dataframe imported using tidyxl::xlsx_cells (one row for
#' each cell).
#' @param pattern character string. Regular expressions used to identify the
#' rows that need added information from the  row above or below. In pub sec
#' this variable is specified by extend_row_pattern.
#' @param placement character string. Either 'forward' or 'reverse'. Used
#' to determine if the added information is placed before or after original row
#' value. In pub sec this variable is specified with extend_row_order.
#' @param direction character string. Either 'above' or 'below'. Used to
#' determine if the added information is from above or below the row that
#' matches pattern. In pub sec this variable is specified with extend_row_with.
#'
#' @return dataframe in xlsx_cells format. If all vars are soecified, returns
#' dat with modified values in the character column. If pattern,
#' placement and direction are NA returns dat with no modification.
#' If the pattern from the data dict doesn't match any character in the
#' dataframe.
#'
#' @examples
#' \dontrun{
#' # see Wiki for example with illustration
#'
#' dat <- data.frame(
#'  id = 1:13,
#'  address = c("A9", "A10", "A11", "A12", "A13", "A14", "A15", "A16",
#'              "A17", "A18","A19","A20","A21"),
#'  character = c("Acquisition of land & existing buildings", "...of which HPA",
#'                "Another character", "...of which expenditure", "More characters",
#'                "...of which grants","...of which HRA", "Even more characters",
#'                "...of which receipts", "Some text", "...of which loans",
#'                "...of which payment","total","payment")
#')
#'
#' result <- extend_row_value(
#'     dat, "^\\s*\\.\\.\\.\\s*of\\s*which", "reverse", "below"
#'     )
#'}
#'@export
extend_row_value <- function(
    dat, pattern=NA, placement=NA, direction=NA
){

  if(all(
    all(is.na(pattern)), is.na(placement), is.na(direction)
  )) {
    return(dat)
  }

  if (any(is.na(pattern), is.na(placement), is.na(direction))) {
    stop(
      "At least one but not all of extend_row_pattern, extend_row_order, and ",
      "extend_row_with have been specified. If one is specified all must be."
    )
  }

  if (direction %in% c('above', 'below') == FALSE) {
    stop(
      "direction ('extend_row_with' in the settings) must be either ",
      "'above' or 'below'. Information in the affected ",
      "character column (which contains matches for the pattern '", pattern,
      "') will not be correctly concatenated."
    )
  }
  if (placement %in% c('forward', 'reverse') == FALSE) {
    stop(
      "placement ('extend_row_order' in the settings) must be either ",
      "'forward' or 'reverse'. Information in the affected ",
      "character column (which contains matches for the pattern '", pattern,
      "') will not be correctly concatenated."
    )
  }

  # Determine whether we subtract from or add to the address value to find the
  # row value of the target
  if (direction == 'above') {
    add_or_subtract_one <- -1
  } else if (direction == 'below') {
    add_or_subtract_one <- 1
  }

  # Filter the data based on the pattern
  filtered_data <- dat %>%
    filter(grepl(pattern, character))

  if (nrow(filtered_data) == 0) {
    stop(
      "When attempting to combine information in one row with information in ",
      "another, no text was found that matched extend_row_pattern ('",
      pattern, "'). Please contact developer if the expected items.",
      "are not in the preprocessed data."
    )
  }

  row_numbers <- filtered_data$row
  row_address <- filtered_data$address

  # get the position of the columns that contain pattern matches
  counts_by_column <- filtered_data %>%
    group_by(col) %>%
    count() %>%
    ungroup()

  most_common_column <- counts_by_column %>%
    filter(n == max(n))

  if (nrow(counts_by_column) == 1) {

    common_column <- pull(counts_by_column, col)

  } else if (length(counts_by_column) > 1) {

    common_column <- most_common_column %>%
      # assume that the column in which the pattern appears most is the one we
      # are interested in
      filter(n == min(n)) %>%
      # if there are two columns with the same number of matches just use the
      # first
      filter(col == min(col)) %>%
      pull(col)

    warning(
      "More than one column was found with character matches for the ",
      "extend_row_pattern in sheet_structure of the data dict ('.",
      pattern, "'). Text matching the extend_row_pattern in column ",
      LETTERS[common_column], " will be combined to information in the row ",
      direction, "."
    )
  }

  addresses <- filtered_data %>%
    filter(col == common_column) %>%
    mutate(direction_list = row + add_or_subtract_one,
           column_letter = str_extract(address, "[A-Z]")
    ) %>%
    mutate(combine_row = paste0(column_letter, direction_list))

  # Loop through each pair of rows to combine
  # for (i in seq_along(combine_dict$combine_row)) {
  for (i in seq_along(addresses$combine_row)) {
    # Get the original cell address and that of the cell we may want to combine
    combine_row <- addresses$combine_row[i]
    original_row_number <- addresses$address[i]

    # Find the indices of the rows in the dat
    combine_index <- which(dat$address == combine_row)
    original_index <- which(dat$address == original_row_number)

    if (length(combine_index) > 0 & length(original_index) > 0) {
      # Get the character values
      combine_character <- dat$character[combine_index]
      original_character <- dat$character[original_index]

      # we need to check that the combine_character does not match the pattern.
      # If it does we need to look at the row above until we find a string that
      # doesn't.
      while ((grepl(pattern, combine_character) == TRUE)) {
        i <- i + add_or_subtract_one
        combine_row <- addresses$combine_row[i]
        combine_index <- which(dat$address == combine_row)
        combine_character <- dat$character[combine_index]
      }

      # Update the character column based on combine_order
      if (placement == "forward") {
        character_to_add <- paste(original_character, combine_character)
      } else if (placement == "reverse") {
        character_to_add <- paste(combine_character, original_character)
      }

      # Assign the new character value to the original row value
      dat$character[original_index] <- character_to_add
    }
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


#' @title Remove first cell from a dataset matching each regular expression
#'
#' @description Remove rows from an xlsx_cells imported dataframe if the
#' character value is the first match for the supplied regular expression.
#'
#' It should only be used when absolutely necessary as it carries the risk of
#' removing a cell that is actually wanted. To mitigate this risk only a given
#' number of non-blank rows are checked for the pattern using n_row.
#'
#' @details
#' This function is not included in clean_xlsx_cells_data because that is only
#' called for the whole sheet, whereas this function is better called at the
#' table level.
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


#' @title Combine consecutive rows within the same column.
#'
#' @description Use regular expression patterns to find the first
#' and last of the rows that need to be combined into a single value, and
#' concatenate those values. Cells will only be combined with others that sit in
#' the same column. Combined character strings are spearated by a space.
#'
#' This was written to address an issue in the pub sec DLUHC (MHCLG) NNDR and
#' council tax data, where headings had been split over multiple Excel rows,
#' rather than using alt enter to get a new line in the cell (see example for a
#' simplified situation.
#'
#' @param dat dataframe imported with tidyxl::xlsx_cells
#' @param start_pattern character string. A regular expression that first
#' appears in the first of the header rows that needs to be combined. In pub sec
#' this variable is specified by combine_start_row_identifier.
#' @param end_pattern character string. A word or pattern that first
#' appears in the last of the header rows that needs to be combined. In pub sec
#' this variable is specified by combine_end_row_identifier.
#'
#' @returns dat dataframe with rows up to the end_row_identifier row
#' concatenated onto the start_row_identifier row for each column. Of the
#' concatenated rows, only the first is kept, the others are removed from the
#' data.
#'
#' @examples
#' \dontrun{
#'
#' |      || this    || another |
#' |      ||---------||---------|
#' |      || is one  || heading |
#' |      ||---------||---------|
#' | year || heading || here    |
#' |------||---------||---------|
#' | 2020 ||   1162  ||  1034   |
#'
#' The data shown above is input by xlsx_cells to look like this:
#'
#' dat <- data.frame(
#'     "row" = c(1, 1, 2, 2, 3, 3, 3, 4, 4, 4),
#'     "col" = c(2, 3, 2, 3, 1, 2, 3, 1, 2, 3),
#'     "character" = c( "this", "another",
#'                      "is one", "heading",
#'                      "year", "heading", "here",
#'                      NA, NA, NA),
#'     "numeric" = c(rep(NA, 7),
#'                   2020, 1162, 1034),
#'     "data_type" = c(rep("character", 7), rep("numeric", 3))
#' )
#'
#' result <- combine_rows_by_column(dat, "this", "here")
#' }
#' @export
combine_rows_by_column <- function(dat, start_pattern=NA, end_pattern=NA){

  if (
    all(is.na(start_pattern), is.na(end_pattern))
  ) {
    return (dat)
  } else if (
    is.na(start_pattern)
  ) {
    stop("combine_start_row_identifier is missing from the settings")
  } else if (
    is.na(end_pattern)
  ) {
    stop("combine_end_row_identifier is missing from the settings")
  }
  message("Combining rows.")

  start_row <- get_first_instance(dat, start_pattern)
  end_row <- get_first_instance(dat, end_pattern)

  if (is.null(start_row)) {
    stop("No matches found for start_row_identifier pattern.")
  }
  if (is.null(end_row)) {
    stop("No matches found for end_row_identifier pattern.")
  }

  if (start_row >= end_row) {
    stop(
      "The first match to combine_start_row_dentifier must be in an earlier ",
      "row than the first match to combine_end_row_identifier.")
  }

  message("Rows ", start_row, " to ", end_row, " will be combined. ",
          "If this is not the desired behaviour, please contact developers.")

  row_range <- seq(start_row, end_row, by=1)
  data_in_rows_to_combine <- filter(dat, row %in% row_range)

  # arrange by row and col as this is the standard xlsx_cells order, but order
  # is important so specify it here in case data have been reordered elsewhere.
  combined_cells <- data_in_rows_to_combine %>%
    arrange(row, col) %>%
    group_by(col) %>%
    mutate(character_to_add = paste0(character, collapse = " ")) %>%
    # we only need to keep one row of the rows being combined
    # so just keep one and label them all as being on the same row.
    filter(row_number()==1) %>%
    mutate(combined = TRUE) %>%
    arrange(row, col)

  to_remove <- data_in_rows_to_combine %>%
    anti_join(combined_cells, by = names(dat))

  # put the new headers back into the main dataset without chanding the order
  combined_in_dat <- dat %>%
    left_join(combined_cells, by = names(dat))

  combined_row_number <- max(combined_cells$row)

  output <- combined_in_dat %>%
    mutate(
      character = case_when(
        combined ~ as.character(character_to_add),
        TRUE ~ as.character(character)),
      row = case_when(
        combined ~ combined_row_number,
        TRUE ~ row)
    ) %>%
    anti_join(to_remove, by = names(dat)) %>%
    # because we changed the row number of some combined cells, we need to get
    # the original order back
    arrange(row, col) %>%
    select(-c(character_to_add, combined))

  return(output)
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
