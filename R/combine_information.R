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
#' @param direction (str). Either 'above' or 'below'. Used to determine if
#' the added information is from above or below the row that matches
#' pattern. In pub sec this variable is specified with extend_row_with.
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
