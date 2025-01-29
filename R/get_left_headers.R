#' @title get_left_headers
#' @description  Get the column names for the left block
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details
#' Get the column names for the left block
#'
#' Sometimes columns in the left block are all named in the raw data,
#' sometimes none of them are named, and sometimes only some are, with others
#' left blank.
#'
#' We have the option to provide names for columns in the left header block in
#' the data dictionary (left_headers), however this is risky as suppliers could
#' change the number or order of those columns.
#'
#' A third option is to use placeholder names (e.g. 'column_1', 'column_2' etc)
#'
#' Rules:
#' - If there is the correct number of names in the data (existing names) use
#'    these, even if names have been given in the data dictionary (left_headers).
#' - If the number of names given in the dict is too many do not use any of them.
#' - If there are names missing from the existing names, fill them with
#'    placeholder names, but use the ones that are given.
#' - Use all of left_headers if no names already exist in the data AND the
#'    correct number of names has been supplied.
#'
#' @param dat dataframe. Must be xlsx_cells format.
#' @param first_right_header_col int. The number of the first column of data.
#' @param left_headers character vector. Can be NA (see rukes above)
#' @return cols_to_name TBD
#' @export
get_left_headers <- function(dat, first_right_header_col, left_headers, tolerance_list = c(tolerance_numeric = 0.4, tolerance_char = 0.7, tolerance_blank = 0.9)) {

  # to avoid errors in assigning names to columns, make sure cols are ordered
  ordered_cols <- arrange(dat, col)

  first_data_row <- get_first_data_row(ordered_cols, tolerance_list = tolerance_list)

  # first get the column ID's (locations) of populated columns in the left block
  col_locs_to_name <- ordered_cols %>%
    filter((col < first_right_header_col)
           & data_type != "blank") %>%
    distinct(col)

  existing_names <- ordered_cols %>%
    filter((col < first_right_header_col)
           & (row < first_data_row)
           & data_type == "character") %>%
    distinct(character, col, row) %>%
    right_join(col_locs_to_name, by = "col") %>%
    arrange(col)

  # In nhs digital SALT 2021-22 there is a row with empty strings in the left
  # block headers, and another row with filled strings. Treat the empty ones
  # like NAs
  non_blank_names <- existing_names %>%
    mutate(character = ifelse(str_squish(character) == "",
                              NA,
                              character))

  # In the NI council tax and NNDR data 2023-24 the table title (the year)
  # is in the same row as the headers, so it has not been removed, and now
  # looks like it could be a left block header because it is above the data and
  # to the left of the first data column.
  # DLUHC borrowing and investments has row headers
  # Assume that there is only ever one row of left block headers: Choose
  # which is the right row by:
  # 1. Are there any rows with the expected number of entries?
  # - if 'yes' limit the options to these
  # 2. Out of the now limited rows, use the row that is closest to the first row
  # of data.
  if (any(!is.na(non_blank_names$character))) {
    nas_removed <- non_blank_names[!is.na(non_blank_names$character),]
    header_row <- max(nas_removed$row, na.rm = TRUE)

    cols_to_name_count <- nrow(col_locs_to_name)
    rows_with_all_cols_filled <- nas_removed %>%
      group_by(row) %>%
      count() %>%
      filter(n == cols_to_name_count) %>%
      pull(row)

    if (length(rows_with_all_cols_filled) > 0) {
      existing_names_chosen_row <- non_blank_names %>%
        filter(row == max(rows_with_all_cols_filled))
    } else {
      existing_names_chosen_row <- non_blank_names %>%
        filter(row == header_row | is.na(row))
    }

    names_excluding_blanks <- existing_names_chosen_row %>%
      filter(is.na(character) == FALSE)

    all_names_exist <- nrow(names_excluding_blanks) == nrow(col_locs_to_name)
    some_names_exist <- any(!is.na(existing_names_chosen_row$character))

  } else {
    all_names_exist <- FALSE
    some_names_exist <- FALSE
  }

  # check the left_headers passed from the data dict
  left_headers <- left_headers[!is.na(left_headers)]
  left_headers_right_length <- length(left_headers) == nrow(col_locs_to_name)
  too_many_left_headers <- length(left_headers) > nrow(col_locs_to_name)
  too_few_left_headers <- length(left_headers) < nrow(col_locs_to_name)

  if (all_names_exist) {
    cols_to_name <- existing_names_chosen_row$character

  } else if (some_names_exist) {
    cols_to_name <- existing_names_chosen_row %>%
      mutate(character = ifelse(
        is.na(character),
        paste0("column_", as.character(col)),
        character
      )) %>%
      pull(character)

    # if no names exist in the data:
  } else if (left_headers_right_length) {
    cols_to_name <- left_headers

  } else {

    if (too_many_left_headers) {
      warning(paste0("More left_headers were supplied in the data dict than have been found in the data. Is there a column that is usually in the data now missing? If not, please check this with a developer"))
    } else if (all(too_few_left_headers & length(left_headers) > 0)) {
      warning(paste0("Not enough left_headers were supplied in the data dict for the number of left block columns found in the data. Is there an extra column in the data that is usually there? If not, please check this with a developer"))
    }

    cols_to_name <- col_locs_to_name %>%
      mutate(character = paste0("column_", as.character(col))) %>%
      pull(character)
  }

  return(cols_to_name)

}
