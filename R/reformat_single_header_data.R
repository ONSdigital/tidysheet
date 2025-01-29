#' @title reformat_single_header_data
#' @description  Reformat data with one header row
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details
#' Reformat data with one header row
#'
#' Used for cases where data have only one header row, but two blocks that are
#' treated differently:
#'
#' The column names of the block of data on the left (in the original table) are
#' carried over to the output.
#'
#' The block of data on the right is changed from wide format to long, so the
#' original column names become entries in two new columns (group_row and
#' nested_column_1)
#'
#' This is used for e.g. dluhc revenue expenditure final data in 2021-22.
#'
#'
#' @param dat tidyxl::xlsx_cells table.
#' @param columns vector of strings. group_col then nested_column_1 (optional)
#'                then nested_column_2. Order is important
#' @param  vector of strings or NA. optional. Names of headers in the left block. If used it must be the correct length for the data
#' @param first_header_row integer. Row number of the first header row
#' @param split_points where to split
#'
#' @return tibble. Tidy dataframe.
#'
#' @examples     harmonised <- reformat_single_header_data(for_reformatting,
#'                     columns, left_headers, first_header_row, split_points)
#'
#' @export
reformat_single_header_data <- function (dat, columns, left_headers, first_header_row, split_points,
                                         tolerance_list = c(tolerance_numeric = 0.4, tolerance_char = 0.7, tolerance_blank = 0.9)) {
  group_col <- columns[1]

  if (length(columns) > 1) {
    nested_column_1 <- columns[2]
  } else {
    nested_column_1 <- NA
  }

  if (length(columns) > 2) {
    nested_column_2 <- columns[3]
  } else {
    nested_column_2 <- NA
  }

  # assume the first column of the right block is the first of consecutive
  # numeric columns (assuming no blank cols between them) -
  # where a numeric column is defined as one where more than 50% of it's cells
  # hold numeric data
  numeric_column_locs <- get_vector_locs_of_type(dat = dat,
                                                 datatype = "numeric",
                                                 tolerance = tolerance_list['tolerance_numeric'],
                                                 direction = "col",
                                                 include_blanks=FALSE)

  if(length(numeric_column_locs) == 0) {
    stop("no consecutive numeric columns have been found with the given tolerance, so could not identify split between left and right header blocks. reformat_single_header_data will need to be adapted by a developer.")
  }

  first_right_header_col <- get_first_of_consecutives(numeric_column_locs)

  # Ideally the right block only contains numeric value columns. However, in
  # DLUHC CPR3 year and quarter series tab in 2023-24 the notes column comes
  # after the numeric columns. Treat this as part of the left header block not
  # the right
  right_block_colnames <- dat %>%
    filter(col >= first_right_header_col &
             row == first_header_row) %>%
    pull(character)

  if ("notes" %in% tolower(right_block_colnames)) {
    notes_col <- dat %>%
      filter(tolower(character)=="notes") %>%
      distinct(col) %>%
      pull(col)
  } else {
    notes_col <- "NA"
  }

  # In DLUHC CPR files, the year and quarter series tab contains two tables that
  # have the same headers. Remove the second line of headers
  duplicated_header_rows <- get_duplicated_rows(dat, first_header_row)

  if (length(duplicated_header_rows) == 0) {
    duplicated_header_rows <- "NA"
  }

  no_duplicated_headers <- dat %>%
    filter(row != duplicated_header_rows)

  # right block of headers (service, sub-service, transaction etc)---
  right_beheaded <- no_duplicated_headers %>%
    filter(col >= first_right_header_col &
             (col != notes_col | notes_col == "NA")) %>%
    behead("up", "original_header")

  if (any(!is.na(split_points))) {  # used only where there are multiple headings in a single row.
    right_block_corrected <- split_original_header(
      right_beheaded, columns, split_points, "original_header")

  } else {
    right_block_corrected <- right_beheaded %>%
      mutate(!!sym(group_col) := original_header)
  }

  # left block of headers (Local Authority, ONS Code etc) ---
  cols_to_name <- get_left_headers(dat,
                                   first_right_header_col,
                                   left_headers,
                                   tolerance_list = tolerance_list)

  # There are still the columns on the left which we are not unpivotting,
  # so we need to use their original names. The order of these columns changes
  # between years. Names are therefore taken from the `cols_to_name` vector rather
  # than being hard-coded.
  # Because the number of columns in the left hand block of headers changes from
  # year to year, they are named using a loop.
  # We go left because e.g. if we take a number in the table and go left to the
  # edge of the table we hit e.g. "E0101", and this value is then given the
  # first name in cols_to_name (E-code)
  left_beheaded <- no_duplicated_headers
  for (i in 1:length(cols_to_name)) {
    left_beheaded <- left_beheaded %>%
      unpivotr::behead("left", !!cols_to_name[i])
  }

  # we need to remove the header row from left_beheaded because we have done
  # the left and right blocks separately, and it has therefore not been properly
  # 'beheaded'. This means that the original column headings are in the new data
  # twice - once as new column headings/ in the new 'original heading' column, and
  # again in the character column - we want to get rid of the entries in the
  # character column. By doing this we get the same number of rows as in
  # right_beheaded (right_block_corrected), so can then join the two.
  beheaded <- left_beheaded %>%
    filter(row > first_header_row) %>%
    left_join(right_block_corrected,
              by = names(no_duplicated_headers)) %>%
    # in DLUHC capital expenditure 2022-23 there is a column at the end with codes
    # remove these non-data because they mess up the validation and are not needed
    filter(!(is.na(original_header) & is.na(numeric)))


  # if there is a notes column as part of the right header block it needs
  # beheading separately and adding to the output
  if(notes_col != "NA") {

    notes_beheaded <- no_duplicated_headers %>%
      filter(col == notes_col) %>%
      mutate(notes = character) %>%
      select(sheet, row, notes)

    output <- beheaded %>%
      left_join(notes_beheaded,
                by = c("sheet", "row"))

  } else {
    output <- beheaded
  }

  return(output)
}
