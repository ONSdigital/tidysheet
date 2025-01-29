#' @title get_first_data_row
#' @description  Get the number of the row that contains the first line of data
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details
#' Get the number of the row that contains the first line of data
#'
#' This assumes that there are at least two rows of data where at least 40%
#' of their cells contain numeric values, and that these rows are not separated
#' by rows containing 70% or more of character strings.
#'
#' The first row of data is identified using the following rules:
#' - At least 40% of the cells in this row contain numbers
#' - The following rows either:
#'    - contain at least 40% number cells
#'    - contain at least 90% blank cells AND there are no rows containing
#'      70% or more cells containing character strings before the next
#'      number row or blank row
#' - The row is the first to fulfill the above rules
#'
#' note: At least 40% of the columns in the raw data must contain numeric data
#' i.e. this function won't produce the desired output if there are 3 columns of
#' info on the left and only 2 columns of data.
#'
#' @param dat. A dataframe imported using xlsx_cells. Each row refers to one cell
#'
#' @return Integer. The number of the row that we estimate contains the first
#' row of numeric data.
#'
#' @export
get_first_data_row <- function(dat, tolerance_list = c(tolerance_numeric = 0.4, tolerance_char = 0.7, tolerance_blank = 0.9)) {
  # the tolerance is low, because heading rows are unlikely to have any numeric
  # cells, and there is little chance that two consecutive non-blank rows have
  # this many numbers and are not data
  numeric_rows <- get_vector_locs_of_type(dat, "numeric", tolerance = tolerance_list['tolerance_numeric'], "row") # changed from 0.4
  character_rows <- get_vector_locs_of_type(dat, "character", tolerance = tolerance_list['tolerance_char'], "row")

  # See documentation for amend_numeric_rows to understand why we care about
  # blank rows
  blank_rows <- get_vector_locs_of_type(dat, "blank", tolerance = tolerance_list['tolerance_blank'], "row")

  # some blank rows may already have been deleted
  available_rows <- unique(dat$row)
  missing_blanks <- setdiff(min(available_rows):max(available_rows), available_rows)

  #' Blank rows complicate the identification of where the first row of data
  #' is. If we didn't allow blank rows to be seen as data and the data structure
  #' was:
  #'   1 = header
  #'   2 = header
  #'   3 = data
  #'   4 = blank
  #'   5 = data
  #'   6 = data
  #' The first data row would be identified as 5, when it should be 3.
  #'
  #' However we cant just call all blank rows 'numeric', because e.g. if:
  #'  row:
  #'   1 = header row
  #'   2 = blank
  #'   3 = header that looks like a number (e.g. a code)
  #'   4 = header row
  #'   5 = data
  #'   6 = data
  #' We would want to return row 5, but if all blanks were seen as numbers we
  #' would get row 2.
  #'
  #' Blank rows are therefore only classed as numeric when they are
  #' book-ended by rows identified as numeric. The only times this will fail to
  #' correctly identify the first row of data are
  #' 1. If there are two rows in the header block that are identified as numeric
  #'    without any character rows between them, and
  #' 2. If the last row of headers looks numeric (e.g. codes)
  all_blank_rows <- sort(c(blank_rows, missing_blanks))

  if (length(numeric_rows) == 0) {
    stop("no numeric rows have been found (numeric rows are those where 40% or more columns contain numeric data). Please contact a developer.")
  }

  if (length(all_blank_rows) > 0 ) {

    rows_to_change <- get_flanked_sequences(numeric_rows, all_blank_rows)
    data_rows <- sort(c(numeric_rows, rows_to_change))

  } else {
    data_rows <- numeric_rows
  }

  first_data_row <- get_first_of_consecutives(data_rows)

  return (first_data_row)
}
