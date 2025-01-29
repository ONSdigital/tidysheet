#' @title reformat_row_headers_data
#' @description Reformat row_headers data
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' Reformat row_headers data
#' 
#' Used for cases where Excel data have only one header row, with one column
#' of strings in the first non-empty column. Some of the entries in that 
#' column are headers for the rows below them and do not have an associated
#' value in any of the other columns ('row headers'). 
#' 
#' Reformat these data so that there is only one number per row, and 
#' 'row headers' are associated with all the relevant entries.
#' 
#' 
#' @param main_table A dataframe containing the main data table to be processed.
#' @param group_row string. New column name for the 'row headers'.
#' @param nested_row_1 string. New column name for the entries in the first 
#' non-empty column that are not 'row headers'.
#' @param group_col string. New column name for the header row 
#' (all columns except the first non-empty column.
#' 
#' @return A data frame containing the unpivotted data.
#' 
#' @examples
#' sample_data <- data.frame(
#'   address = c('B2', 'C2', 'A3', 'B3', 'C3', 
#'               'A4', 'B4', 'C4', 'A5', 'B5', 'C5', 
#'               'A6', 'B6', 'C6', 'A7', 'B7', 'C7'),
#'   row = c(2, 2, rep(3:7, each = 3)),
#'   col = c(2, 3, rep(1:3, 5)),
#'   is_blank = c(rep(FALSE, 3), rep(TRUE, 2), rep(FALSE, 7), rep(TRUE, 2), rep(FALSE, 3)),
#'   data_type = c(rep('character', 3), rep('blank', 2), 'character',
#'               rep(c(rep('numeric', 2), 'character'), 2), rep('blank', 2),
#'               'character', rep('numeric', 2)),
#'   numeric = c(rep(NA, 6), 1, 2, NA, 3, 4, rep(NA, 4), 5, 6),
#'   character = c('measure 1', 'measure 2', 'education', rep(NA, 2), 'primary', rep(NA, 2),
#'                 'secondary', rep(NA, 2), 'Transport', rep(NA, 2), 'roads', rep(NA, 2))
#' )
#' 
#' # look at the data as it would be in Excel
#' rectify(sample_data)
#' 
#' reformat_row_headers_data(main_table = sample_data, 
#'                           group_row = 'service', nested_row_1 = 'subservice', 
#'                           group_col = 'transaction')
#' 
#' @export
reformat_row_headers_data <- function(main_table, group_row, nested_row_1, group_col) {
  
  beheaded <- simple_table_behead(main_table, nested_row_1, group_col)
  
  row_headers_added_as_column <- add_row_headers_as_column(dat = beheaded,
                                                           row_header = group_row,
                                                           from = nested_row_1,
                                                           fill_dir = 'down')
  return(row_headers_added_as_column)
}