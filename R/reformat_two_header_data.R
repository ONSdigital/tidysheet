#' @title reformat_two_header_data
#' @description  Reformat data with one header row on the left and two on the right
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' Reformat data with one header row on the left and two on the right
#' 
#' Used for cases where Excel data have only one header row in the left block, 
#' but two on the right. the left headers are on the same row as the second
#' row of headers on the right. 
#' 
#' The two blocks of headers are treated differently:
#' 
#' The column names of block of data on the left (in the original table) are 
#' carried over to the output. 
#' If a column in the left block doesn't have a name it is called 'column_<x>'.
#' 
#' The block of data on the right is changed from wide format to long, so the 
#' original column names become entries in 2 new columns (group_col, and 
#' nested_column_1)
#' 
#' This is used for e.g. dluhc revenue expenditure final data for RO2 in 2021-22. 
#' 
#' @import tidyxl
#' @import dplyr
#' @importFrom unpivotr behead
#' 
#' @param dat A dataframe containing the main data table to be processed.
#' @param group_col string. New column name for the first header row.
#' @param nested_column_1 string. New column name for the second header row.
#' 
#' @return A data frame containing the processed and rearranged data after unpivoting.
#' 
#' @examples
#' sample_data <- data.frame(
#'    address = c('A1', 'B1', 'C1', 'D1', 'A2', 'B2', 'C2', 'D2', 'A3', 'B3', 'C3', 'D3'),
#'    row = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
#'    col = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4),
#'    is_blank = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
#'    data_type = c('blank', 'blank', 'character', 'blank', 'character',
#'    'character', 'character', 'character', 'character', 'character', 'numeric', 'numeric'),
#'    numeric = c(NA, NA, NA, NA,NA, NA, NA, NA, NA, NA, 10, 20),
#'    date = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
#'    character = c(NA, NA, 'Education', NA, 'LA', 'Region', 'primary', 'secondary', 'Cornwall', 'SW', NA, NA)
#'  )
#' reformat_two_header_data(sample_data, 'service', 'subservice')
#' @export
reformat_two_header_data <- function(dat, group_col, nested_column_1, left_headers) {
  
  # assume the first column of the right block is the first of consecutive 
  # numeric columns (assuming no blank cols between them) - 
  # where a numeric column is defined as one where more than 50% of it's cells 
  # hold numeric data
  numeric_column_locs <- get_vector_locs_of_type(dat = dat, 
                                                 datatype = "numeric",
                                                 tolerance = 0.4)
  
  if(length(numeric_column_locs) == 0) {
    stop("no consecutive numeric columns have been found with the given tolerance, so could not identify split between left and right header blocks. reformat_two_header_data will need to be adapted by a developer.")
  }
  
  first_right_header_col <- get_first_of_consecutives(numeric_column_locs)
  
  right_beheaded <- dat %>%
    behead("up-left", !!sym(group_col)) %>%
    behead("up", !!sym(nested_column_1))
  
  cols_to_name <- get_left_headers(dat, first_right_header_col, left_headers)
  
  beheaded <- deal_with_left_columns(right_beheaded, cols_to_name) 
  
  return(beheaded)
  
}
