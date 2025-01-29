#' @title simple_table_behead
#' @description Reformat simple layout data
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' Reformat simple layout data
#' 
#' Used for cases where Excel data have only one header row where each column contains values,
#' and one column of strings in the first non-empty column identifying what the data are for.
#' Any number of numeric columns may be given, but there must only be one character column:
#' 
#' raw data example:
#' 
#' | string_column_name | unpivotted_column_name A | unpivotted_column_name B |
#' |--------------------|--------------------------|--------------------------|
#' |     primary        |            1             |            2             |
#' |    secondary       |            3             |            4             |
#' 
#' Reformat these data so that there is only one number per row:
#' 
#' | string_column_name | unpivotted_column_name | numeric |
#' |--------------------|------------------------|---------|
#' |     primary        |            A           |   1     |
#' |     primary        |            B           |   2     |
#' |    secondary       |            A           |   3     |
#' |    secondary       |            B           |   4     |
#' 
#' 
#' 
#' @param main_table A dataframe (imported using xlsx_cells) containing the 
#' table to be processed. 
#' @param string_column_name string. New column name for the entries in the first 
#' non-empty column.
#' @param unpivoted_column_name string. New column name for the header row 
#' (all columns except the first non-empty column.
#' 
#' @return A data frame containing the unpivotted data.
#' 
#' @examples
#' sample_data <- data.frame(
#'   address = c('B2', 'C2', 'A3', 'B3', 'C3', 'A4', 'B4', 'C4', 'A5', 'B5', 'C5'),
#'   row = c(2, 2, rep(3:5, each = 3)),
#'   col = c(2, 3, rep(1:3, 3)),
#'   is_blank = rep(FALSE, 11),
#'   data_type = c(rep('character', 3), 
#'               rep(c(rep('numeric', 2), 'character'), 2), rep('numeric', 2)),
#'   numeric = c(rep(NA, 3), 1, 2, NA, 3, 4, NA, 5, 6),
#'   character = c('measure 1', 'measure 2', 'primary', rep(NA, 2),
#'                 'secondary', rep(NA, 2), 'Total education', rep(NA, 2))
#' )
#' 
#' # look at the data as it would be in Excel
#' rectify(sample_data)
#' 
#' simple_table_behead(sample_data, 'subservice', 'transaction')
#' 
#' @export
simple_table_behead <- function(main_table, string_column_name, unpivoted_column_name) {
  
  main_table %>% 
    # move the 'row names' from the raw data into a named column 
    behead("left", !!sym(string_column_name)) %>%
    # un-pivot all the measures so there is only one column of values
    behead("up", !!sym(unpivoted_column_name))  
  
}