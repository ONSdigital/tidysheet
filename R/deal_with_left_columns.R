#' @title deal_with_left_columns
#' @description  Un-pivot data with one row of any number of columns
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' Un-pivot data with one row of any number of columns
#' 
#' Take data with a single value column, and any number of character columns that
#' have only one header row. Data must have been imported using xlsx cells. Some
#' unpivotting of other columns may already have occurred (see sample_data that
#' has already been partly unpivotted). 
#' 
#' raw data example:
#' 
#' |    blank    | col_names B | value   |
#' |-------------|-------------|---------|
#' |  England    | Norfolk     |    1    |
#' |  England    | York        |    2    |
#' 
#' Reformat these data from the xlsx_cells format (one row of data per cell - see example)
#' so that it looks like this when colnames are given as c("A", "B"):
#' 
#' |      A      |      B      | numeric |
#' |-------------|-------------|---------|
#' |  England    | Norfolk     |    1    |
#' |  England    | York        |    2    |
#' 
#' 
#' @import tidyxl
#' @import dplyr
#' @import stringr
#' @importFrom unpivotr behead
#' 
#' @param dat A dataframe (imported using xlsx_cells) containing the 
#' table to be processed. 
#' @param col_names string. Vector of new column names for the header row 
#' 
#' @return A data frame containing the unpivotted data.
#' 
#' @examples
#'  
#' # first row of data as shown in the description above   
#' sample_data <- data.frame(
#'   address = c('A2', 'B2', 'C2'),
#'   row = c(2, 2, 2),
#'   col = c(1, 2, 3),
#'   is_blank = FALSE),
#'   data_type = c(rep('character', 2), 'numeric'),
#'   numeric = c(NA, NA, 1),
#'   character = c('England', 'Norfolk', NA))
#' 
#' deal_with_left_columns(sample_data,  c("A", "B"))
#' 
#' # data that is partially unpivotted (service code and subservice already unpivotted)
#' sample_data <- data.frame(
#'   address = c('C4', 'D4', 'A4', 'B4'),
#'   row = 4,
#'   col = c(3, 4, 1, 2),
#'   is_blank = FALSE,
#'   data_type = c(rep('numeric', 2), rep('character', 2)),
#'   numeric = c(10, 20, NA, NA),
#'   character = c(NA, NA, 'Cornwall', 'SW'),
#'   service = c(rep("Education", 2), NA, NA),
#'   code = c(123, 456, NA, NA),
#'   subservice = c("primary", "secondary", "LA", "Region"))
#'   
#' deal_with_left_columns(sample_data,  c("LA", "Region")) 
#' @export
deal_with_left_columns <- function(dat, col_names) {
  
  for (i in 1:length(col_names)) {
    
    dat <- dat %>%
      behead("left", !!col_names[i])
  }
  
  return(dat)
}
