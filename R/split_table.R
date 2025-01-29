#' @title split_table
#' @description Split a table into component tables
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' Split a table into component tables
#' 
#' Used for cases where there are several tables within one tab of a 
#' spreadsheet. Splits by row based on a regex pattern (table_identifier).
#' 
#' Reformat these data so that there is only one number per row.
#' 
#' 
#' @param dat A tidyxl dataframe containing the main data table to be processed.
#' @param table_identifier string. Regex pattern
#' 
#' @return data frame (tibble) containing the split data tables. These tables 
#' are held in the output dataframe as separate tables within a column called
#' 'cells'
#' 
#' @export
split_table <- function(dat, table_identifier) {
  
  corners <- dat %>% 
    filter(!is.na(character),
           grepl((table_identifier), character))
  
  unpivotr::partition(dat, corners)
}