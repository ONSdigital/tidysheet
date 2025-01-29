#' @title get_info_above_table
#' @description Get the first header row index based on specified identifiers
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details Returns data from above the header row
#' @description
#' Returns data from above the header row
#'
#' This function removes empty rows from the source data.
#'
#' @param main_table The source data table.
#' @param header_row The row number of the first header row.
#' @return info_above_table the data above the table
#' @examples
#' main_table <- remove_empty_rows(main_table, first_header_row)
#' @author Your Name
#' @export
get_info_above_table <- function(main_table, header_row) {
  info_above_table <- main_table %>% 
    filter(row < header_row) %>% 
    filter(!is.na(character))
  
  if (nrow(info_above_table) == 0) {
    warning("No information found above the first header row. If there is info above the headers in the raw data, header_identifier in the data dictionary may need updating. Please contact a developer.")
  }
  
  return(info_above_table)
}