#' @title concatenate_columns
#' @description  Concatenate two columns
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' Concatenate two columns
#' 
#' Concatenate the strings from two columns. The new column overwrites col_1.
#' col_2 is deleted. Information from the two columns is separated by a bar (|)
#'  
#' 
#' @param dat A dataframe containing columns used for col_1 and col_2.
#' @param col_1 string. The column whose information will come first.
#' @param col_2 string. The column whose information will come second. 
#'              This column will then be deleted.
#'              
#' @return tibble. The same as dat but containing the new column, and missing col_2.
#' 
#' @examples
#' sample_data <- data.frame(
#'   notes = c('Note 1', NA, 'Note 2', NA),
#'   info = c(NA, "Info A", "Info B", NA)
#' )
#' 
#' 
#' concatenate_columns(sample_data, "notes", "info")
#' 
#' simple_table_behead(sample_data, 'subservice', 'transaction')
#' 
#' @export
concatenate_columns <- function(dat, col_1, col_2) {
  
  if (col_1 %in% names(dat) == FALSE &
      col_2 %in% names(dat) == FALSE) {
    
    stop(paste0("col_1 ('", col_1, "') and col_2 ('", col_2, "') not found in data"))
    
  } else if (col_1 %in% names(dat) == FALSE) {
    
    stop(paste0("col_1 ('", col_1, "') not found in data"))
    
  } else if (col_2 %in% names(dat) == FALSE) {
    
    stop(paste0("col_2 ('", col_2, "') not found in data"))
    
  }
  
  newdat <- dat %>% 
    mutate(!!sym(col_1) := case_when(
      !is.na(!!sym(col_1)) & !is.na(!!sym(col_2)) ~ paste0(!!sym(col_1), " | ", !!sym(col_2)),
      is.na(!!sym(col_1)) & !is.na(!!sym(col_2)) ~ as.character(!!sym(col_2)),
      !is.na(!!sym(col_1)) & is.na(!!sym(col_2)) ~ as.character(!!sym(col_1)))
    ) %>% 
    select(-!!sym(col_2))
  
  return(newdat)
  
}
