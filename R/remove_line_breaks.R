#' @title remove_line_breaks
#' @description remove_line_breaks - A function to Remove line breaks from character strings in data
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' remove_line_breaks - A function to Remove line breaks from character strings in data
#' 
#' Some raw (downloaded) datafiles have got newlines included in character strings
#' 
#' 
#' @param dataset data.frame : A dataset with line breaks in values
#' 
#' @return cleaned_dataset : The dataset with removed line breaks from values
#'
#' @examples
#' Example data where there is line breaks in data values
#' dataset <- tibble::tibble(
#'    "Service"  <- c("Education\nServices","Education Services", "Education\n\n Services","\nEducation\n Services\n"),
#'    "Transaction" <- c("Expenditure\n", "\nExpenditure", "\nExpenditure\n", "Expenditure"),
#'    "Units" <- c("thousands","thousands","hundreds", "millions"),)
#'    
#' remove_line_breaks(dataset)
#' 
#' @export
remove_line_breaks <- function(dataset){
  
  cleaned_dataset <- dataset %>%
    
    mutate(across(where(is.character), ~ gsub("[\n\r]+", " ", .))) %>% 
    mutate(across(where(is.character), ~ stringr::str_squish(.)))
  
  return(cleaned_dataset) 
}