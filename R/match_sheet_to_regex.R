#' @title match_sheet_to_regex
#' @description Find values in a vector that match a regular expression 
#' @details 
#' Find values in a vector that match a regular expression 
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @param sheet_names Vector of character strings. 
#' @param pattern Regular expression to be detected in sheet_names.
#' 
#' @return List of character strings. Sheet names that fitted regex pattern 
#' @examples
#' sheet_names <- c("Front Page", "LA drop-down", "RA LA Data 2020-21")
#' pattern <- "RA|Data"
#' match_sheet_to_regex(sheet_names, pattern)
#'
#' @export
match_sheet_to_regex <- function(sheet_names, pattern){
  
  if (!is.character(sheet_names)){
    stop("Sheet names should be a vector of character strings")
  }
  if (!is.character(pattern)){
    stop("pattern should be a character string")
  }
  
  matched_sheets <- list() 
  
  for (sheet in sheet_names){
    if (stringr::str_detect(sheet, pattern) == TRUE){
      matched_sheets <- c(matched_sheets, sheet)
    }
  }
  
  if (length(matched_sheets) > 1){
    warning(paste("Multiple Sheets have matched preprocessing regex, only tab: "), matched_sheets[[1]], ", will be preprocessed. Please alert a developer if this is an incorrect tab.")
  }
  
  if (length(matched_sheets) == 0){
    warning("No tab has beeen identified for preprocessing for this dataset. Please contact a developer if this dataset should be preprocessed.")
  }
  
  return(matched_sheets)
}