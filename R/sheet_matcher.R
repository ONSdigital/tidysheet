#' @title sheet_matcher
#' @description Select a sheet from a list of matching sheets based on a regex pattern.
#' @details 
#' Select a sheet from a list of matching sheets based on a regex pattern.
#'
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' 
#' @param matching_sheets: list of strings.
#'
#' @return string 
#' 
#' @examples 
#' Example 1 case of multiple sheets matching: 
#' matching_sheets <- list("Sheet1", "Sheet2")
#' 
#' Example 2 case of no sheets matching: 
#' matching_sheets <- list()
#' 
#' Example 3 case of 1 matching sheet: 
#' matching_sheets <- list("Scotland")
#' 
#' sheet_matcher(matching_sheets)
#' 
#' @export
sheet_matcher <- function(matching_sheets) {
  if (length(matching_sheets) == 0) {
    tabname <- NULL
  } else if (length(matching_sheets) == 1) {
    tabname <- matching_sheets[[1]]
  } else {
    warning("Multiple sheets match with the given regex pattern. The first match has been used. If the first match is the wrong sheet, please contact a developer so that they can correct.")
    tabname <- matching_sheets[[1]]
  }
  return(tabname)
}