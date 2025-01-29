#' @title get_header_identifier
#' @description Get header_identifier from header_identifiers string vector
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @details 
#' Get header_identifier from header_identifiers string vector
#'
#' Function to identify the correct header_identifier for preprocessing by using the file_part variable
#' to subset the string vector (header_identifiers) and return the correct header_identifier respective to file_part.
#' 
#' @param header_identifiers : A string vector 
#' @param file_part (string): An integer/number cannot be a float or 0
#'
#' @return header_identifier : A string selected from the header_identifier string vector
#' 
#' @examples 
#' header_identifiers <- c("total", "HRA Reserves") 
#' file_part <- 1 
#' result <- get_header_identifier(header_identifiers, file_part)
#' print(result)
#' 
#' @export
get_header_identifier <- function(header_identifiers, file_part) {
  
  file_part_numeric <- as.numeric(file_part)
  
  if (between(file_part_numeric, 1, length(header_identifiers))){
    header_identifier <- header_identifiers[file_part_numeric]
  }
  else {warning("file_part is out of range of vector length, contact developer")}
  
}