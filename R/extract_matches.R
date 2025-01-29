#' @title extract_matches
#' @description Extract patterns from a vector of strings
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' Extract patterns from a vector of strings
#'
#' Looks for a pattern and returns the parts of the strings that matched as a 
#' list. Numeric input will be converted to strings. Each item of the input data 
#' will be returned in it's own vector, with each match given as an item
#' in that vector. The output is therefore a list of vectors. For example if
#' dat is c( "a2", "2 and 3"), and the search pattern is for 
#' numbers the list returned will be [[1]][1] "2" [[2]][1] "2" [[2]][2] "3"
#'
#' @importFrom stringr str_detect str_extract_all
#'
#' @param dat  Numeric or string
#' @param pattern String. Must be a Regex pattern.
#'
#' @return List of character strings. See description for detail.
#'
#' @examples
#' numbers <- extract_matches(c( "a2", "2 and 3"), '[0-9]')
#' 
#' @export
extract_matches <- function(dat, pattern) {
  
  ifelse(stringr::str_detect(dat, pattern), 
         stringr::str_extract_all(dat, pattern), NA)
  
}