#' @title make_year_patterns
#' @description Build regex patterns for years between 1900 and 2099
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' Build regex patterns for years between 1900 and 2099
#'
#' Creates regex patterns that look like a year or a financial year from the
#' 20th or 21st century. 
#' 
#' @return Dataframe containing one row per column. Columns are different year types
#'
#' @examples
#' patterns <- make_year_patterns()
#'
#' @export
make_year_patterns <- function () {
  
  not_preceded_by_number_or_letter <- "(?<![0-9A-Za-z])"
  two_numbers <- "\\d{2}"
  century <- "(19|20)"
  not_followed_by_number_or_letter <- "(?![0-9A-Za-z])"
  fy_separators <- "(-|_|\\/|(\\b\\sto\\s\\b)|(to))"
  
  annual_pattern <- paste(c(not_preceded_by_number_or_letter, century, 
                            two_numbers, 
                            not_followed_by_number_or_letter), collapse = "")
  
  financial_pattern <- paste(c(not_preceded_by_number_or_letter, century, two_numbers, fy_separators,
                               two_numbers, not_followed_by_number_or_letter), collapse = "")
  
  year_patterns <- data.frame("annual" = annual_pattern, 
                              "financial" = financial_pattern)
  
  return(year_patterns)
}
