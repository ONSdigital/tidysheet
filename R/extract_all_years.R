#' @title extract_all_years
#' @description Identify years from strings
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' Identify years from strings
#'
#' Identifies whether a string contains a number that looks like a year from the
#' 20th or 21st century. If no year is identified NA is returned.
#' If one or more years are identified, a string, or vector of unique strings 
#' is returned.
#'
#' @importFrom stringr str_detect str_extract_all
#'
#' @param dat  Numeric or string. Also takes vectors, matrices, or dataframes 
#' @param year_type Character vector. "yyyy" for calendar year, "yyyy-yy" for e.g. 2021-22
#'
#' @return A string, vector of strings, or NA. 
#' Returns a vector of all patterns that matched the year_type pattern.
#' If no years are identified in any of the input strings NA is returned.
#'
#' @examples
#' test_dat <- c("a date: 2005", "1995 6", "1993 end", "a1995", "1995a", "a
#' date:1995, another date: 2012", "not a date: 475", "also not a date: 1234",
#' "abc 12345",  "34200943", "190932")#' get_all_country_names(test_dat)
#' extract_all_years(test_dat)
#'
#' @export
extract_all_years <- function(dat) {
  
  dat <- unlist(dat)
  
  patterns <- make_year_patterns()
  
  annual_years <- extract_matches(dat, patterns$annual)
  financial_years <- extract_matches(dat, patterns$financial)
  
  years <- financial_years
  
  if (all(is.na(years))){
    
    # if financial years are found but year type is "yyyy" give a warning as
    # well as returning the annual year found
    years <- annual_years
  }  
  
  if (is.list(years)) {
    
    # if years are identified at all, we don't want any NAs returned even 
    # some entries in a vector 
    if (any(is.na(years))) {
      real_years <- years[!is.na(years)]
    } else { real_years <- years }
    
    year_vector <- unlist(real_years)
    unique_years <- unique(year_vector)
    
  } else { unique_years <- NA }
  
  return(unique_years)
}
