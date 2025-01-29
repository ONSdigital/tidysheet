#' @title consecutive_years_to_financial
#' @description  TBD
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' TBD
#' @param year the year wanted to convert
#' @returns year the newly converted year
#' @export
consecutive_years_to_financial <- function (year) {
  
  if (length(year) < 2) {
    stop("Only one year found.")
  }
  
  if (length(year) > 2) {
    stop("More than two years found. Consecutive years not checked for so even if they are annual, they were not converted to financial.")
  }
  
  if (all(nchar(year)==4)) {
    year <- as.numeric(year)
    
    if (year[2] - year[1] == 1) {
      year1 <- as.character(year[1])
      year2 <- as.character(year[2])
      
      year <- paste0(year1, "-", stringr::str_sub(year2, -2))
      warning(paste0("Consecutive years: ", year1, " and ", year2, " found. They have been compressed to ", year, "."))
      
    } else {
      
      year <- as.character(year)
      warning("Multiple non-consecutive years found.")
    }
  } else {
    warning("Years found are not annual (YYYY), but there is more than one. Check that year column contain expected values")
  }
  
  return(year)
}