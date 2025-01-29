#' @title convert_year_format
#' @description  replace non alphanumeric characters in the year strings with hyphens
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' replace non alphanumeric characters in the year strings with hyphens
#'
#' @param year_string  character string e.g. 2021/22
#'
#' @return clean_year as the output replaced with hyphen in the year
#' @examples
#' year <- c("2021/22", "2022/23")
#' convert_year_format(year)
#'
convert_year_format <- function(year_string){
  clean_year <- gsub("[^0-9A-Za-z]", "-", year_string)
  return(clean_year)
}