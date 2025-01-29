#' @title convert_year_end_to_fy
#' @description  Convert financial year end column (yyyy) to yyyy-yy
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' Convert financial year end column (yyyy) to yyyy-yy
#' 
#' Add a new column to a datframe. Use the financial year end (given as yyyy) to
#' generate a column that gives financial year in the format yyyy-yy.
#' 
#' @param dat dataframe containing end_year
#' @param end_year string. Name of a column in dat containing years as yyyy 
#' @returns tibble. dataframe with a new column  called year, with strings in 
#' the format yyyy-yy
#'
#' @examples  
#' dat <- data.frame(fin_year_end = c("2020", "2021")) 
#' convert_year_end_to_fy(dat, "fin_year_end")
#' 
#' @export
convert_year_end_to_fy <- function(dat, end_year) {
  
  dat_new_year <- dat %>% 
    mutate(year_end = 
             substr(
               as.character(!!sym(end_year)), 
               3, 4),
           century = 
             substr(
               as.character(!!sym(end_year)), 
               1, 2)) %>% 
    mutate(year_end_numeric = as.numeric(year_end)) %>% 
    mutate(year_start_numeric = year_end_numeric - 1) %>% 
    mutate(year = paste0(century, year_start_numeric, "-", year_end_numeric)) %>% 
    select(-c(year_end, century, year_start_numeric, year_end_numeric))
  
  return(dat_new_year)
}