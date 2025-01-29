#' @title check_all_nchar
#' @description  Get the column names for the left block
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' Check that all entries in a column have a certain number of characters
#' 
#' @param dat dataframe containing col_name
#' @param col_name string. Name of a column in dat. DColumn does not have to be character 
#' @param num_char int. Number of characters you want to check for
#' @returns bool. TRUE if all entries (not including NAs) have the number of 
#' characters specified by num_char
#'
#' @examples
#' # example code
#'    
#' all4 <- data.frame(year = c("2020", "2021")) 
#' mixed <- data.frame(year = c("2020-21", "2021")) 
#' 
#' check_all_nchar(all4, "year", 4) # TRUE
#' check_all_nchar(mixed, "year", 4) # FALSE
#' @export
check_all_nchar <- function(dat, col_name, num_char) {
  
  distinct_num_chars <- dat %>% 
    mutate(
      char_count = nchar(as.character(!!sym(col_name)))
    ) %>% 
    mutate(num_chars_check = ifelse(char_count == num_char, TRUE, FALSE)) %>% 
    filter(!is.na(num_chars_check)) %>% 
    distinct(num_chars_check)
  
  all_pass <- all(distinct_num_chars == TRUE)
  
  return(all_pass)
}