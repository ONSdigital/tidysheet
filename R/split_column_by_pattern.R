#' @title split_column_by_pattern
#' @description split_column_by_pattern- Split a column string into two columns
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' split_column_by_pattern- Split a column string into two columns
#' The original_col will contain the original string up to the start of the pattern. 
#' new_col will contain the string following the pattern. 
#' If the name of the new column exists, 
#' then it returns the original data to avoid overwritting column names
#' The pattern itself will be removed from the output.
#' If the pattern is not present, new col will be blank
#' 
#' @param dat data.frame : A dataset where we split the string column into two columns
#' 
#' @param original_col (string): It will contain the original string up to the start of the pattern
#' 
#' @param new-col (string): It will contain the string following the pattern
#' 
#' @return split_col_dat: The dataset with a string column split into two columns 
#'  @examples
#'  Example where data has two character columns with atleast one pattern
#'  dataset <- tibble::tibble("info" = c("chair", "chair \r\n (falling)")
#'                     
#'   split_column_by_pattern(dat = dataset, original_col = "info", 
#'   new_col = "direction", pattern = "\r\n\\s*\\(")
#'   
#' @export
split_column_by_pattern <- function(dat, original_col, new_col, pattern) {
  if( new_col %in% names(dat)) {
    warning(paste(new_col, "is already in dat"))
    return(dat)
  }
  split_col_dat <-  dat %>% 
    mutate(split = str_split_fixed(!!sym(original_col), pattern, 2))  %>% 
    mutate(
      !!sym(original_col) := str_trim(split[,1]),
      !!sym(new_col) := split[,2]) %>% 
    select(-split)
  return(split_col_dat)
}