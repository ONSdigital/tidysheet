#' @title combine_headings
#' @description  concatenate columns that hold raw data header information
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' concatenate columns that hold raw data header information
#'
#' @param dat dataframe
#' @param headers_to_combine  vector of strings taken from the data dict. Each 
#' string is the identifier of a column - this can either be its position e.g.
#' 'group_col' or the name of group_col, e.g. 'item'
#'
#' @return dat with columns concatenated
#' @examples
#' dat <- data.frame("group" = c("Fox", "Fox", "Bear"),
#'                   "common_name" = c("Red", "Arctic", "Black"),
#'                   "item_detail" = c("Vulpes vulpes", "Vulpes lagopus", "Ursus americanus"),
#'                   "value" = 1:3)
#' newdat <- combine_headings(dat, 
#'                            headers_to_combine = c("item_detail", "group", "common_name"))
#' newdat  
#' @export                          
combine_headings <- function(dat, headers_to_combine) {
  
  count <- length(headers_to_combine)
  
  if (count == 1) {
    
    warning("headers_to_combine in the data dict (sheet_structure) gives only  one column, where either 2 or 3 are required,e.g. ['nested_col_1', 'group_col']")
    
  } else if (count == 2) {
    
    combined_name <- headers_to_combine[1]
    name_to_add_1 <- headers_to_combine[2]
    
    output <- dat %>% 
      unite(col = !!sym(combined_name),
            !!sym(name_to_add_1), !!sym(combined_name), # cols to unite
            na.rm = TRUE,
            sep = " - ")
    
  } else if (count == 3) {
    
    combined_name <- headers_to_combine[1]
    name_to_add_1 <- headers_to_combine[2]
    name_to_add_2 <- headers_to_combine[3]
    
    output <- dat %>% 
      unite(col = !!sym(combined_name),
            !!sym(name_to_add_1), !!sym(name_to_add_2), !!sym(combined_name),
            na.rm = TRUE,
            sep = " - ")
  } else {
    warning("headers_to_combine in the data dict (sheet_structure) gives too many columns, where either 2 or 3 are required,e.g. ['nested_col_1', 'group_col']")
  }
  
}
