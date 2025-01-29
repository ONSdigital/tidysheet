#' @title join_cols_data
#' @description checks if any reserved words are used, halts if they are
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details Check for reserved words in sheet_structure settings
#'
#' @param dat dataframe
#' @param columns_to_concat vector of strings of column names to concatenate. 
#' @return tibble. Tidy dataframe. 
#'
#' @examples my_out <- join_cols_data(dat = new_simple, columns_to_concat = my_columns, col_join_name = 'description'))
join_cols_data <- function(dat, columns_to_concat, col_join_name = 'description') {
  # basic join of listed columns
  if (length(columns_to_concat) >0 ) {
    # we have some columns to join together
    
    # replace NA with empty strings in the columns to be concatenated. 
    dat <- dat %>%
      mutate(across(all_of(columns_to_concat), ~replace_na(., "")))
    
    # Using concatenate columns passed in, into a named new column
    dat[[col_join_name]] <- do.call(paste, c(dat[columns_to_concat], sep = " "))
    
    # reorder the columns to make the new one come after the last joined one
    dat <- dat %>%
      relocate(col_join_name, .after = tail(columns_to_concat, 1)) 
    
    dat <- dat %>%
      relocate(all_of(col_join_name), .after = tail(columns_to_concat, 1))
  } else {
    # we dont, so pass through
  }
  
  return(dat)
}
