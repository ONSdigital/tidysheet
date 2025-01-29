#' @title get_left_headers
#' @description  Get the column names for the left block
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' Create a column for grouping variables that are in the same column as their nested items
#' 
#' grouping variables in rows (group_row items) don't have a value associated 
#' with them because they are just the group under which other items 
#' (nested_row_1 items) sit. Put these grouping items in a column in 
#' their own right, so that 'nested_row_1' items are nested under 
#' 'group_row items' in a tidy structure.
#' 
#' Turn data that look like this:
#' 
#' |     from     | numeric |
#' |--------------|---------|
#' | Education    |   NA    |
#' |    primary   |    1    |
#' |    secondary |    3    |
#' | Transport    |   NA    |
#' |    roads     |    5    |
#' 
#' Into this:
#' 
#' |     from     | row_header | numeric |
#' |--------------|------------|---------|
#' |    primary   | Education  |   1     |
#' |    secondary | Education  |   3     |
#' |    roads     | Transport  |   5     |
#' 
#' 
#'  
#' @param dat dataframe or tibble. Imported by xlsx_cells. Must incllude a column 
#' called 'row'.
#' @param row_header string. Name of new column to contain the grouping variable
#' @param from string. Name of the column currently containing both the
#' grouping variable and the nested variable. Must exist in dat
#' @param fill_dir string. Currently either "down", "up", 
#' "downup" (i.e. first down and then up) or "updown" (first up and then down).
#' @param group_row_na_identifier string.Either the name of a column in dat that contains NAs for
#' rows containing the grouping variable, or 'all' (the default), in which case 
#' grouping variable rows are identified by being associated with an otherwise 
#' blank row.
#' @return tibble. dat but with a new column for group_row
#' 
#' @examples 
#' sample_data <- data.frame(
#'   address = c('B3', 'B4', 'B5', 'B6', 'B7'),
#'   row = c(3:7),
#'   col = 2,
#'   is_blank = c(TRUE, rep(FALSE, 2), TRUE, FALSE),
#'   data_type = c('blank', rep('numeric', 2), 'blank', 'numeric'), 
#'   numeric = c(NA, 1, 3, NA, 5),
#'   subservice = c('education', 'primary',  'secondary', 'Transport', 'roads')
#' )
#' 
#' add_row_headers_as_column(sample_data, 'service', 'subservice', 'down')
#' 
#' 
#' @export
add_row_headers_as_column <- function(dat, row_header, from, fill_dir, group_row_na_identifier="all") {
  
  if (group_row_na_identifier == "all") {
    
    # get cases where there are ONLY blanks associated with the row
    not_groups <- dat %>% 
      filter(is_blank == FALSE) %>% 
      distinct(!!sym(from)) %>% 
      pull()
    potential_groups <- dat %>% 
      filter(is_blank == TRUE) %>% 
      distinct(!!sym(from)) %>% 
      pull()
    groups_array <- setdiff(potential_groups, not_groups)
    
    column_created <- dat %>% 
      mutate(!!sym(row_header) := ifelse(
        !!sym(from) %in% groups_array, 
        !!sym(from), NA))
    
  } else if (group_row_na_identifier %in% names(dat)) {
    
    column_created <- dat %>% 
      mutate(!!sym(row_header) := ifelse(is.na(!!sym(group_row_na_identifier)), 
                                         !!sym(from), NA)) 
    
  } else {
    warning("group_row_na_identifier must be either 'all' or the name of a coumn in the unpovotted (preprocessed) data")
  }
  
  # in scot gov LFR_CR data some nested_row_1 items are incorrectly identified
  # as group_row items because one or more of the data columns is blank
  # FIX!
  
  row_headers_added_as_column <- column_created %>% 
    # need to make sure data are in order of row so that fill works correctly
    arrange(row) %>% 
    fill(!!sym(row_header), .direction = fill_dir) 
  
  return(row_headers_added_as_column)
}
