#' @title get_layout_type
#' @description  Get the layout type from the data dictionary
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' Get the layout type from the data dictionary
#' 
#' Find which layout type the file is (given the year). 
#' 
#' 
#' @param year character string e.g. 2021 
#' @param layout_dict dataframe with key and item columns. annual/Financial year
#' is given in the key column
#' 
#' @return The item from layout_dict for the year in question
#' 
#' @examples
#' layout_dict <- data.frame("key" = c("from 2019-20", "from 2020-21", "from 2020-21"),
#'                            item = c("single_header", "multi-header", "single_header"))
#' layout <- get_layout_type(2021, layout_dict)
#' print(layout)
#' 
#' @export
get_layout_type <- function(year, layout_dict, element=1) {
  
  year_start <- as.numeric(substr(year, 1, 4))
  
  layout_types <- layout_dict %>% 
    mutate(from_year = as.numeric(str_extract(key, '[0-9]{4}'))) %>% 
    filter(from_year <= year_start) %>% 
    # this gives all years previous to the current year, so just take the most recent
    arrange(desc(from_year)) %>% 
    filter(row_number()==1) 
  
  layout_type <- pull(layout_types, item)  
  
  # check for issues in the dictionary
  distinct_items <- layout_dict %>% 
    filter(key %in% layout_types$key) %>% 
    distinct(item) 
  
  item_count <- lengths(distinct_items$item)[1]
  
  if (element > item_count) {
    warning("Expecting ", element, " layouts for ", year, " but only ", item_count, " given in data dict. Please speak to a developer." )
  }
  
  if (length(layout_type) == 0) {
    layout_type <- NA
    stop(paste0(
      "No layout type was found for the given year (", year, "). This may be because the earliest year in the data dict for layout_type is later than the year found for the file. Please contact a developer for help.")
    )
  } else  if (item_count > 1) {
    warning(
      "Layout type has more than one element for ", year ,", now using element ", element, ": ", str_squish(layout_type[[1]][element])
    )
  }
  
  output <- str_squish(layout_type[[1]][element])
  
  return(output)
  
}