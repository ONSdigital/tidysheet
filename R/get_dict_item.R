#' @title get_dict_item
#' @description Get value for a key in a python dict (as a data-frame)
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @details
#' Get value for a key in a python dict (as a data-frame)
#' 
#' Used on a python dictionary that has been turned into a data-frame with a 
#' column for key and a column for item
#' 
#' @import stringr 
#'  
#' @param key character string that is present in dict
#' @param dict dataframe with key and item columns. 
#' 
#' @return The item from dict for the key in question
#' 
#' @examples
#' layout_dict <- data.frame("key" = c("header", "group"),
#'                            "item" = c("NA", "mice"))
#' group <- get_dict_item("header", layout_dict)
#' print(group)
#' 
#' @export
get_dict_item <- function(key, dict) {
  
  if (key %in% dict$key){
    item <- stringr::str_squish(dict$item[dict$key == key][[1]]) 
    if (all(item == "NA")) {
      item[item == "NA"] <- NA
    }
  } else {
    item <- NA
  }
  return(item)
  
}