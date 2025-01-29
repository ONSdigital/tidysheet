#' @title trim_separators
#' @description Remove hyphens, colons etc, and spaces from the start and end of strings 
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' Remove hyphens, colons etc, and spaces from the start and end of strings 
#' 
#'
#' @param string character string. 
#' 
#' @return vector of strings. Original vector with trailing separators removed 
#'
#' @examples
#' example_vector <- c("these - ", "hyphens-", " and colons :", ":now   gone- ")
#' trim_separators(example_vector, "-|:")
#'
#' @export
trim_separators <- function(string, separators) {
  if (!is.character(string)) {
    stop("String should be a character vector")
  }
  
  for(i in 1:length(separators)) {
    string <- string %>% 
      stringr::str_trim() %>% 
      stringr::str_remove_all(pattern = paste0("^", separators[i], "+")) %>%
      stringr::str_remove_all(pattern = paste0(separators[i], "+$")) %>% 
      stringr::str_trim() 
  }
  
  return(string)
}
