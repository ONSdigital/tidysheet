#' @title build_regex
#' @description Convert instructions into a regex pattern
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @details
#' Convert instructions into a regex pattern
#' 
#' Because it is problematic to pass regex patterns from the data dictionary
#' as an argument through subprocess in Python, we can instead use this function
#' to build the regex pattern inside R.
#' 
#' @param instructions vector of strings containing words given in regex_lookup.
#' Each string in the vector can have multiple instruction words in it.
#' Each instruction word is converted to a regular expression and these are 
#' pasted together to create a single pattern from each string. Must not 
#' contain punctuation.
#' 
#' @param regex_lookup datafarame containing the columns 'word' and 'pattern'. 
#' Each row of 'word' is a single word instruction that relates to a regex
#' pattern separated by single underscores. See examples. 
#' 
#' @returns a vector of regular expressions; one for each instruction string
#' Raises an error if there is an instruction that does not appear in 
#' regex_lookup.
#'     
#' @examples 
#' regex_lookup <- data.frame(
#'     "word" = c("colon", "whitespace", "poundsign", "newline"),
#'     "pattern" = c("\\:", "\\s*", "\\�", "(\r\n|\r|\n)"))
#'     
#' instructions <- c("colon_whitespace", "newline_poundsign_newline")
#' 
#' build_regex(instructions, regex_lookup)
#' @export
build_regex <- function(instructions, regex_lookup){
  
  instruction_list <- strsplit(instructions, "_")
  
  pattern <- NULL
  instruction_patterns <- NULL
  
  for (i in 1:length(instruction_list)) {
    
    components <- instruction_list[[i]]
    
    for (j in 1: length(components)) {
      
      keep_as_is <- grepl("(^\\[).*(\\]$)", components[j])
      keep_no_equals <- grepl("^=", components[j])
      
      if (keep_as_is) {
        
        pattern <- paste0(pattern, components[j])
        
      } else if (keep_no_equals) {
        
        pattern <- paste0(pattern, substr(components[j], 2, nchar(components[j])))
        
      } else if (components[j] %in% regex_lookup$word){
        
        pattern_part <- regex_lookup %>% 
          filter(word == components[j]) %>% 
          pull(pattern)
        
        pattern <- paste0(pattern, pattern_part)
        
      } else {
        stop(paste0("'", components[j], "' passed to R from the data dictionary is not present in the regex_lookup. Please ask a developer to edit the dictionary or add this to the regex_lookup"))
      }
    }
    instruction_patterns <- c(instruction_patterns, pattern)
    pattern <- NULL
  }
  
  return(instruction_patterns)
}