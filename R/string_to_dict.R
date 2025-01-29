#' @title string_to_dict
#' @description Generate a dictionary-style dataframe from an input string
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @details 
#' Generate a dictionary-style dataframe from an input string
#' 
#' The form of a Python dict brought into R as a string is 
#' "{key: item, key: item}". 
#' 
#' In R, to access items using their key we convert this string into a dataframe 
#' where the first column contains keys, and the second column contains item 
#' lists. 
#'  
#' This is not written for use with nested dictionaries.
#' 
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @param input_string A character string containing key-value pairs separated 
#' by commas. See example.
#' 
#' @return A data frame representing a dictionary with keys corresponding 
#' to a list of items. If the input string is empty, an empty data frame is returned.If the 
#' input string contains any empty keys, they are replaced with NA values.
#' 
#' @examples
#' # simple
#' input_string <- "{key1: value1, key2: value2, key3: value3}"
#' result_dict <- string_to_dict(input_string)
#' 
#' # nested
#' input_string <- "{key1: [value 1, value 1b], key2: [value2, value2b], key3: value3}"
#' result_dict <- string_to_dict(input_string)
#' 
#' @export
string_to_dict <- function(input_string) {
  
  if (input_string == "{}") {
    warning("Empty input string provided. Returning an empty dictionary.")
    return(data.frame(key = character(0), item = character(0)))
  }
  
  # We no longer need the curly braces - they are just an artifact of bringing 
  # a dict into r as a string
  no_braces <- stringr::str_remove_all(input_string, "\\{|\\}")
  squished <- stringr::str_squish(no_braces)
  no_quotes <- stringr::str_replace_all(squished, '"', '')
  no_colon_spaces <- stringr::str_replace_all(no_quotes, " :", ":")
  
  # find the location of the keys so that we can split the string into keys and items
  # look for any word (including underscores) immediately followed by a colon
  key_locs <- stringr::str_locate_all(no_colon_spaces, "\\w+:")[[1]]
  
  # so that the split occurs just after the colon, 
  # we need to add 1 to the end loc
  split_locs <- c((key_locs[, "start"] - 1), 
                  (key_locs[, "end"] + 1))
  
  # The first split location will always be 0 so we need to remove this one.
  # We need to order the locations as we will split the string in the order 
  # given in this vector
  split_points <- sort(split_locs[2:length(split_locs)])
  
  key_item_vector <- substring(no_colon_spaces,
                               c(1, split_points),
                               c(split_points, nchar(no_colon_spaces)))
  
  keys <- tibble(key_item_vector) %>% 
    filter(row_number() %% 2 != 0 ) %>% 
    rename(key = key_item_vector) %>% 
    mutate(key = stringr::str_remove(key, ": "),
           key = stringr::str_squish(key))
  
  items <- tibble(key_item_vector) %>% 
    filter(row_number() %% 2 == 0 )  %>% 
    rename(item = key_item_vector) %>% 
    mutate(item = trimws(item, whitespace = "[, ]")) %>% 
    # because of how it is passed from Python, the first and last character of 
    # each item at this point will be a square brace. We need to remove these.
    # Doesn't need case_when if it always came from Python, but during dev
    # we pass arguments from the pre-processing_setup.r file, which could cause
    # issues if we forgot to put the square braces in.
    mutate(item = 
             ifelse(
               substr(item, 1, 1)=="[" & substr(item, nchar(item), nchar(item))=="]",
               substr(item, 2, nchar(item)-1), 
               as.character(item)
             )
    )
  
  dict <- bind_cols(keys, items) %>% 
    mutate(item = stringr::str_split(item, ","))
  
  return(dict)
}
