#' @title get_release_number_vintage
#' @description get_release_number_vintage_version - Get the version from a release number in a tibble/dataframe
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' get_release_number_vintage_version - Get the version from a release number in a tibble/dataframe
#' 
#' Get the release number (first, 1st, second, 2nd etc) from the column of a 
#' dataframe or tibble called 'character'. Convert this into  vintage (either 
#' 'provisional' when 1st release, or 'final' when anything later than first).
#' 
#' 
#' @param character_matches: A dataframe containing a column called 'character' that contains the release information.
#' 
#' @return string: the vintage or NA 
#'
#' @examples
#' character_matches <- c(NA, "character(0)", 
#'                      "Data from this workbook have been used to compile 'LA 2022-23 Outturn, England - Second release' .",
#'                      "notes on first release")
#' front_page_sheet <- data.frame(character = character_matches)
#' get_release_number_vintage(character_matches)
#' 
#' @export
get_release_number_vintage <- function(front_page_sheet) {
  
  # Pattern to find word before release
  pattern <- "(\\S+)\\s+release" 
  sec_pattern <- "first|1st|second|2nd|third|3rd|fourth|4th|fifth|5th|sixth|6th|seventh|7th"
  
  # extract the strings that match
  vector_str_extract <- str_extract_all(
    str_squish(tolower(front_page_sheet$character)), pattern
  )
  
  # extract strings that have release in it
  character_matches_list <- vector_str_extract[
    0==FALSE & 
      grepl("release", vector_str_extract)==TRUE & 
      is.na(vector_str_extract)==FALSE
  ]
  
  # clean strings
  if (length(character_matches_list) >= 1 ){
    
    character_matches <- unlist(character_matches_list)
    
    # extract strings that match second pattern
    release_matches <- character_matches[
      grep(sec_pattern, character_matches, ignore.case = TRUE)
    ]
    
    if (length(release_matches) == 0) {
      
      warning("Release number not found in the front page. Vintage will be assigned using information in the main table")
      vintage <- NA
      
    } else if (length(release_matches) >= 1){
      
      release_numbers <- str_replace_all(
        unique(str_squish(tolower(release_matches))), 
        "[^a-zA-Z0-9 ]", "")
      
      vintage <- get_latest_release_number(release_numbers)
    }
    
  }  else if (length(character_matches_list) == 0) {
    
    warning("Release number not found in the front page. Vintage will be assigned using information in the main table")
    vintage <- NA
    
  }
  
  return(vintage)
}