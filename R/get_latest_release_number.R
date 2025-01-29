#' @title get_latest_release_number
#' @description get_latest_release_number - Get the vintage from the release number
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' get_latest_release_number - Get the vintage from the release number
#'  
#' Use the highest release number to determine vintage. This works regardless of
#' whether e.g. "first", "1st" or a mix of these is used.
#' 
#' If more than one vintage was found, the user is given a warning with the other 
#' releases found
#' 
#' If the highest release_number is 1st, or first, vintage is "provisional"
#' If the highest release_number is 2nd, or second, or higher, vintage is "final"
#' 
#'
#' @param release_numbers (character vector). These can be either 1st, 2nd etc, or first, second etc
#'
#' @return latest_release_number (string)
#' 
#' @examples 
#' release_numbers <- c("2nd release", "first release")
#' get_latest_release_number(release_numbers)
#' 
#' @export
get_latest_release_number <- function(release_numbers) {
  # tibble required for getting max from character string
  release_numbers_tibble <- tibble(release_text = release_numbers)
  latest_release_number <- max(release_numbers_tibble$release_text)
  
  if (grepl("first|1st", latest_release_number) == TRUE){
    vintage <- "provisional"
    
  } else if (grepl("first|1st", latest_release_number) == FALSE) {
    vintage <- "final"
    
  }
  
  if (length(release_numbers) >= 2) {
    
    warning( paste0("vintage has been assigned as ", vintage, 
                    ", but ", release_numbers_tibble, 
                    " were referred to in the front page tab."))
  }
  
  return(vintage)
}