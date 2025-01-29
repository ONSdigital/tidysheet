#' @title get_first_instance
#' @description  Identify the first row or column on which a string occurs
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' Identify the first row or column on which a string occurs
#'
#' Takes an xlsx_cells dataframe and a string known to be
#' in the row or column you wish to identify but not in any previous rows/cols. 
#' Returns the row/col number from the original excel sheet.
#' This function is not case sensitive - it will for example identify 
#' pattern = "Blue" as a match with "blue" in the dataframe. 
#'
#' @param dat  An xlsx_cells dataframe
#' @param pattern string 
#' @param column "row" or "col". Default is row
#'
#' @return numeric. The first row/col from the xlsx on which known_string occurs. 
#'         Defaults to 1 if dat is not an xlsx_cells df.
#'
#' @examples
#' dat <- data.frame("character" = c("title", "million", NA, 
#'                                  "Education services", "Highways services",
#'                                  NA),
#'                   "row" = c(1:3, 4, 4, 5),
#'                   "numeric" = c(rep(NA, times = 5), 50))
#' get_first_instance(dat, "services")
#'
#' @export
get_first_instance <- function(dat, pattern, column = "row") {
  
  
  if (all(c("character", column) %in% colnames(dat)) == FALSE){
    
    stop(paste0("dat must contain columns named character and ", column))
  }
  
  pattern_cleaned <- stringr::str_squish(tolower(pattern))
  
  tryCatch(
    {
      first_location <- min(grep(pattern_cleaned,
                                 stringr::str_squish(
                                   tolower(dat$character)
                                 )))
      # Because xlsx_cells dfs have a row per cell, there are many rows for
      # each row in the original dataframe. So, unlike with other ways of reading
      # a dataframe, first_row_location does not equal row number. To get that we
      # need this next line
      first_instance <- dat[first_location, ][[column]]
      
      return(first_instance)
      
    },
    error=function(e) {
      message('An error occurred in get_first_instance')
      print(e)
    },
    
    warning=function(w) {
      message(paste0("Failed to find the pattern '", pattern, "' in the source data. 
      Please check with a developer that (where relevant) header_identifier,
      nested_column_1, combine_start_row_identifier, and combine_end_row_identifier 
      are correct for this source in dev_config and data_dict respectively."))
      print(w)
    }
  )
}