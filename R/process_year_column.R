#' @title process_year_column
#' @description Process the 'year' column in a data frame
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' Process the 'year' column in a data frame
#'
#' Process the 'year' column in a data frame according to specific conditions.
#' Format year so that it follows the structure yyyy-yy.
#'
#' @param dat The data frame containing the data to be processed.
#' @param year_value_patterns Regular expression pattern to match year values.
#' @param year_for_layout Year value to be used for creating a new 'year' column if needed.
#' 
#' @return The modified data frame after processing the 'year' column.
#'
#' @examples
#' # Sample usage:
#' output <- data.frame(year_col = c("2021", "2022-23"))
#' year_for_layout <- "2023"
#' processed_output <- process_year_column(output, year_value_patterns, year_for_layout)
#' @export
process_year_column <- function(dat, year_pattern, year_for_layout){
  
  lowercase_names <- tolower(names(dat))
  
  # If there's more than one column with 'year' in the name we need to treat
  # the creation of 'year' column differently to if there is only one.
  # If a column called exactly "year" exists, it will stay the same.  
  names_with_year <- names(dat)[str_detect(lowercase_names, "year")]
  potential_year_cols <- names_with_year[tolower(names_with_year) != "year"]
  
  year_in_names <- "year" %in% lowercase_names
  year_original_case <- names(dat)[which(lowercase_names=="year")]
  
  only_one_year_column <- length(potential_year_cols) == 1
  multiple_year_column <- length(potential_year_cols) >= 2
  
  if (only_one_year_column & !year_in_names) {
    # by the time code reaches here this list will only have 1 value
    year_col <- potential_year_cols[[1]]
    
    # If not all values in the column match the year pattern we shouldn't 
    # use it as 'year'
    year_values_in_data <- all(str_detect(dat[[year_col]], year_pattern))
    
    if (year_values_in_data) {
      dat_with_year <- rename(dat, year = all_of(year_col))
      warning(paste0("Year values found in '", year_col, "' so it has been renamed 'year'"))
    } else if (!year_values_in_data) {
      dat_with_year <- mutate(dat, year = year_for_layout)
      warning("No 'year' column found in data. Using Year Method specified in earlier warning message. Please check that the year column contains the correct values in the pre-processed data")
    }
    
  } else if (multiple_year_column & !year_in_names) {
    dat_with_year <- mutate(dat, year = year_for_layout)
    warning("Multiple columns with 'year' in the name have been found, but none are called just 'year': None have been renamed 'year' as it is not known which to use. Using the Year Method specified in earlier warning message. Please check that the year column contains the correct values in the pre-processed data")
    
  } else if (year_in_names) {
    dat_with_year <- dat %>% 
      rename(year = !!sym(year_original_case))
    
  } else {
    dat_with_year <- mutate(dat, year = year_for_layout)
    warning("No 'year' column found in data. Using Year Method specified in earlier warning message. Please check that the year column contains the correct values in the pre-processed data")
  }
  
  # now that the column called 'year' contains the financial year, we can 
  # standardise the format
  separator_word_patterns <- "(\\b\\sto\\s\\b)|(to)|(_to_)"
  separator_symbol_patterns <- "(\\/)|(\\s\\/\\s)|(\\/\\s)|(\\s\\/)|(\\b\\s-\\s\\b)"
  
  output <- dat_with_year %>% 
    mutate(year = str_replace(year, separator_symbol_patterns, "-")) %>% 
    # we don't want to replace "to" with "-" in year if it is e.g 
    # "2021-22 to 2022-23" so do this in an if else statement.
    mutate(year = 
             ifelse(str_detect(year, "-") == FALSE,
                    str_replace(year, separator_word_patterns, "-"),
                    year)
    )
  
  return(output)
}