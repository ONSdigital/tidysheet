#' @title get_first_of_consecutives
#' @description  Get the first integer where the following number is consecutive
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' Get the first integer where the following number is consecutive
#' 
#' This function returns the first number in a vector where the next number is 
#' the consecutive number.
#' 
#' For example, if you pass it a vector of numbers that are column positions
#' where the datatype is numeric, it will return the position of the first 
#' column where both it and the following column are numeric. 
#' 
#' @param x vector of integers
#' @returns integer. If there are consecutive values, this will be the first of 
#' the consecutive values. If there are no consecutive values a warning will
#' be given and the first value returned.
#' 
#' @examples
#' x <- c(1, 1, 3, 5, 6, 8)
#' 
#' get_first_of_consecutives(x)
#'
#' @export
get_first_of_consecutives <- function(x) {
  
  consecutives_together <- split_by_consecutives(x)
  consecutive_sets <- lengths(consecutives_together) > 1 
  
  if (any(consecutive_sets)) {
    first_set <- consecutives_together[consecutive_sets==TRUE][[1]]
  } else {
    # If there are no consecutive values, return the first occurrence
    first_set <- x
    warning("no consecutive values were found. This may have resulted in an error in identifying the row or column on which data start.")
  }
  return(first_set[1])
  
}