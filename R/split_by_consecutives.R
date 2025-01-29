#' @title split_by_consecutives
#' @description  split a numeric vector into sets of consecutive numbers
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' split a numeric vector into sets of consecutive numbers
#'
#' @param x vector of integers
#' @returns integer
#' 
#' @examples output <- split_by_consecutives(x)
#' @export
split_by_consecutives <- function(x) {
  
  ordered <- sort(x)
  output <- split(ordered, cumsum(c(1, diff(ordered) != 1)))
  
  return(output)
  
}