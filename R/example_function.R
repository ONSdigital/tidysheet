#' @title example_function
#' @description value
#' An empty function to copy as a template for new functions.
#' Delete when the package is finished
#' @details value
#' An Example Function
#'
#'
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#'
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @examples
#' example_function()
#' @return NULL
#' @export
example_function <- function(love=TRUE){
  if(love==TRUE){
    print("I love cats!")
  }
  else {
    print("I guess you dont love cats")
  }
}
