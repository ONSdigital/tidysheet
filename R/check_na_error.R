#' @title check_na_error
#' @description  Check if variables are NA and stop if any are
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' Check if variables are NA and stop if any are.
#'
#' This function checks if the given variables are NA. If any of them are,
#' the function stops execution with an error message containing the variable
#' name and the message "is NA". Ellipsis (...) allows the function to accept
#' any number of arguments.
#'
#' @param ... Variables to be checked for NA.
#'
#' @examples
#' header_identifier <- "header"
#' check_na_error(header_identifier)  # No error, as header_identifier is not NA
#'
#' group_col <- NA
#' check_na_error(group_col)  # Error: "group_col is NA"
#' 
#' Note for multiple arguments
#'
#' @export
check_na_error <- function(...) {
  variables <- list(list(...), substitute(...()))
  values <- unlist(variables[[1]])
  names <- unlist(variables[[2]])
  
  na_variables <- character()
  for (i in 1:length(values)) {
    if (is.na(values[i])) {
      na_variables <- c(na_variables, names[i])
    }
  }
  
  if (length(na_variables) > 0) {
    stop(paste(
      paste(na_variables, collapse = ", "),
      "not defined in the data dictionary. Please contact a developer to resolve the issue"
    ))
  }
}