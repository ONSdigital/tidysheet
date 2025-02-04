#' @title get_first_header_row
#' @description Get the first header row index based on specified identifiers
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details
#' This function takes a date as a character, a list of header identifiers, a list of second identifiers, and a file part as input.
#' It identifies the first row of the header based on the specified identifiers and returns its index.
#' @param date_as_character A data table of the excel file.
#' @param header_identifier A list of header identifiers to search for in the file part.
#' @param second_identifier A list of second identifiers to search for in the file part.
#' @param file_part A file part to search for the identifiers, usually 1, or sometimes 2.
#'
#' @return The index of the first header row.
#' @examples
#'
#'  first_header_row <- get_first_header_row(date_as_character, header_identifier, second_identifier, file_part)
#' @description
#' This function searches for the specified header identifiers and second identifiers (if provided) in the file part.
#' It then identifies the first row of the header based on the found identifiers and returns its index.
#' @export
get_first_header_row <- function(date_as_character, header_identifier, second_identifier, file_part) {
  if (!is.na(second_identifier)) {
    header_identifier <- get_header_identifier(second_identifier, file_part)
    second_identifier_var <- TRUE
  } else {
    header_identifier <- get_header_identifier(header_identifier, file_part)
    second_identifier_var <- FALSE
  }

  first_header_identifier <- get_first_instance(date_as_character, header_identifier)
  if (second_identifier_var == TRUE){
    first_header_row <- first_header_identifier - 1
  } else {
    first_header_row <- first_header_identifier
  }

  if (is.numeric(first_header_row)){
    message(paste0('The first header row has been identified as row ', first_header_row))
  } else {
    stop('First header row has not been identified. Please see warnings.')
  }

  return(first_header_row)
}

