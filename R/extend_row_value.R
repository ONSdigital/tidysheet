#' @title extend_row_value
#' @description extend_row_value adds information from above or below a row into another row.
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @details
#' extend_row_value adds information from above or below a row into another row.
#'
#' Uses pattern passed from the data dictionary to find values in the character column that match then using
#' variables passed from the data dict to decide which rows to add that found values to.
#'
#' This was written to address an issue in the LA Dropdown tab of the DLUHC capital payments receipts data, see example.
#'
#' @param source_data Tidyxl format dataframe (Data imported with tidyxl::xlsx_cells,
#' which have one row for each cell).
#' @param extend_row_regex (str).  regex to identify the rows that need added information from the row above or below
#' @param extend_row_order (str). Either forward or reverse. Used to determine if the added information is placed before or after original row value
#' @param extend_row_with (str). Either above or below. Used to determine if the added information is from above or below the row.
#'
#' @return source_data with modified values in the character column
#'
#' Pipeline in halted if extend_row_with and extend_row_order are not the expected variables or if
#' the pattern from the data dict doesnt match any character in the dataframe
#'
#' @examples (see confulence for visual example)
#'
#' data <- data.frame(
#'  id = 1:13,
#'  address = c("A9", "A10", "A11", "A12", "A13", "A14", "A15", "A16", "A17", "A18","A19","A20","A21"),
#'  character = c("Acquisition of land & existing buildings", "...of which HPA",
#'                "Another character", "...of which expenditure", "More characters", "...of which grants","...of which HRA",
#'                "Even more characters", "...of which receipts", "Some text", "...of which loans", "...of which payment","total","payment")
#')
#'
#'result <- extend_row_value(data, "^\\s*\\.\\.\\.\\s*of\\s*which", "reverse", "below")
#'
#'
extend_row_value <- function(source_data, extend_row_regex, extend_row_order, extend_row_with){

  # Determine whether we subtract from or add from the address value to find row value
  if (extend_row_with == 'above'){
    add_or_subtract_one <- - 1
  }else if (extend_row_with == 'below'){
    add_or_subtract_one <- 1
  } else{
    stop("'extend_row_with' in sheet_structure in the data dict must be either 'above' or 'below'. Please contact a developer to resolve the issue.")
  }
  # Filter the source_data based on the regex pattern
  filtered_data <- source_data %>%
    filter(grepl(extend_row_regex, character))
  # Check that the pattern found matches
  if (nrow(filtered_data) == 0) {
    stop("When attempting to combine information in one row with information in another, no text was found that matched extend_row_regex in data dict sheet_structure. Please contact developer.")
  }
  # Extract row numbers and addresses
  row_numbers <- filtered_data$row
  row_address <- filtered_data$address

  #--------------
  # get the position of the columns that contain pattern matches
  counts_by_column <- filtered_data %>%
    group_by(col) %>%
    count() %>%
    ungroup()

  most_common_column <- counts_by_column %>%
    filter(n == max(n))

  if (nrow(counts_by_column) == 1) {

    common_column <- pull(counts_by_column, col)

  } else if (length(counts_by_column) > 1) {

    common_column <- most_common_column %>%
      # assume that the column in which the pattern appears most is the one we are interested in
      filter(n == min(n)) %>%
      # if there are two columns with the same number of matches just use the first
      filter(col == min(col)) %>%
      pull(col)

    warning(paste0("More than one column was found with character matches for the extend_row_regex in sheet_structure of the data dict. Text matching the extend_row_regex in column ",
                   LETTERS[common_column], " will be combined to information in the row ",
                   extend_row_with, "."))
  }

  addresses <- filtered_data %>%
    filter(col == common_column) %>%
    mutate(extend_row_with_list = row + add_or_subtract_one,
           column_letter = str_extract(address, "[A-Z]")
    ) %>%
    mutate(combine_row = paste0(column_letter, extend_row_with_list))

  # Loop through each pair of rows to combine
  # for (i in seq_along(combine_dict$combine_row))
  for (i in seq_along(addresses$combine_row)) {
    # Get the original cell address and that of the cell we may want to combine
    combine_row <- addresses$combine_row[i]
    original_row_number <- addresses$address[i]

    # Find the indices of the rows in the source_data
    combine_index <- which(source_data$address == combine_row)
    original_index <- which(source_data$address == original_row_number)

    if (length(combine_index) > 0 & length(original_index) > 0) {
      # Get the character values
      combine_character <- source_data$character[combine_index]
      original_character <- source_data$character[original_index]

      # we need to check if the combine_character doesn't match the regex pattern,
      # if it does we need to look at the row above until we find the character that
      # doesn't match regex pattern
      while ((grepl(extend_row_regex,combine_character) == TRUE)) {
        i <- i + add_or_subtract_one
        combine_row <- addresses$combine_row[i]
        combine_index <- which(source_data$address == combine_row)
        combine_character <- source_data$character[combine_index]
      }

      # Update the character column based on combine_order
      if (extend_row_order == "forward") {
        character_to_add <- paste(original_character, combine_character)
      } else if (extend_row_order == "reverse") {
        character_to_add <- paste(combine_character, original_character)
      } else{
        stop("'extend_row_order' in sheet_structure in the data dict must be either 'forward' or 'reverse'. Please contact a developer to resolve the issue.")
      }

      # Assign the new character value to the original row value
      source_data$character[original_index] <- character_to_add
    }
  }

  return(source_data)
}
