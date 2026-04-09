#' @title Read in data using xlsx_cells
#'
#' @description Determine which sheet to read based on a regular expression
#' and read in the data using tidyxl::xlsx_cells().
#'
#' @param filepath character string. Full filepath of an xlsx file.
#' @param pattern character string. Regular expression to match the required
#' sheet.
#'
#' @returns dataframe in an xlsx_cells format with one row for every cell of
#' the excel data. If pattern is not provided NULL is returned. If filepath
#' is not fir an xlsx file an error is raised.
#'
#' @export
get_data <- function (filepath, pattern) {

  if (is.na(pattern)) {
    return(NULL)
  }

  if (all(!endsWith(filepath, '.xlsx'), !endsWith(filepath, '.xlsm'))) {
    stop(
      "Data must be in an xlsx file. The following filepath does not meet, ",
      "this expectation: '", filepath, "'. Please contact a developer."
    )
  }

  if (file.exists(filepath)) {
    sheets <- tidyxl::xlsx_sheet_names(filepath)
    message("Available sheets: '", paste0(sheets, collapse = "', '"), "'.")
    tabname <- match_sheet_to_regex(sheets, pattern)

    if (is.null(tabname)) {
      stop(
        "No sheets found with a name that matches the pattern: '", pattern, "'.",
        "Data cannot be preprocessed because it is not known which tab to use. ",
        "Please contact a developer."
      )
    }

    dat <- xlsx_cells(filepath, sheets = tabname)

    message("Data read from the '", tabname, "' sheet of ", filepath, ".")

    return(dat)
  } else {
    stop("filepath '", filepath, "' does not exist.")
  }

}


#' @title Find values in a vector that match a regular expression
#'
#' @param sheet_names Vector of character strings.
#' @param pattern Regular expression to be detected in sheet_names.
#'
#' @return List of character strings. Sheet names that fitted regex pattern
#'
#' @examples
#' \dontrun{
#' sheet_names <- c("Front Page", "LA drop-down", "RA data", "RA other")
#' pattern <- "RA|Data"
#' match_sheet_to_regex(sheet_names, pattern)
#' }
#' @export
match_sheet_to_regex <- function(sheet_names, pattern){

  if (all(is.na(pattern))) {return(NULL)}

  if (!is.character(sheet_names)){
    stop("Sheet names should be a vector of character strings")
  }
  if (!is.character(pattern)){
    stop("Pattern should be a character string")
  }

  matched_sheets <- sheet_names[stringr::str_detect(sheet_names, pattern)]

  if (length(matched_sheets) > 1){
    warning(
      "Multiple Sheets have matched the tabname_regex ('",
      paste0(matched_sheets, collapse = "', '"), "'). Only '",
      matched_sheets[[1]], "', will be preprocessed. Please contact a ",
      "developer if this is the incorrect sheet."
    )
  }

  if (length(matched_sheets) == 0){
    warning(
      "No sheets have beeen identified for preprocessing for this dataset. ",
      "Are the sheetnames different to previous years? The tabname_regex in ",
      "the dev_config may need updating. Please contact a developer if this ",
      "is the case."
    )
    return (NULL)
  }

  return(matched_sheets[[1]])
}


#' @title Split metadata at the top of the sheet from the rest of the data.
#'
#' @description
#' Many Excel datasets have metadata at the top of each sheet giving, for
#' example, the title, units, and notes. This function separates out the sheet
#' metadata from the rest of the sheet.
#'
#' @details
#' Where sheets contain multiple tables, each of the sub tables may have their
#' own blocks of metadata. This function *does not* remove the metadata from
#' subtables.
#'
#' @param dat dataframe imported using xlsx_cells.
#' @param pattern character string. A regular expression to search for in the
#' data to identify the first header row. In pub sec this variable is specified
#' with header_identifier.
#' @param instance An integer specifying which instance of the header identifier
#' pattern to use. Defaults to 1 (the first instance). In pub sec this variable
#' is specified with header_identifier_instance.
#' @param offset_by An integer specifying the number of rows to offset
#' the header row. Defaults to 0. In pub sec this variable is specified with
#' header_row_offset. If the row the pattern is found on is the row after the
#' first header row, offset_by will be 1. If the pattern is on the row before
#' the first header row, offset_by will be -1.
#'
#' @returns named list of dataframes. Tables are called 'data' and 'metadata'.
split_data_from_sheet_info <- function(dat, pattern, instance, offset_by) {

  message(
    "Splitting metadata at the top of the sheet from the rest of the data"
  )

  first_header_row <- get_header_row(dat, pattern, instance, offset_by)

  data <- get_table_data(dat, first_header_row)

  metadata <- get_info_above_table(dat, first_header_row)

  table_list <- list(
    "data" = data, "metadata" = metadata
  )

  return(table_list)
}


#' @title Get the table data (excluding information above the table)
#'
#' @description Retain only data that is part of the table itself.
#'
#' @param dat dataframe imported using tidyxl::xlsx_cells().
#' @param row_num The row number of the first row that is to be included. Any
#' rows with a smaller row number will be removed.
#'
#' @returns dataframe. dat with rows prior to row_num removed.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'     row = as.numeric(1:4),
#'     col = 1,
#'     character = c("title", NA, "first_header", NA),
#'     numeric = c(NA, 1995, NA, 1000)
#'     )
#' get_table_data(dat, 3)
#' }
#'
get_table_data <- function(dat, row_num) {

  message("Getting dat from the first header row onwards.")

  if (is.na(row_num)) {
    warning(
      "No data removed from above the first header row as the first row was ",
      "not identified."
    )
    return(dat)
  } else {
    return(filter(dat, row >= row_num))
  }

}


#' @title get the string information from above the data in an excel sheet.
#'
#' @description Get a subset of the data from dataset imported using tidyxl::
#' xlsx_cells() where each row describes a cell in an excel sheet. All character
#' string information above a specified row is returned. If data_type is
#' anything other than character the row is removed.
#'
#' @param dat dataframe imported using tidyxl::xlsx_cells().
#' @param row_num The row number of the first row that is to be excluded.
#' @return dataframe in an xlsx_cells layout containing everything from dat that
#' is in rows prior to row_num.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'     row = as.numeric(1:4),
#'     col = 1,
#'     character = c("title", NA, "first_header", NA),
#'     numeric = c(NA, 1995, NA, 1000)
#'     )
#' get_info_above_table(dat, 3)
#' }
#' @export
get_info_above_table <- function(dat, row_num) {

  message("Getting metadata from above the first header row.")

  output <- dat %>%
    filter(row < row_num) %>%
    filter(!is.na(character))

  if (nrow(output) == 0) {
    message("No information found above the first header row.")
  }

  return(output)
}
