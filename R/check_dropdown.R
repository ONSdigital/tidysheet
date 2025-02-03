#' @title check_dropdown
#' @description  Send check message about string is present in the character column
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details
#' Send check message about  string is present in the character column
#'
#' Data imported with tidyxl::xlsx_cells have one row for each cell. As such
#' the usual ways of finding duplicated rows in the raw data (e.g `unique` or
#' `distinct`) do not work.
#'
#' Given a row number, find any rows that are duplicates of that row.
#'
#' @param info_above_table tidyxl::xlsx_cells dataframe that contains the dropdown#'
#' @param dropdown_regex string. A regex pattern that should match only the
#' dropdown cell.
#' @returns message or warning
#'
#' info_above_table <- data.frame(
#'   row = c(1:3),
#'   col = rep(1,3),
#'   character = c("Title", "some other info", "England")
#' )
#'
#' check_dropdown("Eng", info_above_table) # found
#' check_dropdown("Wales", info_above_table) # not found
#' @export
check_dropdown <- function(dropdown_regex, info_above_table) {
  # Check if there are regex instructions
  if (!is.na(dropdown_regex)) {

    dropdown_cell_matches <- grepl(dropdown_regex, info_above_table$character)

    if (any(dropdown_cell_matches)) {
      message(paste(dropdown_regex, "found in information above table, so it is assumed that the dropdown contains the correct selection"))
    } else {
      stop(paste(dropdown_regex, "is the expected dropdown selection (as set in the data dictionary sheet_structure) and has not been found in the information above the table. Please check the correct dropdown has been selected in the interim data. If not, change the selection, save it and re-run pre-processing. If the correct selection is made already, contact a developer"))
    }
  }
}

