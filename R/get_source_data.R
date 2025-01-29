#' @title get_source_data
#' @description  get_source_data
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details
# Get the current date and time, start f logfile with the datetime as part of the name
#' @examples get_source_data()
get_source_data <- function(sheets, tab_regex_front_page){

  # match sheet names to regex to find correct tab for preprocessing
  matching_sheets <- tidysheet::match_sheet_to_regex(sheets, tab_regex)
  matching_sheets_front_page <- suppressWarnings(tidysheet::match_sheet_to_regex(sheets, tab_regex_front_page))
  if (length(matching_sheets_front_page)==0) {
    warning("No front page information was used (this may be intentional)")
  }

  # checker for if there are multiple sheets matching to the regex pattern
  if (length(matching_sheets) > 0) {
    tabname <- tidysheet::sheet_matcher(matching_sheets)
  } else {
    if (supplier == "northern_ireland" & dataset == "council_tax_nndr") {

      stop(paste0("Budget and final data are in diffferent tabs of the same spreadsheet. The tab regex pattern: ", tab_regex, " is not relevant for this year. Available sheets are called: ", list(sheets)))
    } else {
      stop(paste0("No matching sheets found for the tabname_regex: '", tab_regex, "' in the dev_config. Available sheets are called: ", list(sheets)))
    }
  }

  if (length(matching_sheets_front_page) > 0) {
    tabname_front_page <- tidysheet::sheet_matcher(matching_sheets_front_page)
  } else {
    tabname_front_page <- NULL
  }

  # check if tabnames are NULL
  if (is.null(tabname)) {
    stop("No regex pattern given. Exiting the system.\n")
  }

  # Read in excel with the regex match tab names
  source_data <- tidyxl::xlsx_cells(input_filepath, sheets = tabname)
  return(source_data)
}
