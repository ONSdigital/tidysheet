#' @title get_vintage_from
#' @description Gets a vintage as a string match
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details
#' Additional details...
#' 
#' get_vintage_from checks items against a string to find a match 
#' for the vintage string possibilities
#' @param vintage_check_item a cell or filepath that could contain the vintage information
#' @param vintage_patterns a list of allowable vintage strings
#' @param sheet_vintage default NA, but it set if found.
#' @return vintage_result the matched result or 'NA' if none found.
#' @examples
#' example code sets the sheet vintage to 'forecast', finding it in the patterns
#'   sheet_vintage <- "NA"
#'   vintage_check_item <- "2023/24 Forecast"
#'   expected_result <- "forecast"
#'   sheet_vintage <- get_vintage_from(vintage_check_item, vintage_patterns, sheet_vintage)
#' 
#' Caes of multiple vintages:
#' sheet_vintage <- NA
#' vintage_check_item <- "budget and provisional data"
#' expected_result <- "budget, provisional"
#' actual_result <- suppressWarnings(get_vintage_from(vintage_check_item, sheet_vintage))
#' 
#' 
#' @export
get_vintage_from <- function(vintage_check_item, sheet_vintage=NA) {
  vintage_patterns <- "budget|forecast|provisional|final"
  if ((is.na(sheet_vintage) | sheet_vintage == "NA")) {
    sheet_vintage <- paste0(
      unique(
        extract_matches(str_to_lower(vintage_check_item), vintage_patterns)[[1]],
        collapse = "|"
      )
    )
  }
  return(sheet_vintage)
}

