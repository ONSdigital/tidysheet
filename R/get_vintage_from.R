#' @title get_vintage_from
#' @description Gets a vintage as a string match
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details
#' If sheet_vintage is NA, get_vintage_from checks items against a regex string
#' to find a match for the vintage string possibilities (budget, forecast,
#' provisional, final and estimated return).
#'
#' Forecast and estimated return are other words for budget, so for example where forecast is picked up, return
#' 'budget' for consistency.
#'
#' @param vintage_check_item string that could contain the vintage information
#' @param sheet_vintage string. Default is NA.
#'
#' @return vector of strings. The matched result or 'NA' if none found.
#'
#' @examples
#' example code sets the sheet vintage to 'forecast', finding it in the patterns
#'   sheet_vintage <- "NA"
#'   vintage_check_item <- "forecast final item"
#'   expected_result <- "forecast"
#'   sheet_vintage <- get_vintage_from(vintage_check_item, sheet_vintage)
#'
#' @export
get_vintage_from <- function(vintage_check_item, sheet_vintage=NA){
  vintage_patterns <- "budget|forecast|provisional|final|estimated\\s+return"
  if ((is.na(sheet_vintage) | sheet_vintage == "NA")) {
    matches <-  extract_matches(str_to_lower(vintage_check_item), vintage_patterns)
    sheet_vintage <- unique(matches)
  }

  if ((length(unique(sheet_vintage[[1]]))) > 1 ) {
    sheet_vintage <- sheet_vintage[!sheet_vintage %in% "NA"]
  }

  # We want to make sheet_vintage "NA" if there is more than one non-NA vintage
  # so that other code can set it
  if(all(is.na(sheet_vintage[[1]]))) {
    sheet_vintage[[1]] <- NA
  } else {
    sheet_vintage[[1]] <- unique(sheet_vintage[[1]][!is.na(sheet_vintage[[1]])])
  }

  if (length(sheet_vintage[[1]]) > 1) {
    warning("More than one vintage found: vintage will be set by another method")
    sheet_vintage <- "NA"
  } else{
    sheet_vintage <- paste(sheet_vintage, collapse = "|")
  }

  if (sheet_vintage=="forecast") {sheet_vintage <- "budget"}
  if (grepl("estimated", sheet_vintage)) {sheet_vintage <- "budget"}

  return(sheet_vintage)
}

