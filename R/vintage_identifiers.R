#' @title get vintage (budget, provisional, or final)
#'
#' @description Get a single vintage (i.e. budget, provisional, or final) for
#' the dataset. Vintage can be found in different places depending on the
#' supplier and dataset. These options have an order of preference. The source
#' of vintage considered to be the most reliable is returned.
#'
#' Note that 'budget' is used to describe both 'forecast' and 'estimated return'
#' data.
#'
#' In order of priority:
#' - as specified in the settings.
#' - release number.
#' - subtable title.
#' - table title.
#' - other information found in information above the data (table_info takes
#' priority over sheet_info).
#'
#' We would usually expect all found vintages to be the same, so give a warning
#' if a mismatch is found (subtables may be different in which case the warning
#' could just be ignored).
#'
#' @param single_vintage character string. Vintage as specified in the settings.
#' In pub sec this variable is specified by single_vintage.
#' @param release_number integer between 1 and 9 or NA.
#' @param sheet_title character string. The title at the top of the sheet.
#' @param sheet_info dataframe imported using tidyxl::xlsx_cells()- this should
#' be a subset of the dataset that just includes information given above the
#' data in the sheet.
#' @param table_title character string. The title above the table.
#' @param table_info dataframe imported using tidyxl::xlsx_cells() Same as
#' sheet_info but for an individual subtable.
#'
#' @returns character string. The highest priority vintage. Possible values are
#' "final", "budget", "provisional", or NA if no vintage can be determined. If
#' multiple different vintages are found a warning is given but the highest
#' priority vintage is still returned.
#'
#' @examples
#' \dontrun{
#' # the following shows what happens when the sources of vintage disagree
#' # with each other
#' get_vintage(
#'   single_vintage = "final",
#'   release_number = 2,
#'   sheet_title = "provisional and final data",
#'   table_title = "final data",
#'   sheet_info = data.frame(
#'       character = "This sheet contains final and provisional data", row = 1
#'       ),
#'   table_info = data.frame(character = "provisional data", row = 1),
#' )
#' }
#' @export
get_vintage <- function(
    single_vintage, release_number, sheet_title, sheet_info, table_title = NA,
    table_info = NA
    ) {

  message("Determining vintage from metadata and settings.")

  if (!is.na(single_vintage)) {
    if (! single_vintage %in% c("budget", "provisional", "final")) {
      stop(
        "single_vintage setting must be one of 'budget', 'provisional' or ",
        "final. It is '", single_vintage, "'. Exiting from determining vintage."
           )
    }
  }

  release_number_vintage <- get_vintage_from_number(release_number)

  sheet_vintage <- get_vintages_from_table(sheet_info)
  table_vintage <- get_vintages_from_table(table_info)

  sheet_title_vintage <- get_vintage_from_string(sheet_title)
  table_title_vintage <- get_vintage_from_string(table_title)

  vintages <- unique(
    c(single_vintage,
      release_number_vintage,
      table_title_vintage,
      sheet_title_vintage,
      table_vintage,
      sheet_vintage)
    )
  valid_vintages <- vintages[!is.na(vintages)]

  if (length(valid_vintages) > 1) {
    warning(
      "Multiple vintages found for dataset in different locations: '",
      paste0(valid_vintages, collapse = "', '"), "'."
      )
  }

  if (!is.na(single_vintage)) {

    message("Vintage taken from settings: '", single_vintage, "'.")
    return(single_vintage)

  } else if (!is.na(release_number_vintage)) {

    message(
      "Vintage taken from release number: '", release_number_vintage, "'."
      )
    return(release_number_vintage)

  } else if (!is.na(table_title_vintage)) {

    message("Vintage taken from table title: '", table_title_vintage, "'.")
    return(table_title_vintage)

  } else if (!is.na(sheet_title_vintage)) {

    message("Vintage taken from table title: '", sheet_title_vintage, "'.")
    return(sheet_title_vintage)

  } else if (!is.na(table_vintage)) {

    message(
      "Vintage taken from information above the table: '", table_vintage,
      "'."
    )
    return(table_vintage)

  } else if (!is.na(sheet_vintage)) {

    message(
      "Vintage taken from information above the table: '", sheet_vintage,
      "'."
    )
    return(sheet_vintage)

  } else {

    message("vintage not found in dataset metadata or in settings.")
    return(NA)
  }

}


#' @title Get vintage information from a small table imported using xlsx_cells
#'
#' @description Get vintage information from a small table imported using
#' tidyxl::xlsx_cells(). Designed to handle the part of a sheet that sits
#' outside the main data i.e. the information above or below the data. As
#' such, if there are more than 2 unique Excel rows in the data NA and a
#' warning are returned.
#'
#' @param dat dataframe imported using tidyxl::xlsx_cells. Must not contain
#' more than 20 unique row ids.
#'
#' @returns character string. Possible values are "final", "budget",
#' "provisional", or NA if no vintage can be determined.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'     row = rep(1:2, each = 2),
#'     col = rep(1:2, times = 2),
#'     character = c("Title of the data", NA, "budget data", NA)
#'     )
#' get_vintages_from_table(dat)
#' }
#' @export
get_vintages_from_table <- function(dat) {

  if (is.null(dat)) {
    return(NA)
  }

  # to avoid turning a huge df into a single string check the size of dat.
  # dat should just be the info above the header.
  if (length(unique(dat$row)) > 20) {
    warning(
      "dat is larger than expected so will be ignored for vintage. ",
      "Try passing a smaller subset of data."
    )
    return(NA)
  }

  dat_string <- paste(dat$character, collapse = ", ")
  vintage <- get_vintage_from_string(dat_string)

  return(vintage)

}


#' @title Get all valid vintages from a string
#'
#' @description
#' Get vintages (i.e. budget, provisional, or final) from a string.
#' Vintage can be found in different places depending on the supplier and
#' dataset. This function can be used to get vintage from any string variables.
#'
#' 'budget' is used to describe both 'forecast' and 'estimated return' data.
#'
#' @param item string that could contain the vintage information
#'
#' @returns vector of strings. The matched result or 'NA' if none or multiple
#' different vintages are found. If multiple vintages are found a warning is
#' given stating what they are.
#'
#' @examples
#' \dontrun{
#' sheet_vintage <- get_vintage_from_string("some forecast and final data")
#'}
#' @export
get_vintage_from_string <- function(item){

  if(is.na(item)) {
    return(NA)
  }

  budget_pattern <- "\\bbudget\\b"
  other_budget_patterns <- "\\bforecast\\b|\\bestimated\\s*return\\b"
  provisional_patterns <- "\\bprovisional\\b"
  final_patterns <- "\\bfinal\\b"
  vintage_patterns <- paste0(c(
    budget_pattern, other_budget_patterns, provisional_patterns, final_patterns
    ), collapse = "|"
  )

  matches <- extract_matches(str_to_lower(item), vintage_patterns)
  vintages <- unique(matches)[[1]]
  valid_vintages <- vintages[!is.na(vintages)]

  if (any(valid_vintages=="forecast"| grepl("estimated", valid_vintages))) {
    forecast_locs <- which(valid_vintages=="forecast")
    estimate_locs <- which(grepl("estimated", valid_vintages))
    locs <- c(forecast_locs, estimate_locs)
    standardised_vintages <- unique(replace(valid_vintages, locs, "budget"))
  } else {
    standardised_vintages <- unique(valid_vintages)
  }

  if (length(standardised_vintages) == 0) {
    return(NA)

  } else if (length(standardised_vintages) > 1) {
    warning(
      "More than one vintage found in string: '",
      paste0(standardised_vintages, collapse = "', '"),
      "'. A different method will therefore be used to determine vintage"
    )
    return(NA)
  }  else {
    return(standardised_vintages)
  }
}


#' @title Get the vintage from the release number
#'
#' @description Get the vintage from release numbers. If number is 1, vintage
#' is set to "provisional", if higher than 1 it is set to "final".
#' This is based on requirements for MHCLG (DLUHC) revenue expenditure
#' provisional and final data where the 1st release is always 1, and any number
#' higher than that is final until another release is made at which point those
#' lower number releases are relegated to a provisional status.
#'
#' @param number integer between 1 and 9.
#'
#' @returns character string. If number is 1, "provisional" is returned,
#' if higher than 1 "final" is returned.
#'
#' @examples
#' \dontrun{
#' get_latest_release_number(1)
#' }
#' @export
get_vintage_from_number <- function(number) {

  if(length(number) > 1) {
    stop("Number must be of length 1.")
  }

  if(is.na(number)) {
    return(NA)
  }

  if (!is.numeric(number)) {
    stop("Number must be numeric.")
  }

  if (!is.numeric(number) & !is.integer(number)) {
    stop("Number must be numeric or integer.")
  }

  if(number %% 1 != 0) {
    stop("Number must not have decimal places.")
  }

  if (number == 1) {vintage <- "provisional" } else { vintage <- "final"}

  return(vintage)
}
