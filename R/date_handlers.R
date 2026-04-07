#' @title Build regex patterns for years between 1900 and 2099
#'
#' @description Create regular expression patterns that match calendar and
#' financial years from the 20th and 21st centuries. Will not match years that
#' are either immediately preceded or followed by a dash. Acceptable financial
#' year formats are:
#' YYYY-YY, YYYY/YY, YYYY_YY, YYYY to YY, YYYYtoYY,
#' YYYY-YYYY, YYY/YYYY, YYYY_YYYY, YYYY to YYYY, YYYYtoYYYY.
#' Acceptable calendar year format is: YYYY.
#'
#' @returns named vector with calendar and financial patterns Different year
#' types can be accessed by subsetting for 'calendar', 'financial',
#' 'long_form_financial', 'short_form_financial', and 'fy_separators'.
#'
#' @examples
#' \dontrun{
#' patterns <- make_year_patterns()
#' }
#' @export
make_year_patterns <- function () {

  century <- "19|20"

  not_preceded_by_number_or_letter <- "(?<![0-9A-Za-z])"

  two_numbers <- "\\d{2}"
  followed_by_two_numbers <- "(?=\\d{2})"
  followed_by_four_numbers <- "(?=\\d{4})"
  four_numbers <- paste0("(", century, "\\d{2})",  collapse = "")
  two_or_four_numbers <- paste0("((", century, "\\d{2})|", "\\d{2})",  collapse = "")

  not_followed_by_number_or_letter <- "(?![0-9A-Za-z])"
  not_followed_by_number_or_letter_except_q <- "(?![0-9A-PR-Za-pr-z])"
  not_followed_by_dash <- "(?![-])"

  dash <- "\\s{0,3}-\\s{0,3}"
  underscore <- "\\s{0,3}_\\s{0,3}"
  slash <- "\\s{0,3}\\/\\s{0,3}"
  to <- "\\s{0,3}to\\s{0,3}"
  to_with_underscores <- "\\s{0,3}_\\s{0,3}to\\s{0,3}_\\s{0,3}"
  to_with_dashes <- "\\s{0,3}-\\s{0,3}to\\s{0,3}-\\s{0,3}"


  fy_separators <- paste0(
    "((",
    dash, ")|(", underscore, ")|(", slash, ")|(",
    to, "|", to_with_underscores, ")|(", to_with_dashes,
    "))"
  )

  not_preceded_by_year_and_separator <- paste0(
    "(?<![0-9]{4}", dash, ")(?<![0-9]{4}", underscore, ")(?<![0-9]{4}", slash,
    ")(?<![0-9]{4}",  to, ")(?<![0-9]{4}",  to_with_underscores,
    ")(?<![0-9]{4}", to_with_dashes, ")"
  )

  yyyy <- "[1-2][0-9]{3}"
  yy <- "[0-9]{2}"

  non_number_or_string_end <- "([^0-9]|$)"

  not_followed_by_separator_and_yyyy <- paste0(
    "(?!",
    dash, yyyy, non_number_or_string_end, ")(?!",
    underscore, yyyy, non_number_or_string_end, ")(?!",
    slash, yyyy, non_number_or_string_end, ")(?!",
    to, yyyy, non_number_or_string_end, ")(?!",
    to_with_underscores, yyyy, non_number_or_string_end, ")(?!",
    to_with_dashes, yyyy, non_number_or_string_end,
    ")"
  )

  not_followed_by_separator_and_yy <- paste0(
    "(?!",
    dash, yy, non_number_or_string_end, ")(?!",
    underscore, yy, non_number_or_string_end, ")(?!",
    slash, yy, non_number_or_string_end, ")(?!",
    to, yy, non_number_or_string_end, ")(?!",
    to_with_underscores, yy, non_number_or_string_end, ")(?!",
    to_with_dashes, yy, non_number_or_string_end,
    ")"
  )

  or_space <- "|(\\s{0,3})"

  calendar_pattern <- paste(
    c(not_preceded_by_year_and_separator,
      not_preceded_by_number_or_letter,
      "(", century, ")",
      followed_by_two_numbers, two_numbers,
      not_followed_by_number_or_letter_except_q,
      not_followed_by_separator_and_yyyy,
      not_followed_by_separator_and_yy
    ),
    collapse = ""
  )

  financial_pattern <- paste(
    c(not_preceded_by_number_or_letter, "(", century, ")",
      followed_by_two_numbers, two_numbers, "(", fy_separators, or_space, ")",
      two_or_four_numbers,
      not_followed_by_number_or_letter, not_followed_by_dash),
    collapse = ""
  )

  long_form_financial <- paste(
    c(not_preceded_by_number_or_letter, "(", century, ")",
      followed_by_two_numbers, two_numbers, "(", fy_separators, or_space, ")",
      followed_by_four_numbers, four_numbers,
      not_followed_by_number_or_letter, not_followed_by_dash),
    collapse = ""
  )

  short_form_financial <- paste(
    c(not_preceded_by_number_or_letter, "(", century, ")",
      two_numbers, fy_separators, two_numbers,
      not_followed_by_number_or_letter, not_followed_by_dash),
    collapse = ""
  )

  year_patterns <- c(
    "calendar" = calendar_pattern,
    "financial" = financial_pattern,
    "long_form_financial" = long_form_financial,
    "short_form_financial" = short_form_financial,
    "fy_separators" = fy_separators
  )

  return(year_patterns)
}


#' @title Build regex patterns for quarters 1 through 4
#'
#' @description Create regular expression patterns that match quarters. NOTE:
#' The pattern has been designed for use where no other individual numbers
#' between 1 and 4 are expected e.g. the returned pattern would correctly
#' identify quarters in a vector containing year and quarter (e.g. 2024 Q1), but
#' not in a descriptive paragraph e.g. ('This is the 1st release of Q2 data'
#' would return both 1 and 2).
#'
#' @returns named vector. Currentluy only one pattern is created: 'single' which
#' can be used to identify single quarters. No pattern is yet returned for
#' quarter ranges such as Q1-Q3, but this could be a future addition (hence the
#' return of a named vector rather than a single value).
#'
#' @examples
#' \dontrun{
#' dat <- c("Q1", "1", ".1", "quarter 1", "q1",
#'            "1-3", "1 - 3", "q1-Q3", "q1 - Q3", "Q1to3", "Q1   to Q3", "13",
#'           "12-3", "Q1 note 2")
#' patterns <- make_quarter_patterns()['single']
#' str_extract_all(dat, patterns)
#' }
#' @export

make_quarter_patterns <- function() {

  not_preceded_by_a_number <- "(?<![0-9])"
  not_preceded_by_dash <- "(?<!(?i)\\-\\s{0,3})"
  not_preceded_by_dash_q <- "(?<!(?i)\\-\\s{0,3}q)\\s{0,3}"
  not_preceded_by_to <- "(?<!(?i)to\\s{0,3})"
  not_preceded_by_to_q <- "(?<!(?i)to\\s{0,3}q\\s{0,3})"
  quarters <- "([1-4])"
  not_followed_by_a_number <- "(?![0-9])"
  not_followed_by_a_dash <- "(?![\\-])"
  not_followed_by_a_spaced_dash <- "(?![\\s*][\\-][\\s*])"
  not_followed_by_to <- "(?!\\s*to\\s*)"

  single_quarter <- paste0(
    not_preceded_by_a_number,
    not_preceded_by_dash,
    not_preceded_by_dash_q,
    not_preceded_by_to,
    not_preceded_by_to_q,
    quarters,
    not_followed_by_a_number,
    not_followed_by_a_dash,
    not_followed_by_a_spaced_dash,
    not_followed_by_to
    )

  # future development could create a pattern for quarter ranges that would
  # match c("1-3", "1 - 3", "q1-Q3", "q1 - Q3", "Q1to3", "Q1   to Q3") but not
  # c("Q1", "1", ".1", "quarter 1", "q1", "12-13")

  return(c('single' = single_quarter))
}


#' @title Make regex patterns for months and quarters in Q1 of a calendar year
#'
#' @description Create regular expressions that can be used to identify the
#' months and quarters of a calendar year whose financial year start year is the
#' calendar year minus 1.
#'
#' @param period Must be 'all', 'quarter', or 'month'. Default is 'all'.
#' @param q1_jan_to_mar bool, default is TRUE. If FALSE, it is assumed that
#' Q4 is jan to mar, and Q1 is the start of the financial year.
#'
#' @return a regular expression pattern for the selected period(s)
#'
#' @examples
#' \dontrun{
#' make_calendar_q1_month_and_quarter_patterns(q1_jan_to_mar=FALSE)
#' }
#' @export
make_calendar_q1_month_and_quarter_patterns <- function(
    period = "all", q1_jan_to_mar = TRUE
) {

  if (!period %in% c("all", "quarter", "month")) {
    stop("period must be 'all', 'quarter' or 'month', not '", period, "'.")
  }

  if (is.na(q1_jan_to_mar) | q1_jan_to_mar=="NA") {q1_jan_to_mar <- TRUE}

  if (q1_jan_to_mar) {
    end_year_quarter_patterns <- "((?i)q1(?!-)|1(?!-))" # not followed by hyphen
  } else {
    end_year_quarter_patterns <- "((?<!-)(?i)q4|4)" # not preceded by hyphen
  }

  end_year_quarter_month_patterns <- "((?i)jan(?s).*\\w)"
  end_year_month_patterns <- "((?i)jan|feb|mar)"

  end_year_all_quarter_patterns <- paste0(end_year_quarter_patterns, "|",
                                          end_year_quarter_month_patterns)
  end_year_items_pattern <- paste0(end_year_quarter_patterns, "|",
                                   end_year_quarter_month_patterns, "|",
                                   end_year_month_patterns)
  if (period == "all") {
    return (end_year_items_pattern)
  } else if (period == "quarter") {
    return (end_year_all_quarter_patterns)
  } else {
    return (end_year_month_patterns)
  }

}


#' @title Get and compare year from above the data to a set year
#'
#' @description For data that are for a single year where there is no year
#' column. Year may be given in information above the table. In public sector
#' a year is also given in the file name. This function finds years in the
#' info above the data (dat) that are of the same type (calendar/financial)
#' as the year in the file name. It compares them to the year in the file name,
#' and if they are different, it selects one based on `use_filename_year`.
#'
#' If multiple are found and one of them matches the file name, the year from
#' the file name is used.
#' If multiple are found and none of them match the file name, and
#' use_filename_year is FALSE, the first is used and a warning is given.
#' If none are found, the file name year is used as default.
#'
#' @param title character string.
#' @param filename_year character string vector.
#' @param use_filename_year boolean. Default is FALSE. If FALSE and there is a
#' mismatch, the year found above the table will be returned.If TRUE and no year
#' that is found in the information above the table matches the filename year,
#' the filename year will be returned.  In pub sec this variable is specified
#' by use_year_from_filename_over_year_above_data.
#' @param prefer_sheet_year boolean. Default is FALSE. Only used when there is a
#' mismatch between the year(s) given at the top of the sheet and the years
#' found above a table. If either are NA, this variable is not used. If FALSE,
#' the year from above the table is used. If TRUE, the year from the top of the
#' sheet is used. In pub sec this variable is specified by
#' use_sheet_year_over_table_year.
#' @param suppress_warning boolean. Default is FALSE. If TRUE, warnings about
#' mismatching years between the filename and the information above the data
#' will be turned off. In pub sec this variable is specified by
#' suppress_year_above_table_warning. It is required because there are some
#' cases where we expect a mismatch and giving a warning would be confusing to
#' users.
#'
#' @returns list of 'year' and 'warn': 'year' is a character string, 'warn' is
#' boolean. If TRUE, a mismatch was found between the filename year and the year
#' found above the data. This is given as a boolean output rather than raising
#' a warning, because the warning is only relevant if the year output is added
#' to the data as a column. If 'warn' is TRUE a warning is raised if and when
#' 'year' is added as a column to the data.
#'
#' @examples
#' \dontrun{
#' filename <- "D:/data-2024_25.xlsx"
#' get_year_for_use_in_data(
#'     "2024-25", "2025-26", use_filename_year = FALSE
#'     )
#' get_year_for_use_in_data(
#'     "Title 2026", "2024_25", use_filename_year = TRUE
#'     )
#' }
get_year_for_use_in_data <- function(
    sheet_year, table_year, filename_year=NA, use_filename_year = FALSE,
    prefer_sheet_year = FALSE, suppress_warning = FALSE
){

  message(
    "Getting year for use in the data if year is not already in the data as ",
    "a column."
  )

  use_filename_year <- replace_na(use_filename_year, FALSE)
  prefer_sheet_year <- replace_na(prefer_sheet_year, FALSE)
  suppress_warning <- replace_na(suppress_warning, FALSE)

  filename_year_standardised <- standardise_year(filename_year)
  filename_year_type <- get_year_type(filename_year)

  if (is.na(filename_year_standardised)) {
    warning("No year found in the file name. Please contact a developer.")
  }

  sheet_standardised <- refine_metadata_year(sheet_year, filename_year_type)
  table_standardised <- refine_metadata_year(table_year, filename_year_type)

  year_above_data <- choose_between_sheet_and_table_years(
    sheet_standardised, table_standardised, prefer_sheet_year
  )

  if (all(is.na(year_above_data), is.na(filename_year_standardised))) {
    message(
      "No years found in the character string information above the data ",
      "or in the filename."
    )
    return(list("year" = NA, "warn" = FALSE))

  } else if (all(is.na(filename_year_standardised))) {
    message(
      "No years found in the filename. Returning year from above the data"
    )
    year_info <- choose_from_multiple_years(year_above_data)

    if (suppress_warning) { year_info[["warn"]] <- FALSE }
    return(year_info)

  } else if (all(is.na(year_above_data))) {
    message(
      "No years found in the character string information above the data. ",
      "Returning year from filename."
    )
    return(list("year" = filename_year_standardised, "warn" = FALSE))
  }

  all_years_match <- all(
    year_above_data == filename_year_standardised, na.rm = TRUE
  )
  any_years_match <- any(
    year_above_data == filename_year_standardised, na.rm = TRUE
  )

  if (all_years_match) {
    message(
      "Year from file name matches the year from info above the data: ",
      year_above_data, "."
    )
    year <- filename_year_standardised
    year_warning_if_used_in_column <- FALSE

  } else if (all(any_years_match, use_filename_year)) {

    message(
      "Not all years found above the data match the year in the filename, ",
      "but the year in the filename was amongst the years found, so is ",
      "assumed to be correct."
    )
    year <- filename_year_standardised
    year_warning_if_used_in_column <- TRUE

  } else if (all(any_years_match, !use_filename_year)) {

    message(
      "Not all years found above the data match the year in the filename. ",
      "The year in the filename was amongst the years found, but ",
      "use_filename_year is set to the default of FALSE so the first of the ",
      "other options has been returned. If no warning about this is given ",
      "when year column is created, or if the year column in the data is ",
      "correct no further action needs to be taken."
    )
    possibilities <- year_above_data[
      year_above_data != filename_year_standardised
    ]
    year <- possibilities[1]
    year_warning_if_used_in_column <- TRUE

  } else if (use_filename_year) {
    message(
      "No years found above the data match the filename year. The year in ",
      "the filename will be returned. If no warning about this is given ",
      "when year column is created, or if the year column in the data is ",
      "correct no further action needs to be taken. However if it is wrong a ",
      "developer may need to edit use_year_from_filename_over_year_above_data."
    )
    year <- filename_year_standardised
    year_warning_if_used_in_column <- TRUE

  } else if (length(year_above_data) == 1) {
    message(
      "Returning year found above the data. This does not match the ",
      "year in the file name. However, if no warning about this is given ",
      "when year column is created, or if the year column in the data is ",
      "correct no further action needs to be taken."
    )
    year <- year_above_data
    year_warning_if_used_in_column <- TRUE

  } else if (length(year_above_data) > 1) {
    message(
      "More than one year found in the information above the data. None of ",
      "these match the year in the file name."
    )
    year_info <- choose_from_multiple_years(year_above_data)
    year <- year_info[["year"]]
    year_warning_if_used_in_column <- year_info[["warn"]]

  } else {
    stop("No conditions have been met, please contact a developer for a fix.")
  }

  if (suppress_warning) {
    year_warning_if_used_in_column <- FALSE
  }

  return (list("year" = year, "warn" = year_warning_if_used_in_column))
}


#' @title Refine year taken from sheet or table metadata
#'
#' @description
#' A short description...
#'
#' @param year vector of character strings, each of which is a year in either
#' calendar or financial year format.
#' @param preferred characters string. Financial or calendar.
#'
refine_metadata_year <- function(year, preferred) {

  if (all(is.na(year))) {
    return(year)
  }

  message("Refining year(s) found in information above tables.")

  if (length(year) == 2) {
    years_compressed <- consecutive_years_to_financial(year)
  } else {
    years_compressed <- year
  }

  preferred_year <- get_year(
    years_compressed, prefer = preferred, type = "single"
  )

  if (length(preferred_year) == 0) {
    warning(
      "The year from above the data is not of the same type (", preferred,
      ") as determined by the year in the file name, it will therefore not be ",
      "considered for use in the year column."
    )
    return(NA)

  } else if (length(preferred_year) > 1) {
    message(
      "More than one year found: '",
      paste0(preferred_year, collapse = "', "), "'."
    )
  }

  standardised <- standardise_year(preferred_year)

  return(sort(standardised))
}


choose_between_sheet_and_table_years <- function(
    sheet_year, table_year, prefer_sheet_year = FALSE
){

  if (is.na(prefer_sheet_year)) { prefer_sheet_year <- FALSE }

  all_years_over_data_match <- all(sheet_year == table_year, na.rm = FALSE)
  all_years_over_data_match <- replace_na(all_years_over_data_match, FALSE)
  sheet_is_na <- all(is.na(sheet_year))
  table_is_na <- all(is.na(table_year))
  table_and_sheet_are_na <- all(sheet_is_na, table_is_na)

  if (table_and_sheet_are_na) {
    year <- NA
  } else if (any(all_years_over_data_match, table_is_na)) {
    year <- sheet_year
  } else if (sheet_is_na) {
    year <- table_year
  } else if (prefer_sheet_year) {
    message(
      "Year at the top of the sheet (", paste(sheet_year, collapse = ","),
      ") does not match year above the table (",
      paste(table_year, collapse = ","), "). Choosing sheet year (can be ",
      "changed using the prefer_sheet_year setting)."
      )
    year <- sheet_year
  } else {
    message(
      "Year at the top of the sheet (", paste(sheet_year, collapse = ","),
      ") does not match year above the table (",
      paste(table_year, collapse = ","), "). Choosing table year (can be ",
      "changed using the prefer_sheet_year setting)."
    )
    year <- table_year
  }

  return (year)
}


#' @title Select the first year if multiple unique years are given
#'
#' @description
#' Select the first year if multiple years are given and flag that a warning
#' is required. If only one year is provided, set the warning flag to FALSE.
#'
#' @details
#' This is used by the get_year_for_use_in_data function, which returns both a
#' year and a flag for if a warning of multiple options is needed. A flag is
#' created rather than just raising a warning because the user only needs to
#' be given the warning if the variable is actually used. For example, if year
#' is already a column in the data the returned variable will not be used and
#' it does not matter if multiple years are mentioned in the metadata.
#'
#' Whilst the output of this is currently very simple, this function could be
#' expanded in the future to include a setting that the user can control to say
#' whether to return e.g. the last element instead of the first. At the moment
#' doing so would be overcomplicating it as the issue has not yet arisen.
#'
#' @param years character vector.
#'
#' @returns named list with a character vector of length 1 for 'year' and a
#' corresponding boolean vector for 'warn'.
#'
#' @examples
#' \donotrun{
#' choose_from_multiple_years(c("2021-22", "2022-23"))
#' choose_from_multiple_years("2021-22")
#' choose_from_multiple_years(NA)
#' }
choose_from_multiple_years <- function(years) {

  years <- years[!is.na(years)]

  if (length(years) == 0) {
    return (NA)
  }

  if (length(unique(years)) == 1) {

    year <- unique(years)
    warn <- FALSE

  } else if (length(unique(years)) > 1) {
    message(
      "There is more than one year. The first will be used: ",
      unique(years)[1]
    )
    warn <- TRUE
  } else {
    warn <- FALSE
  }

  year_info <- list("year" = years[1], "warn" = warn)

  return(year_info)
}


#' @title Convert consecutive years to a financial year
#'
#' @description Given two years that are consecutive, combine them to form a
#' single financial year in the format YYYY-YY
#'
#' @param year character string or numeric vector of length 2.
#'
#' @returns character string of length 1 in the format YYYY-YY. If the length
#' of the year vector is not 2 an error is raised. If the years found are not
#' consecutive or not in the format YYYY a warning is given and the input
#' year is returned.
#'
#' @examples
#' \dontrun{
#' year <- consecutive_years_to_financial(c("2021", "2022"))
#' year <- consecutive_years_to_financial(c("2020", "2022"))
#' year <- consecutive_years_to_financial(c("2020-21", "2021-22"))
#' }
#' @export
consecutive_years_to_financial <- function (year) {

  if (length(year) < 2) {
    stop(
      "Only one year found when trying to convert consecutive years ",
      "to financial. Check that the year column contains the expected values. ",
      "If not, contact a developer."
    )
  }

  if (length(year) > 2) {
    stop(
      "More than two years found when trying to turn multiple years into ",
      "a financial year. Conversion to financial year aborted. Check that ",
      "the year column contains the expected values. If not, contact a ",
      "developer."
    )
  }

  message("Converting consecutive years to financial year.")

  if (all(nchar(year)==4)) {
    year <- as.numeric(year)

    if (year[2] - year[1] == 1) {
      year1 <- as.character(year[1])
      year2 <- as.character(year[2])

      year <- paste0(year1, "-", stringr::str_sub(year2, -2))
      message(
        "Consecutive years: ", year1, " and ", year2, " found. They have ",
        "been compressed to ", year, ". If this is incorrect, please ",
        "contact a developer."
      )

    } else {

      year <- as.character(year)
      message(
        "Multiple non-consecutive years found."
      )
    }
  } else {
    message(
      "Years found are not calendar years (YYYY), but there is more than one. "
    )
  }

  return(year)
}


#' @title Get first financial or calendar year from each entry in a given column
#'
#' @description Get the first year from a specified column or vector and put it
#' in a new column called 'year' (can be changed using new_col arg).
#'
#' If a string contains more than one year (of the same type), only the first is
#' put in 'year'.
#'
#' If `prefer` and `type` are not specified, financial years will be returned
#' where available, but where no financial year is found, calendar year will be
#' returned.
#'
#' If `type` = 'single' year will be restricted to either calendar or financial
#' years (whichever is set by prefer).
#'
#' @param dat dataframe or vector. If a dataframe, it must contain a column with
#' the name from `from`.
#' @param from character string or NA. The name of the column containing year
#' information if dat is a dataframe, otherwise NA.
#' @param new_col character string. The name of the column the new information
#' will be stored in. Default is 'year'
#' @param prefer character string. Either 'financial' (default), or 'calendar'.
#' If type is 'both' and both a calendar year and a financial year are found in
#' a single entry, this determines which is chosen. If type is 'single', prefer
#' determines whether year represents calendar year or financial year. If only
#' years of a different type to prefer are provided, and type is single, no
#' years are returned.
#' @param type character string. Either 'both' (default), or 'single'. If
#' 'single', the new year column will contain only calendar years or only
#' financial years (depending on prefer). If 'both', the year column can
#' contain both types.
#'
#' @returns dat with a new column called yearif dat is a dataframe, or a vector
#' of years if input is a vector.
#'
#' @examples
#' \dontrun{
#' dat_fy <- data.frame(
#'   "year_and_vintage" = c("2019-20, 2020-21 budget", "this is 2020",
#'                          "2020-21 provisional", "2020 2021")
#'   )
#' dat_calendar_only <- data.frame(
#'   "year_and_vintage" = c("2019 budget", "this is 2020",
#'                          "2020 provisional", "2020 2021")
#'   )
#' dat_mixed <- data.frame(
#'   "year_and_vintage" = c("2019 and 2020-21 ", "this is 2020-21",
#'                          "2020 provisional", "2020 2021")
#'   )
#' get_year(dat_fy, "year_and_vintage")
#' get_year(dat_calendar_only, "year_and_vintage")
#' get_year(dat_mixed, "year_and_vintage")
#' get_year(c("a 2020", "c 1985"))
#' }
#' @export
get_year <- function (
    dat, from = NA, new_col = "year", prefer = "financial", type = "both"
    ) {

  if (is.na(prefer)) {
    prefer <- "financial"
  }

  if (! prefer %in% c("financial", "calendar")) {
    stop(
      "prefer must be one of 'financial', or 'calendar'. It is: '",
      prefer, "'."
    )
  }

  if (! type %in% c("single", "both")) {
    stop(
      "type must be one of 'single', or 'both'. It is: '",
      type, "'."
    )
  }

  if (!is.data.frame(dat) & !is_tibble(dat) & !is.vector(dat)) {
    stop("dat must be a dataframe, tibble, or vector.")
  }

  if (!is.vector(dat) & is.na(from)){
    stop("from must be supplied unless dat is a vector.")
  }

  if (is.vector(dat)) {
    dataframe <- data.frame(vector = dat)
    from <- 'vector'
  } else {
    dataframe <- dat
  }

  patterns <- make_year_patterns()

  all <- dataframe %>%
    mutate(
      calendar = str_extract(!!sym(from), patterns["calendar"]),
      financial = str_extract(!!sym(from), patterns["financial"])
    ) %>%
    mutate(
      financial_preferred = ifelse(is.na(financial), calendar, financial),
      calendar_preferred = ifelse(is.na(calendar), financial, calendar)
      )

  selected <- all %>%
    mutate(
      !!sym(new_col) := case_when(
        prefer == "financial" & type == "single" ~ financial,
        prefer == "calendar" & type == "single" ~ calendar,
        prefer == "financial" & type == "both" ~ financial_preferred,
        prefer == "calendar" & type == "both" ~ calendar_preferred
      )
    ) %>%
    select(-c(calendar, financial, financial_preferred, calendar_preferred))

  any_questionable_years <- check_for_multiple_years(
    dataframe, from, prefer, type
    )

  if (any_questionable_years) {
    warning(
      "Multiple different years were found for some rows. Please check that ",
      "column '", new_col, "' contains the expected values."
      )
  }

  if (is.vector(dat)) {
    as_vector <- pull(selected, !!sym(new_col))
    selected <- as_vector[!is.na(as_vector)]
  }

  return(selected)
}

#' @title Check if there are multiple different years in any one entry.
#'
#' @description Check if there are multiple different years in any  single entry
#' of specified column. Used internally by get_year.
#'
#' If `type` is not specified, both financial and calendar years will be
#' checked - so if an entry contains both a financial year and a calendar year,
#' these will be classed as different years and TRUE will be returned.
#'
#' If `type` = 'single' only multiple values of the preferred year (calendar or
#' financial) will be looked for, unless no values of that type are found, in
#' which case multiple values of the non-preferred year will be checked for.
#'
#' @param dat dataframe that contains a column with the name from `from`.
#' @param from character string. The name of the column containing year
#' information.
#' @param prefer character string. Either 'financial' (default), or 'calendar'.
#' If type is 'both' and both a calendar year and a financial year are found in
#' a single entry, this determines which is checked If type is 'single', prefer
#' determines whether calendar years or financial years are checked for multiple
#' different values.
#' @param type character string. Either 'both' (default), or 'single'. If
#' 'single', a value with both a calendar year and a financial year will not
#' be classed as having multiple different values.
#'
#' @returns boolean. TRUE if any entry contains more than one unique year.
#'
#' @examples
#' \dontrun{
#' check_for_multiple_years(data.frame(char = c("2024", "2025 2026")), "char")
#' check_for_multiple_years(data.frame(char = c("2024", "2025")), "char")
#' check_for_multiple_years(
#'     data.frame(char = c("2021 2022-23 2023-24")), "char",
#'     prefer = "financial",
#'     type = "single"
#'     )
#' check_for_multiple_years(
#'     data.frame(char = c("2021 2022-23 2023-24")), "char",
#'     prefer = "calendar",
#'     type = "single"
#'     )
#' check_for_multiple_years(
#'     data.frame(char = c("2021 2022 2023-24")), "char",
#'     prefer = "calendar",
#'     type = "single"
#'     )
#' }
check_for_multiple_years <- function(
    dat, from, prefer = "financial", type = "both"
    ) {

  patterns <- make_year_patterns()

  counts <- dat %>%
    mutate(calendar = str_extract_all(!!sym(from), patterns["calendar"]),
           financial = str_extract_all(!!sym(from), patterns["financial"])) %>%
    rowwise() %>%
    mutate(unique_calendar = length(unique(calendar[!is.na(calendar)])),
           unique_financial = length(unique(financial[!is.na(financial)])))

  check_of_multiples <- counts %>%
    mutate(
      multiples = case_when(
        prefer == "financial" & type == "single" ~ unique_financial > 1,
        prefer == "calendar" & type == "single" ~ unique_calendar > 1,
        type == "both" ~
          sum(unique_financial, unique_calendar) > 1
      )
    ) %>%
    distinct(multiples)

  any_multiple_years <- sum(check_of_multiples) > 0

  return(any_multiple_years)

}


#' @title Identify all years from a vector of character strings
#'
#' @description Get all years (from the 20th and 21st centuries).
#' If no years are found NA is returned. If one or more years are found, unique
#' years are returned.
#'
#' @param dat numeric or character string. Accepts vector, matrices and
#' dataframe data.
#' @param type character string. Either 'both' (default), 'financial', or
#' 'calendar'.
#' @returns character string vector, or NA. Returns a vector of all strings
#' that matched the pattern for the year_type. If no years are identified in
#' any of the input strings, NA is returned.
#'
#' @seealso [get_year()] for getting the first year from each entry of a
#' dataframe column.
#'
#' @examples
#' \dontrun{
#' test_dat <- c("a date: 1998-1999", "1995 6", "1993 end", "a1995", "1995a", "a
#' date:1995, another date: 2012", "not a date: 475", "also not a date: 1234",
#' "abc 2021-22",  "34200943", "190932")
#' extract_all_years(test_dat)
#'
#' test_dat2 <- data.frame(year_unedited = c("2021q1", "2022"))
#' test_dat2 %>% mutate(year = extract_all_years(year_unedited, 'calendar'))
#' }
#' @export
extract_all_years <- function(dat, type = "both") {

  dat <- unlist(dat)

  patterns <- make_year_patterns()

  calendar_years <- unlist(str_extract_all(dat, patterns['calendar']))
  financial_years <- unlist(str_extract_all(dat, patterns['financial']))

  if (type == "financial"){
    years <- financial_years
  } else if (type == "calendar") {
    years <- calendar_years
  } else if (type == "both") {
    years <- c(financial_years, calendar_years)
  }

  if (length(years) == 0) {
    unique_years <- NA
  } else {
    unique_years <- unique(years[!is.na(years)])
  }

  return(unique_years)
}


#' @title Get the year type for each row of a dataframe, or for a single year
#'
#' @description Get an unambiguous year type. If more than one year type is
#' found NA is returned.
#'
#' @param dat character string, integer, or dataframe. If character string or
#' integer, one calendar or financial year. If dataframe, a dataframe with a
#' column named the same as `column`.
#' @param column character string or NA. defaults to NA as this arg is only used
#' when dat is a dataframe or tibble.
#'
#' @returns same type as dat, stating the year type. If dat is a dataframe a new
#' column is added called 'year_type' containing 'financial', 'calendar' or NA.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(year = c("2021", "2021-22", "2021 2021-22"))
#' get_year_type(dat, "year")
#'
#' get_year_type("2021-22")
#' get_year_type("2021")
#' get_year_type("202123")
#'
#' }
#' @export
get_year_type <- function(dat, column = NA) {

  if (length(dat) == 0) {
    return(NA)
  }

  if (is.vector(dat)) {
    dataframe <- data.frame(temp_column = dat)
    column <- "temp_column"
  } else {
    dataframe <- dat
  }

  patterns <- make_year_patterns()

  df_output <- dataframe %>%
    mutate(temp_calendar = str_detect(!!sym(column), patterns["calendar"]),
           temp_financial = str_detect(!!sym(column), patterns["financial"]),
           type = case_when(
             temp_calendar == TRUE & temp_financial == FALSE ~"calendar",
             temp_calendar == FALSE & temp_financial == TRUE ~"financial",
             TRUE ~ NA)) %>%
    select(-c(temp_calendar, temp_financial))

  if (is.vector(dat)) {
    type <- pull(df_output, type)
    return(type)
  } else {
    return (df_output)
  }

}


#' @title Get the years before or after the current year
#'
#' @description Get the string for the year x years before or after the given
#' year. Only a single year (calendar or financial must be given). If multiple
#' valid years (or no valid years) are provided, NA is returned with a warning.
#'
#' @param year character string or integer. Only a single year must be given,
#' but this can be a financial year (YYYY-YY or YYYY-YYYY) or calendar (YYYY).
#' @param x integer. The number of years before (negative) ot after (positive)
#' year that you want to be returned.
#' @param type character string. Must be "financial" or "calendar"
#'
#' @returns calendar string. Theyear that is x years before or after the
#' provided year.
#'
#' @examples
#' \dontrun{
#' get_bookend_year("2021-22", -1, "financial")
#' get_bookend_year("2021-22", 1, "financial")
#' get_bookend_year("2021", -1, "calendar")
#' }
#' @export
get_bookend_year <- function(year, x, type) {

  if (! type %in% c("financial", "calendar")) {
    stop("type must be financial or calendar")
  }

  years <- extract_all_years(year, type)

  if (length(years) > 1) {
    warning(
      "More than one ", type, " year provided. Bookend year not calculated."
      )
    return (NA)
  } else if (is.na(years)) {
    warning(
      "No ", type, " year provided. Bookend year not calculated."
    )
    return (NA)
  }

  if (x < 0) {
    message("Creating previous year string")
  } else if (x > 0) {
    message("Creating future year string")
  } else {
    return (year)
  }

  start <- as.numeric(substr(year, 1, 4)) + x

  if (type == "financial") {
    end <- paste0("-", as.numeric(substr(year, nchar(year)-1, nchar(year))) + x)
  } else {
    end <- ""
  }
  bookend_year <- paste0(as.character(start), as.character(end))

  return(bookend_year)

}


#' @title split a date formatted column into columns for day, month, and year
#'
#' @description Where a column can be unambiguously converted to a POSIXt object,
#' split the column out into day, month, and year columns. If it cannot be
#' converted, return the original data and an error message.
#'
#' The information in neonscience.org/resources/learning-hub/tutorials/
#' dc-convert-date-time-posix-r was used to create code that extracts year and
#' day.
#'
#' @param dat dataframe that contains the column name given by column.
#' @param column character string. The exact name of the column containing
#' the date. In pub_sec this variable is specified by POSIX_column.
#'
#' @returns datafram. dat with columns day, month, and year added, and
#' column removed.
#'
#' @examples
#' \dontrun{
#'
#'  dat <- data.frame(
#'    period = c("2017-01-08", "2017-08-14"),
#'    value = c(1:2)
#'  )
#'
#'  split_date_to_columns(dat, "period")
#' }
#' @export
split_date_to_columns <- function(dat, column) {

  if (is.na(column)) {
    return(dat)
  } else {
    message("Splitting POSIX column into year, month, and day columns.")
  }

  if (all(is.na(dat[column]))) {
    warning(
      "'", column, "' is in the settings as 'POSIX_column', but it ",
      "is not read as a date by xlsx_cells. Please check that year is as ",
      "expected in the preprocessed data. If not, please contact a developer."
    )
    return(dat)
  }

  if(str_to_lower(column) %in% str_to_lower(names(dat)) == FALSE) {
    stop(
      "column ('", column, "') in dev_config sheet_structure is ",
      "not a column in the data. Please contact a developer."
    )
  }

  # check that the column in question can be converted to POSIXlt
  tryCatch ({

    # for some reason, dates in the format dd/mm/yyyy
    # are converted wrongly:
    if (any(grepl("/", dat[[column]]))) {
      stop(
        "Error in split_date_to_columns: Please contact a developer. ",
        "The column given in sheet_structure for column is not being ",
        "read into R as an unambiguous date."
      )
    }

    date_split <- dat %>%
      mutate(day = unclass(as.POSIXlt(!!sym(column)))$mday,
             month = months(as.POSIXlt(!!sym(column)), abbreviate = TRUE),
             year = unclass(as.POSIXlt(!!sym(column)))$year + 1900)



    return(date_split)
  },
  warning = function(w){
    message(w)
    return(dat)
  },
  error = function(e) {
    stop(
      "Error in split_date_to_columns: Please contact a developer. ",
      "The column given in sheet_structure for column is not being ",
      "read into R as an unambiguous date."
    )
  }
  )


}


#' @title change dates to character strings
#'
#' @description Treat date strings as character string in xlsx_cells data.
#' If the data_type is 'date' and the date column is not NA, update
#' the data_type column to character and cut information from the date
#' column into the character column.
#'
#' @param dat dataframe imported using tidyxl::xlsx_cells()
#'
#' @returns dataframe dat with date strings set to be treated as character
#' strings.
#'
#' @examples
#' \dontrun{
#' convert_date_to_char(source_df)
#' }
#' @export
convert_date_to_char <- function(dat) {

  if (all(is.na(dat$date))) {
    return (dat)
  }
  newdat <- dat %>%
    mutate(
      character = case_when(
        data_type == "date" ~ as.character(date),
        .default = as.character(character)
      )
    ) %>%
    mutate(
      date = NA,
      data_type = case_when(
        data_type == "date" ~ "character",
        .default = as.character(data_type)
      )
    )

  return(newdat)
}
