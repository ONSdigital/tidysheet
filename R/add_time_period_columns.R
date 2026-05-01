#' @title Add quarter, fy_start, and year columns.
#'
#' @description
#' Get quarter from a column containing more than just quarter, get fy_start
#' from the available information on year, and select the correct information
#' for the year column (see documentation for process_year_column).
#'
#' @details
#' Some functions that deal with time periods (split_date_to_columns,
#' and split_year_and_vintage) are done prior to this in fill_missing_info.
#' For more details see the documentation for add_quarter_column, 
#' get_fy_start, process_year_column, and fill_missing_fy_start.
#'
#' @param dat dataframe.
#' @param quarter_from_col_pattern character string. A regular expression that
#' matches only the column containing quarter information. Only required
#' if single quarters (not quarter ranges) are not already in their own column.
#' @param quarter_col_name character string. The name of the column that quarter
#' will be put in.
#' @param year_from_pattern character string. Regular expression that matches
#' the name of the column containing year. This column may contain extra
#' information alongside year, such as quarter. Required unless fy_from_fy_end
#' is provided.
#' @param fy_from_fy_end boolean. Default is FALSE. TRUE if the financial
#' year is to be calculated from the financial year end.fy_end_pattern must be
#' supplied if TRUE.
#' @param fy_start_from_fy_end boolean. Default is FALSE. TRUE if the financial
#' year start is to be calculated from the financial year end.
#' @param fy_end_pattern characters string. Regular expression that matches the
#' name of the column containing the financial year end. Default is NA
#' @param calendar_year_to_fy_start boolean. TRUE if calendar year is to be
#' converted to financial year start. Default is FALSE.
#' @param q1_is_jan_to_mar boolean. TRUE if Jan to Mar is Q1. FALSE if Apr to
#' Jun is Q1. Default is FALSE.
#' @param quarter_col_pattern character string. Regular expression used to
#' identify the column that contains only the quarter (e.g. 'Q1' or '1'). Not
#' required if quarter_from_pattern is supplied.
#' @param month_col_pattern character string. Regular expression used to
#' identify the column that contains only the month.
#' @param year_col_pattern character string. The name of the year column
#' provided in settings. If this is NA, the year column is identified
#' automatically by get_year_column_names().
#' @param single_year_of_data boolean or NA. If TRUE create a column called year
#' and populate it with single_year. Unless single_overrides_all is TRUE, this
#' will only be used as a last option.
#' @param year_for_column list of length 2: 'year' is a character string
#' containing a valid year; 'warn' is boolean. TRUE if a warning needs to be
#' raised because of inconsistent years from different locations. This is only
#' used if use_single_year is TRUE. If so, the 'year' column is populated with
#' this year value. Must be specified if use_single_year is TRUE. In pub sec
#' this variable is taken either from the file name (which gets its year or from
#' information above the data.
#' @param single_year_overrides_all boolean or NA. If TRUE, and
#' single_year_of_data is TRUE, single_year overrides any columns found with
#' year in the name. If FALSE or NA, other options for filling the year are used
#' first and single year is only used as a final option.  N.B If
#' single_overrides_all is TRUE we would not expect year_col_pattern to have
#' been specified (unless it is also used in other functions).
#' @param multi_year_range_is_not_valid bool. If NA or NULL defaults to FALSE
#' If TRUE, and the year is a range of years e.g '2021-22 to 2023-24' or
#' '2023, 2024', the year column will show NA. Whether it is TRUE or FALSE,
#' year_type will be NA for invalid year entries.
#'
#' @returns dataframe with quarter, fy_start, and year columns.
#' 
#' @export
add_time_period_columns <- function(
    dat, quarter_from_col_pattern, quarter_col_name, year_from_pattern,
    fy_from_fy_end, fy_start_from_fy_end, fy_end_pattern,
    calendar_year_to_fy_start, q1_is_jan_to_mar, quarter_col_pattern,
    month_col_pattern, year_col_pattern, single_year_of_data, year_for_column,
    single_year_overrides_all, multi_year_range_is_not_valid
) {

  q_added <- add_quarter_column(dat, quarter_from_col_pattern, quarter_col_name)

  fy_start_added <- get_fy_start(
    q_added, year_from_pattern, fy_from_fy_end, fy_start_from_fy_end,
    fy_end_pattern, calendar_year_to_fy_start, q1_is_jan_to_mar,
    quarter_col_pattern, month_col_pattern
    )

  year_col_cleaned <- process_year_column(
    fy_start_added, year_col_pattern, single_year_of_data, year_for_column,
    single_year_overrides_all, multi_year_range_is_not_valid
  )

  simple_fy_start_added <- fill_missing_fy_start(year_col_cleaned)

  return(simple_fy_start_added)
}


#' @title Get quarter from a column containing more than just quarter.
#'
#' @description Extract single quarters (i.e. not quarter ranges) from a given
#' column e.g. quarter may be given alongside year.
#'
#' @param dat dataframe containing one column name that matches the
#' quarter_from_pattern
#' @param quarter_from_pattern character string. A regular expression that
#' matches only the column containing quarter information. In pub sec this
#' variable is supplied by quarter_from_col_pattern.
#' @param quarter_col_name character string. The name of the column that quarter
#' will be put in.
#'
#' @return dat with quarter column added. Warnings are raised about rows where
#' no single quarter or multiple single quarters have been found.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   x = c("2023Q4", "2024Q1", "2023Q1 to Q3", "2023Q1 Q3", "Q4", "2021", "no")
#' )
#' add_quarter_column(dat, "x", "quarter")
#' }
#' @export
add_quarter_column <- function(dat, quarter_from_pattern, quarter_col_name) {

  if (all(is.na(quarter_from_pattern), is.na(quarter_col_name))) {
    return(dat)
  } else if (is.na(quarter_from_pattern)) {
    stop(
      "A column name has been provided for the column to put quarter in, ",
      "but no pattern has been provided to identify the column to extract it ",
      "from. Please amend the settings."
    )
  } else if (is.na(quarter_col_name)) {
    stop(
      "A pattern has been provided for the column to extract quarter from, ",
      "but no new name has been provided for the column to extract it to. ",
      "Please amend the settings."
    )
  }

  message("Creating '", quarter_col_name, "' column for quarter.")

  quarter_col <- unname(
    get_colnames_from_pattern(dat, quarter_col_name, quarter_from_pattern)
  )

  message("Quarter will be extracted from the '", quarter_col, "' column.")

  if (is.na(quarter_col)) {
    stop(
      "Quarter column not identified - please contact a developer to update ",
      "the quarter_col_pattern in the settings."
    )
  }

  if (quarter_col_name %in% names(dat)) {
    warning(
      "'", quarter_col_name, "' is already a column in the data. It will be ",
      "overwritten with information taken from '", quarter_col, "'. To ",
      "prevent this you may want to edit the setting for quarter_col_name."
    )
  }

  patterns <- make_quarter_patterns()["single"]

  extracted <- dat %>%
    rowwise() %>%
    mutate(found := str_extract_all(!!sym(quarter_col), patterns),
           !!sym(quarter_col_name) := str_squish(unlist(found)[1])) %>%
    ungroup()

  check <- extracted %>%
    rowwise() %>%
    mutate(
      multi_q_warning = ifelse(length(found) > 1, TRUE, FALSE),
      missing_warning = ifelse(
        nchar(!!sym(quarter_col)) > 0 & length(found) == 0,TRUE, FALSE)
    ) %>%
    ungroup()

  if (sum(check$multi_q_warning) > 0) {
    multi_example <- check[which(check$multi_q_warning), quarter_col][1]
    warning(
      "More than one quarter found in ", sum(check$multi_q_warning),
      " entry(ies) e.g. in '", multi_example, "'. In such cases the first ",
      "will be assumed correct. Please contact a developer if quarters are ",
      "wrong."
    )
  }

  if (sum(check$missing_warning) > 0) {
    missing_example <- check[which(check$missing_warning), quarter_col][1, 1]
    warning(
      "No individual quarters found in ", sum(check$missing_warning),
      " entry(ies) e.g. in '", missing_example, "'. In such cases quarter ",
      "will be left blank. Please contact a developer if quarters are ",
      "wrong."
    )
  }

  used_columns_removed <- select(extracted, -"found")

  return (used_columns_removed)

}


#' @title Fill missing financial year start values.
#'
#' @description Fill in missing fy_start values for rows where year_type is
#' 'financial'.
#'
#' @details If a row has 'financial' as year_type and fy_start is missing (or
#' the column does not exist), extract the first four digits from the year
#' column and use this for fy_start. Note that this requires the year column to 
#' ONLY contain year.
#' 
#' Rows with other year_type values are left unchanged. This function is 
#' intended for use with datasets that contain both calendar and financial years
#' but could be used in other scenarios.
#'
#' @param dat dataframe containing columns called 'year' and 'year_type'.
#'
#' @returns dataframe. dat with fy_start filled in for financial years.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'     year = c("2020", "2020-21", "2021-22"),
#'     year_type = c("calendar", "financial", "financial"),
#'     fy_start = c(NA, NA, "2021")
#'     )
#'
#' fill_missing_fy_start(dat)
#'
#' }
#' @export
fill_missing_fy_start <- function(dat) {

  message(
    "Filling blank fy_start entries for rows where year_type is 'financial'."
  )

  if (! "year" %in% names(dat)) {
    stop("Column 'year' not found in data")
  }
  if (! "year_type" %in% names(dat)) {
    stop(
      "Column 'year_type' not found in data. This is required for filling ",
      "fy_start."
    )
  }

  # create fy_start column if it does not already exist.
  if (! "fy_start" %in% names(dat)) {
    dat <- mutate(dat, fy_start = NA)
  }

  all_fy_starts_filled <- dat %>%
    mutate(
      fy_start = case_when(
        is.na(fy_start) & year_type == "financial" ~ substr(year, 1, 4),
        is.na(fy_start) & year_type != "financial" ~ NA,
        .default = as.character(fy_start)
      )
    )

  return(all_fy_starts_filled)
}

