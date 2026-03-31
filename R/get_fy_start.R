#' @title Get fy_start from the available information on year.
#'
#' @description If financial year start year is not already available, extract
#' it from the available information e.g. from financial year, fy end year, or
#' calendar year and quarter or month (where Q1 is either Jan to March or Apr to
#' Jun).
#'
#' @param dat dataframe
#' @param year_from_pattern character string. Regular expression that matches
#' the name of the column containing year. This column may contain extra
#' information alongside year, such as quarter. Required unless fy_from_fy_end is provided.
#' @param fy_from_fy_end boolean. Default is FALSE. TRUE if the financial
#' year is to be calculated from the financial year end.fy_end_pattern must be
#' supplied if TRUE.
#' @param fy_start_from_fy_end boolean. Default is FALSE. TRUE if the financial
#' year start is to be calculated from the financial year end.
#' @param fy_end_pattern characters string. Regular expression that matches the
#' name of the column containing the financial year end. Default is NA
#' @param calendar_to_fy_start boolean. TRUE if calendar year is to be converted
#' to financial year start. Default is FALSE.
#' @param q1_is_jan_to_mar boolean. TRUE if Jan to Mar is Q1. FALSE if Apr to
#' Jun is Q1. Default is FALSE.
#' @param quarter_pattern character string. Regular expression used to
#' identify the column that contains only the quarter (e.g. 'Q1' or '1'). Not
#' required if quarter_from_pattern is supplied.
#' @param month_pattern character string. Regular expression used to
#' identify the column that contains only the month.
#'
#' @returns dataframe. Dat with fy_start added as a column. Depending on how
#' fy_start is ascertained, other columns may also be created ('year' and
#' 'quarter').
#'
#' @examples
#' \dontrun{
#' # 1. Only financial year end is available.
#' dat <- data.frame(
#'     fy_end = c(2021, 2022),
#'     value = c(1, 2)
#'     )
#' get_fy_start(
#'     dat, fy_from_fy_end = TRUE, fy_start_from_fy_end = TRUE,
#'     fy_end_pattern = "fy"
#'     )
#'
#' # 2. Only calendar year and quarter are available in a single column.
#' dat <- data.frame(
#'     year = c("2021 (Q4 jan-mar of 20-21)", "2021 (Q1 apr-jun of 21-22"),
#'     value = c(1, 2)
#'     )
#' get_fy_start(
#'     dat, year_from_pattern = "year", get_quarter = TRUE,
#'     quarter_from_pattern = "year", quarter_col_name = "quarter",
#'     calendar_to_fy_start = TRUE,  q1_is_jan_to_mar = FALSE,
#'     )
#'
#' # 3. Only calendar year and month are available in separate columns.
#' dat <- data.frame(
#'     year = c("2021", "2021"),
#'     month = c("jan-mar", "apr-jun"),
#'     value = c(1, 2)
#'     )
#' get_fy_start(
#'     dat, year_from_pattern = "year", month_pattern = "month",
#'     calendar_to_fy_start = TRUE,  q1_is_jan_to_mar = FALSE,
#'     )
#' }
get_fy_start <- function(
    dat, year_from_pattern = NA, fy_from_fy_end = FALSE,
    fy_start_from_fy_end = FALSE, fy_end_pattern = NA,
    calendar_to_fy_start = FALSE, q1_is_jan_to_mar = FALSE,
    quarter_pattern = NA, month_pattern = NA
) {

  year_col_created <- dat %>%
    get_year_from_column(year_from_pattern) %>%
    get_year_info_from_fy_end(
      fy_end_pattern, fy_start_from_fy_end, fy_from_fy_end
    )

  output <- year_col_created %>%
    get_fy_start_from_calendar_year(
      calendar_to_fy_start, q1_is_jan_to_mar, "^year$", month_pattern,
      quarter_pattern
    )

  return(output)
}



#' @title Extract the first year and save in a new column called year.
#'
#' @description Find the column containing year using a regular expression,
#' get the year from that column and put it in a new column called 'year'.
#' 'year' can contain either financial year, or calendar year,
#' but not both. By default, if any of the years found in the specified column
#' are financial (YYYY-YY), only financial years are put in the new column.
#' Calendar years are used if no financial years are found anywhere in the
#' column. Preference of calendar or financial year is controlled with `prefer`.
#' If a string contains more than one year (of the same type), only the first is
#' put in 'year'.
#'
#' @param dat dataframe.
#' @param pattern string. Regular expression that matches only the column name
#' of the column with strings containing year information. In pub sec this
#' variable is specified with column_pattern_containing_year.
#'
#' @returns dataframe dat with a new column added called 'year'. If 'year' is
#' already a column in dat an error is raised.
#'
#' @examples
#' \dontrun{
#' dat_mixed <- data.frame(
#'      `date span` = c("Jan to Mar 2021", "2020-21")
#'      )
#' dat_w_year <- data.frame(
#'      year = c("Jan to Mar 2021", "2020-21")
#'      )
#' dat_calendar <- data.frame(`date span` =
#'                              c("Jan to Mar 2021", "Apr 2020 to Mar 2021"))
#' dat_financial <- data.frame(`date span` = c("Q1 to Q4 2020-21", "2021"))
#'
#' get_year_from_column(dat_mixed, "(?i)date*.span")
#' get_year_from_column(dat_w_year, "(?i)year")
#' get_year_from_column(dat_calendar, "(?i)date*.span")
#' get_year_from_column(dat_financial, "(?i)date*.span")
#' }
#' @export
get_year_from_column <- function(dat, pattern) {

  if (is.na(pattern)) { return(dat) }

  message("Getting year from specified column.")

  year_colname <- get_colnames_from_pattern(dat, "year", pattern)["year"]

  # because we get year colname using get_colname_from_pattern if there is
  # more than one match year_colname will be NA, so we do not need to explicitly
  # check for multiple matches again
  if (is.na(year_colname)) {
    stop(
      "Column containing year information not identified - The pattern ",
      "given may have matched more than one column, or no columns",
      " (Note that 'date' is a standard column in xlsx_cells data). ",
      "Please contact a developer to update column_pattern_containing_year ",
      "in the settings.")
  }

  if ("year" %in% names(dat)) {
    message(
      "'year' is already a column in the data. It will be renamed to ",
      "'_year' to avoid it being overwritten."
    )
    dat <- rename(dat, "_year" = year)
    year_colname <- "_year"
  }

  output <- get_year(dat, year_colname)

  message("'year' taken from '", year_colname, "'.")

  return (output)

}


#' @title Get year from column containing the financial year end
#'
#' @description If the only year column given is the financial year end, this
#' function can be used to get the financial year (YYYY-YY) or the financial
#' year start (YYYY), or both. These are added as new columns. No columns are
#' removed.
#'
#' @param dat dataframe containing a column name that matches fy_end_pattern
#' @param fy_end_pattern character string. Regex pattern for the name of the
#' column containing financial year end information in the format YYYY
#' @param fy_start_from_fy_end bool. Default is FALSE. If TRUE, a column called
#' fy_start is added to dat and filled using the format YYYY. If fy_start is
#' already a column in dat, it is NOT overwritten. In pub sec the setting
#' is also called fy_start_from_fy_end.
#' @param fy_from_fy_end bool. Default is FALSE. If TRUE, a column called 'year'
#' is added to dat and populated using the format YYYY-YY. If year is
#' already a column in dat, it is NOT overwritten. In pub sec the setting
#' is also called fy_start_from_fy_end.
#'
#' @returns dataframe dat with new columns.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame("fy_end" = c(2021, 2022))
#' get_year_info_from_fy_end(dat, "end",
#'                           fy_start_from_fy_end=TRUE,
#'                           fy_from_fy_end=TRUE)
#' }
get_year_info_from_fy_end <- function(
    dat, fy_end_pattern, fy_start_from_fy_end=FALSE, fy_from_fy_end=FALSE
) {

  if (is.na(fy_start_from_fy_end)) {
    fy_start_from_fy_end <- FALSE
  }

  if (is.na(fy_from_fy_end)) {
    fy_from_fy_end <- FALSE
  }

  if (all(fy_start_from_fy_end == FALSE, fy_from_fy_end == FALSE)) {
    return(dat)
  }

  if (any(fy_start_from_fy_end, fy_from_fy_end) & is.na(fy_end_pattern)) {
    stop(
      "A regular expression to identify the column containing fy end must be ",
      "supplied as one or both of fy_start_from_fy_end and fy_from_fy_end is ",
      "TRUE."
    )
  }

  message(
    "Getting year information from column containing financial year end."
  )

  financial_year_end_col <- get_matching_colnames(dat, fy_end_pattern)

  if (length(financial_year_end_col) > 1) {
    warning(
      "More than one column found matching fy_end_pattern (",  fy_end_pattern,
      ") in the name. ",
      "None have been edited to show financial year. Please check that year ",
      "is correct in the validated data."
    )

  } else if (length(financial_year_end_col) == 1) {

    if (fy_start_from_fy_end) {

      dat <- get_fy_start_from_fy_end(dat, financial_year_end_col)

      message(
        paste0(
          "Column: '", financial_year_end_col,
          "' has been used to create 'fy_start' in the format YYYY."
        ))

    }

    if (fy_from_fy_end) {

      dat <- get_fy_from_fy_end(dat, financial_year_end_col)
      message(
        paste0(
          "Column: '", financial_year_end_col,
          "' has been used to create 'year' in the format YYYY-YY."
        ))

    }

  } else {
    stop(
      "No columns found with '", fy_end_pattern, "' in the name. ",
      "financial year wil not be created from financial year end."
      )
  }
  return(dat)
}


#' @title get financial year start year from financial year end year
#'
#' @description Get the financial year start year from a column containing
#' the financial year end year as YYYY.  (fy_start = fy_end - 1).
#'
#' @param dat dataframe
#' @param end_year character string. The name of the column containing fy_end
#' information in the format YYYY.
#'
#' @returns dataframe dat with additional column 'fy_start' and with invalid
#' year information removed from the end_year column. If there are any entries
#' in the end_year column that do not contain a valid year, fy_start is given
#' as NA for those rows, and a warning is raised.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(fy_end = c("2000", "this 2020", "blue"))
#' get_fy_start_from_fy_end(dat, "fy_end")
#' }
#' @export
get_fy_start_from_fy_end <- function (dat, end_year) {

  message(
    "Getting financial year start from financial year end column: '",
    end_year, "'."
  )

  if ("fy_start" %in% names(dat)) {
    warning(
      "'fy_start' is already a column in dat so will not be created from '",
      end_year, "'."
    )
    return (dat)
  }

  year_cleaned_name <- paste0(end_year, "_cleaned")
  # We expect the end year to be a single year, not a financial year (YYYY-YY)
  # So only pick up on calendar year pattern matches.
  year_cleaned <- get_year(
    dat, end_year,
    new_col = year_cleaned_name,
    prefer = "calendar",
    type = "both"
  )

  output <- year_cleaned %>%
    mutate(fy_start = as.integer(!!sym(year_cleaned_name)) - 1) %>%
    select(-c(!!sym(year_cleaned_name)))

  invalid_year_count <- output %>%
    filter(!is.na(!!sym(end_year)) & is.na(fy_start)) %>%
    count() %>%
    pull(n)

  if (invalid_year_count > 0) {
    warning(
      invalid_year_count, " rows in '", end_year, "' did not contain a ",
      "recognised year (YYYY). For these rows year is given as NA. If these ",
      "rows need to contain a valid year please contact a developer as they ",
      "may need to update the get_year clanedar year pattern."
    )
  }

  return(output)

}


#' @title Convert financial year end column (yyyy) to yyyy-yy
#'
#' @description Add a new column to a daatframe. Use the financial year end
#' (given as yyyy) to generate a column that gives financial year in the format
#' yyyy-yy.
#'
#' If the end_year column contains invalid year entries (no numbers, or a number
#' that is not four characters long), year will be given as NA, and the user
#' is given a warning.
#'
#' @param dat dataframe containing end_year.
#' @param end_year character string. Name of a column in dat containing years as
#' yyyy.
#'
#' @returns dataframe dat with a new column  called year, with strings in
#' the format yyyy-yy, using the previous year to the one passed in.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(fin_year_end = c("2020", "2021", "999", "non"))
#' get_fy_from_fy_end(dat, "fin_year_end")
#' }
#' @export
get_fy_from_fy_end <- function(dat, end_year) {

  message(
    "Getting financial year from financial year end column: '", end_year, "'."
  )

  if ("year" %in% names(dat)) {
    warning(
      "'year' is already a column in dat so will not be created from '",
      end_year, "'."
    )
    return (dat)
  }

  year_cleaned_name <- paste0(end_year, "_cleaned")
  year_cleaned <- get_year(dat, end_year, new_col = year_cleaned_name)

  output <- year_cleaned %>%
    mutate(my_year_end =
             substr(
               as.character(!!sym(year_cleaned_name)),
               3, 4),
           my_century =
             substr(
               as.character(!!sym(year_cleaned_name)),
               1, 2)) %>%
    mutate(my_year_end_numeric = as.numeric(my_year_end)) %>%
    mutate(my_year_start_numeric = my_year_end_numeric - 1) %>%
    mutate(year = ifelse(
      !is.na(my_year_end),
      paste0(my_century, my_year_start_numeric, "-", my_year_end_numeric),
      !!sym(year_cleaned_name)
    )) %>%
    select(
      -c(
        my_year_end, my_century, my_year_start_numeric, my_year_end_numeric,
        !!sym(year_cleaned_name)
      ))

  invalid_year_count <- output %>%
    filter(!is.na(!!sym(end_year)) & is.na(year)) %>%
    count() %>%
    pull(n)

  if (invalid_year_count > 0) {
    warning(
      invalid_year_count,
      " rows in '", end_year, "' either contained no numbers or the first ",
      "block of numbers in the entry was not four characters long. These rows ",
      "were therefore considered to have invalid years. For these rows year ",
      "is given as NA. If these rows need to contain a valid year please ",
      "contact a developer."
    )
  }

  return(output)
}


#' @title Get the year column name to be used by get_quarter
#'
#' @description Get the name of the column that matches the year pattern and
#' contains a valid years. If more than one year type is found, preferentially
#' return calendar year, then the column that contains at least some calendar
#' years, then financial. This order is selected because the quarter column is
#' more likely to state a calendar year than financial in sources seen so far.
#' However, if the financial column is preferred over the calendar year, this
#' can be done by making the year_pattern more specific to that column name.
#'
#' @param dat dataframe with a column name that matches pattern.
#' @param pattern character string. Regular expression that matches the column
#' containing year
#'
#' @returns character string. The name of the year column
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   Year = c("2023", "2024"),
#'   financial_year = c("2023-24", "2023-24"),
#'   quarter = c("2023Q4", "2024Q1"),
#'   Value = c(1, 2)
#' )
#' get_year_col_to_drop_from_quarter(dat, "(?i)year")
#' }
#' @export
get_year_col_to_drop_from_quarter <- function(dat, pattern) {

  possible_cols <- get_year_column_names(dat, pattern)
  columns <- possible_cols[!is.na(possible_cols)]
  types <- names(columns)

  if (length(columns) > 1) {
    warning(
      "More than one year column matching the year col pattern identified: '",
      paste0(columns, collapse = "', '"), "'."
    )
  }

  if ("calendar" %in% types) {
    column <- unname(columns["calendar"])
  } else if ("mixed" %in% types) {
    column <- unname(columns["mixed"])
    warning(
      "Some years have been identified as financial years. Check ",
      "that year and quarter are as expected. If not, contact a developer."
    )
  } else if ("financial" %in% types) {
    column <- unname(columns["financial"])
  } else {
    warning(
      "No valid year column found, so year will not be dropped from the ",
      "quarter column. Please contact a developer to update the settings."
    )
    return (NA)
  }
  message("'", column, "' used.")

  return(column)
}


#' @title Get financial year start for quarters in datasets with Q1 as Apr-Jun
#'
#' @description Get financial year (FY) start for quarters. This is calculated
#' differently when year is given as calendar year but Q1 is Apr-Jun and when
#' Q1 is Jan-May. In the former, financial year start will equal the calendar
#' year for Q1-Q3 but Q4/Jan-Mar financial year start will be year minus one.
#'
#' SAP processing may require FY start to perform calculations but in data
#' published on a calendar year basis (where Q1 is the first quarter of the
#' calendar year), FY may not be given. This function calculates financial year
#' start from the calendar year and either the quarter or the month.
#'
#' See make_calendar_q1_month_and_quarter_patterns() for the accepted
#' presentation of months and quarters (e.g. 'jan'/'Jan-Mar'/'Q1'/'Quarter 1').
#'
#' For 2021, jan-mar, Q1, Financial year will be 2020-21 <- fy start == year-1.
#' For 2021, apr-jun, Q1, Financial year will be 2021-22 <- fy start == year.
#' For 2021, apr-dec, Q1-Q3, Financial year will be 2021-22 <- fy start == year.
#'
#' @param dat dataframe containing a year column and either a month or quarter
#' column
#' @param calendar_year_to_fy_start boolean. default is FALSE. If TRUE, actions
#' are performed, if NA or FALSE the original data are returned.
#' @param q1_is_jan_to_mar boolean. Default is TRUE and means that
#' Q1 is for Jan to Mar. Use FALSE when Q1 is for Apr to Jun i.e. the first
#' quarter of the financial year.
#' @param year_from_pattern character string. Defaults to "(?i)year". Regular
#' expression matched by the column name for the year column. Must only match
#' one column name. This column must be in the format YYYY NOT YYYY-YY or YY.
#' Non-numeric information can be present as it will be removed (i.e. '2024 -r'
#' will be seen as '2024'). There must be no numbers present other than the
#' year. Column can be character or integer.
#' @param month_col_pattern character string. Same as year_from_pattern but for
#' month. Only required if quarter_col_pattern is not given.
#' @param quarter_col_pattern character string. Same as year_from_pattern but for
#' quarter. Only required if month_col_pattern is not given.
#' @param calendar_year_to_fy_start boolean. Default is FALSE. TRUE if the
#' action is to be performed, NA or FALSE to return the original data.
#'
#' @returns dat with fy_start (character string) column if
#' calendar_year_to_fy_start is TRUE, or the original dat if not.
#'
#' @examples
#' \dontrun{
#' # q1_is_jan_to_mar is false:
#' dat <- tibble(
#'   Year = c("2021","2021", "2021", "2021", "2022"),
#'   Quarter = c("Q4", "Q1", "Q2", "Q3", "Q4"),
#'   Months = c("jan - mar", "apr - jun", "jul - sep", "oct - dec", "jan - mar"),
#'   Value = 1:5
#' )
#' get_fy_start_from_calendar_year(
#'     dat, calendar_year_to_fy_start = TRUE,
#'     q1_is_jan_to_mar = FALSE,
#'     year_from_pattern = "(?i)year",
#'     quarter_col_pattern = "(?i)quarter",
#'     month_col_pattern = NA
#'     )
#'
#' # q1_is_jan_to_mar is true:
#' dat <- tibble(
#'   Year = c("2021","2021", "2021", "2021", "2022"),
#'   Quarter = c("Q1", "Q2", "Q3", "Q4", "Q1"),
#'   Month = c("jan - mar", "apr - jun", "jul - sep", "oct - dec", "jan - mar"),
#'   Value = 1:5
#' )
#' get_fy_start_from_calendar_year(
#'     dat, calendar_year_to_fy_start = TRUE,
#'     q1_is_jan_to_mar = FALSE,
#'     year_from_pattern = "(?i)year",
#'     quarter_col_pattern = "(?i)quarter"
#'     )
#'
#' # month is given for some rows, quarter is given for others, and some have both:
#' dat <- tibble(
#'   Year = c("2021", "2022", "2022", "2022", "2022"),
#'   Quarter = c("Q3", "Q4", NA, "Q1", NA),
#'   Months = c("oct - dec", "jan", "jan - mar", NA, "Jul"),
#'   Value = 1:5
#' )
#' get_fy_start_from_calendar_year(
#'     dat, calendar_year_to_fy_start = TRUE,
#'     q1_is_jan_to_mar = FALSE,
#'     year_from_pattern = "(?i)year",
#'     quarter_col_pattern = "(?i)quarter",
#'     month_col_pattern = "(?i)month"
#'     )
#' }
#' @export
get_fy_start_from_calendar_year <- function(
    dat, calendar_year_to_fy_start = FALSE, q1_is_jan_to_mar = TRUE,
    year_from_pattern = "(?i)year", month_col_pattern = NA,
    quarter_col_pattern = NA
) {

  if (is.na(calendar_year_to_fy_start) | calendar_year_to_fy_start == FALSE) {
    return(dat)
  }

  message("Getting financial year start from column containing calendar year.")

  if (all(is.na(month_col_pattern), is.na(quarter_col_pattern),
          calendar_year_to_fy_start == TRUE)) {
    stop("calendar_year_to_fy_start is TRUE, but neither ",
         "month_col_pattern, nor quarter_col_pattern has been supplied. ",
         "Developer: please supply one of these in the data dictionary.")
  }

  if ("fy_start" %in% names(dat)) {
    stop("calendar_year_to_fy_start is TRUE, but fy_start is already a ",
         "column. Calendar year not converted to fy_start.")
  }

  year_col <- get_colnames_from_pattern(dat, "year", year_from_pattern)["year"]

  if (is.na(q1_is_jan_to_mar)) { q1_is_jan_to_mar <- TRUE}

  if (q1_is_jan_to_mar) {
    message(
      "Q1 is set as being for jan - mar. If Q1 is ",
      "meant to be apr - jun in this data source ",
      "please contact a developer to update the settings ",
      "('q1_is_jan_to_mar')."
    )
    message(
      "'fy_start' will be created so that the first three months (Q1) ",
      "of each year equals '",  as.character(year_col), "' - 1. "
    )
  } else {
    message(
      "Q1 is set as being for apr - jun. If Q1 is ",
      "meant to be jan - mar in this data source ",
      "please contact a developer to update the settings ",
      "('q1_is_jan_to_mar')."
    )
    message(
      "'fy_start' has been created so that the first three months ",
      "of each year (Q4) equals '",  as.character(year_col), "' - 1 "
    )
  }

  period_col_names <- get_colnames_from_pattern(
    dat, c("month", "quarter"), c(month_col_pattern, quarter_col_pattern)
  )
  if (!is.na(month_col_pattern) & is.na(period_col_names["month"])) {
    stop(
      "Please fix the settings to address the issue with month_col_pattern. ",
      "month_col_pattern and/or calendar_year_to_fy_start may need to change."
    )
  }
  if (!is.na(quarter_col_pattern) & is.na(period_col_names["quarter"])) {
    stop(
      "Please fix the settings to address the issue with quarter_col_pattern. ",
      "quarter_col_pattern and/or calendar_year_to_fy_start may need to change."
    )
  }

  # run month and quarter through separately, then compare the results before
  # setting fy_start
  fy_start_by_type <- dat
  for (i in 1:2) {

    # If months and quarters are specified, for different rows, we need to
    # distinguish between the columns created from each and then compare them
    # to check for issues.
    fy_start_colname <- paste0("fy_start_", names(period_col_names[i]))

    if (!is.na(period_col_names[i])) {
      period_col_name <- period_col_names[i]
    } else {
      fy_start_by_type <- fy_start_by_type %>%
        mutate(!!sym(fy_start_colname) := NA)
      next
    }

    end_year_items_pattern <- (
      make_calendar_q1_month_and_quarter_patterns("all", q1_is_jan_to_mar)
    )

    # flag the years that need to be changed by looking for those that are not
    # financial years (i.e. they  have 4 characters) and they contain a
    # Q1/Jan-Mar pattern in the period column
    years_flagged <- fy_start_by_type %>%
      mutate(
        financial_year = ifelse(nchar(!!sym(year_col)) > 4, TRUE, FALSE),
        change_year = ifelse(
          financial_year == FALSE
          & str_detect(!!sym(period_col_name), end_year_items_pattern),
          TRUE, FALSE
        )) %>%
      # so that we can convert the year to a number, change the
      # financial years to fy_start
      mutate(temp_numeric_year := substr(!!sym(year_col), 1, 4))

    fy_start_for_type_i <- years_flagged %>%
      mutate(
        !!sym(fy_start_colname) := case_when(
          is.na(!!sym(period_col_name)) | !!sym(period_col_name) == "" ~ NA,
          change_year == FALSE ~ as.numeric(temp_numeric_year),
          change_year == TRUE ~ as.numeric(temp_numeric_year) - 1
        )) %>%
      select(-c(change_year, temp_numeric_year))

    join_names <- intersect(names(fy_start_by_type), names(fy_start_for_type_i))

    fy_start_by_type <- fy_start_by_type %>%
      left_join(fy_start_for_type_i, by = join_names)

  }

  fy_starts_checked <- fy_start_by_type %>%
    mutate(
      fy_start = case_when(
        is.na(fy_start_month) ~ fy_start_quarter,
        is.na(fy_start_quarter) ~ fy_start_month,
        fy_start_month == fy_start_quarter ~ fy_start_month,
        fy_start_month != fy_start_quarter ~ NA,
        .default = NA
      ),
      fy_warning = case_when(
        fy_start_month != fy_start_quarter
        & !is.na(fy_start_month) & !is.na(fy_start_quarter) ~ "mismatch",
        .default = NA
      )
    )

  mismatches <- fy_starts_checked[!is.na(fy_starts_checked$fy_warning), ]
  if (nrow(mismatches) > 0) {
    warning(
      "fy_start calculated for month did not match fy_start calculated for ",
      "quarter in ", nrow(mismatches), " row(s). The first instance is: '",
      paste0("fy_start = ", mismatches$fy_start[1], "fy_start for month = ",
             mismatches$fy_start_month[1], "fy_start for quarter = ",
             mismatches$fy_start_month[1], "'.")
    )
  }

  # Get fy_start for rows where financial year is given but not quarter or month
  output <- fy_starts_checked %>%
    mutate(
      fy_start := case_when(
        is.na(fy_start)
        & financial_year == TRUE ~ as.integer(substr(!!sym(year_col), 1, 4)),
        .default = as.integer(fy_start)
      )
    ) %>%
    select(
      -c(fy_warning, fy_start_month, fy_start_quarter, financial_year)
    )

  return(output)
}
