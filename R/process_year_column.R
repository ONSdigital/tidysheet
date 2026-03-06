#' @title Process the 'year' column in a data frame
#'
#' @description Process the 'year' column.
#' Potential year columns are identified either by their matching a provided
#' pattern OR by matching '(?i)year' if pattern is not provided. Where there is
#' no year column, it will be generated from a provided single year if
#' use_single_year is true.
#'
#' If more than one year column is available, financial is preferred over
#' calendar, which is preferred over a mix of the two, which is preferred over
#' a given single year, unless single_overrides all is true. Note that in pub
#' sec, single year is taken from the filename which comes from the user
#' selection of what to download/process.
#' if the user has supplied a pattern and it matches the name of a column/
#' columns containing valid years, this/these columns will be the only
#' possible_cols. If pattern is not supplied, any column with 'year' in the
#' name that contains valid years will be included (one column per year type).
#' However, if single_overrides_all is TRUE and use_single_year is TRUE.
#'
#' Year type is ascertained for each row (financial or calendar). If it is
#' financial year it is standardised to the format YYYY-YY (rather than e.g.
#' YYYY/YY or YYYY-YYYY)
#'
#' If the original year column is called 'year' it will be overwritten.
#'
#' @param dat The data frame containing the data to be processed.
#' @param pattern character string. The name of the year column as given by a
#' user. If this is NA, the year column is identified automatically by
#' get_year_column_names(). In pub sec this variable is specified by
#' year_col_pattern.
#' @param use_single_year boolean or NA. If TRUE create a column called year and
#' populate it with single_year, in pub sec this variable is specified by
#' single_year_of_data. Unless single_overrides_all is TRUE, this will only be
#' used as a last option.
#' @param single_year list of length 2: 'year' is a character string containing
#' a valid year; 'warn' is boolean. TRUE if a warning needs to be raised.
#' single_year is only used if use_single_year is TRUE. IF so, the 'year'
#' column is populated with this year value. Must be specified if
#' use_single_year is TRUE. In pub sec this variable is taken either from the
#' file name (which gets its year from the user config selections) or from the
#' information above the data. 'warn' is TRUE when the file name year and the
#' year from above the data both exist but do not match.
#' @param single_overrides_all boolean or NA. If TRUE, and use_single_year is
#' TRUE, single_year overrides any columns found with year in the name. If FALSE
#' or NA, other options for filling the year are used first and single year is
#' only used as a final option.  N.B If single_overrides_all is TRUE we would
#' not expect pattern to have been specified (unless it is also used in other
#' functions).
#' @param multi_year_entries_as_na bool. If NA or NULL defaults to FALSE
#' If TRUE, and the year is a range of years e.g '2021-22 to 2023-24' or
#' '2023, 2024', the year column will show NA. Whether it is TRUE or FALSE,
#' year_type will be NA for invalid year entries. In pub sec this variable is
#' specified with the same name (multi_year_entries_as_na)
#'
#' @returns The modified data frame after processing the 'year' column. If they
#' did not already exist 'year' and 'year_type' are added as new columns,
#' otherwise they are overwritten with standardised text.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(year_col = c("2021", "2022_23 to 2025_26", "2021, 2022"))
#' process_year_column(dat, "year_col", NA, NA, NA, NA)
#'
#' dat <- data.frame(
#'     financial_year = c("2021-22 to 2022-23", "2022_2023", "words"),
#'     calendar_year = c(2021, 2022, NA)
#'     )
#' process_year_column(dat, NA, NA, NA, NA, TRUE)
#' process_year_column(dat, NA, NA, NA, NA, FALSE)
#'
#' # case where there is no year column, but there is a column that looks like
#' # it might be:
#' dat <- data.frame(
#'     description = c("A", "B", "C"),
#'     implementation_year = c(1980, 1992, 2003),
#'     value = 1:3
#'     )
#' single_year = list("year" = "2021_22", "warn" = TRUE)
#' process_year_column(
#'     dat, pattern = NA, use_single_year = TRUE, single_year = single_year,
#'     single_overrides_all = TRUE, multi_year_entries_as_na = TRUE
#'     )
#' }
#' @export
process_year_column <- function(
    dat, pattern, use_single_year, single_year, single_overrides_all,
    multi_year_entries_as_na
){

  if (all(is.na(single_year))) {
    single_year <- list("year" = NA, warn = FALSE)
  }

  if (!is.list(single_year)) {
    stop(
      "single_year must be a list with 'year' and 'warn' elements."
    )
  }
  if (! "year" %in% names(single_year)) {
    stop("single_year must include a character string element called 'year'.")
  }
  if (! "warn" %in% names(single_year)) {
    stop("single_year must include a boolean element called 'warn'.")
  }

  if (is.na(use_single_year)) { use_single_year <- FALSE }
  if (is.na(single_overrides_all)) { single_overrides_all <- FALSE }
  if (is.na(multi_year_entries_as_na)) { multi_year_entries_as_na <- FALSE }

  if (all(use_single_year, is.na(single_year[["year"]]))) {
    stop("use_single_year is TRUE, but single_year has not been provided.")
  }
  if (single_overrides_all & is.na(single_year[["year"]])) {
    stop("single_overrides_all is TRUE, but single_year has not been provided.")
  }
  if (single_overrides_all & (is.na(use_single_year)|use_single_year == FALSE)) {
    stop(
      "single_overrides_all is TRUE, but use_single_year has either not been ",
      "provided or is FALSE."
      )
  }

  possible_cols <- get_year_column_names(dat, pattern)

  all_possible_cols <- add_single_year_to_possibilities(
    possible_cols, use_single_year, single_year[["year"]], single_overrides_all
    )

  with_year <- add_best_year_column(dat, all_possible_cols, single_year)

  standardised <- standardise_year(with_year, "year")

  type_added <- mutate(standardised, year_type = get_year_type(year))

  year_ranges_removed <- clear_invalid_year_and_type(
    type_added, multi_year_entries_as_na
  )
  return(year_ranges_removed)
}


#' @title Get the column names of columns containing valid years.
#'
#' @description Get the names of columns that either match a supplied regular
#' expression or if this is not given, '(?i)year' and also contain valid years.
#'
#' @param dat dataframe
#' @param pattern character string or NA. A regular expression, that, if
#' supplied and it matches a column name it will be the only column name
#' retained.
#'
#' @returns named vector containing 'calendar', 'financial' and 'both'. Gives
#' the names of columns that contain year. If only calendar years are found the
#' column name is given under calendar, if only financial years are found the
#' name is given under financial, and if a mix is found the name is given under
#' mixed. If more than one column is found for any one of 'calendar',
#' 'financial', or 'both', the one with the most matches is returned along with
#' a warning.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'     financial_year = c("2012-2013", "2013-14 note", NA),
#'     mixed_year = c("2012-2013", "2013", "2014"),
#'     calendar_year = 2025,
#'     year_notes = c("", "something about 2021", ""),
#'     other = 2021
#'     )
#' get_year_column_names(dat, "year")
#' }
#' @export
get_year_column_names <- function(dat, pattern) {

  patterns <- make_year_patterns()

  names_with_year <- names(dat)[str_detect(names(dat), "(?i)year")]

  if (!is.na(pattern)) {
    message("Finding column names to match pattern '", pattern, "'.")

    pattern_matches <- names(dat)[str_detect(names(dat), pattern)]

    if (length(pattern_matches) == 0) {
      # If there are no column name matches to the pattern supplied by the user,
      # other columns containing year will be checked instead
      names_to_check <- names_with_year
      warning(
        "No year column found to match the pattern in settings for year column ",
        "name. The year column will be automatically identified. If the year ",
        "column is wrong please contact a developer to edit the settings e.g.",
        "year_col_pattern may need to be more tightly constrained to '^year$'"
      )
    } else {
      # If there are column name matches to the pattern supplied by the user,
      # these will be the only columns that have a chance of being chosen as the
      # 'year' column.
      names_to_check <- pattern_matches
    }

  } else {
    # If the user hasn't supplied a pattern that matches the column containing
    # year, all columns containing 'year' in the name will be checked instead.
    message("Finding column names to match pattern '(?i)year'.")
    names_to_check <- names_with_year
  }

  if (length(names_to_check) == 0) {
    message("No pre-existing columns contain 'year' in their name.")
    return (NA)
  }

  named_vector <- get_year_names_by_type(dat, names_to_check)

  message(
    "possible year columns found: '",
    paste0(unname(named_vector)[!is.na(named_vector)], collapse = "', '"), "'."
    )

  return(named_vector)

}


#' @title get the column names and year types of columns containing years
#'
#' @description Create a named vector that states a maximum of one column name
#' for each year type, where year types are 'financial' (variations on YYYY-YY,
#' YYYY-YYY, YYYY_YY, YYYY to YYYY etc), 'calendar' (YYYY), or 'mixed' (a mix
#' of calendar and financial year types).
#'
#' @param dat dataframe
#' @param columns character string vector of column names.
#'
#' @returns named vector in which the name is the type of year and the value is
#' the name of the column. Types of year are 'financial', 'calendar', and
#' 'mixed'. If more than one of the given columns contains the same type of year
#' the one with the highest frequency of valid years is given for that type.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'     calendar_years = 2021,
#'     alternative_financial_years = c("2022-23", NA),
#'     financial_years = "2022-23",
#'     random = c("2021", "2022-23"),
#'     words = "hello"
#'     )
#' get_year_names_by_type(dat, names(dat))
#'
#' }
#' @export
get_year_names_by_type <- function(dat, columns) {

  cal_cols <- c()
  fin_cols <- c()
  mixed_cols <- c()
  cal_counts <- c()
  fin_counts <- c()
  mixed_counts <- c()

  patterns <- make_year_patterns()

  year_types <- dat

  for (i in 1:length(columns)) {

    year_col <- columns[i]

    year_types <- year_types %>%
      mutate(
        calendar_exists = str_detect(!!sym(year_col), patterns["calendar"]),
        financial_exists = str_detect(!!sym(year_col), patterns["financial"]),
        mixed = ifelse(calendar_exists|financial_exists, TRUE, FALSE)
      )

    mixed_count <- sum(year_types$mixed, na.rm = TRUE)
    cal_count <- sum(year_types$calendar_exists, na.rm = TRUE)
    fin_count <- sum(year_types$financial_exists, na.rm = TRUE)
    max_count <- max(mixed_count, cal_count, fin_count)

    no_years <- sum(mixed_count, cal_count, fin_count) == 0

    if (max_count == fin_count & no_years == FALSE) {
      fin_cols <- c(fin_cols, year_col)
      fin_counts <- c(fin_counts, fin_count)
    } else if (max_count == cal_count & no_years == FALSE) {
      cal_cols <- c(cal_cols, year_col)
      cal_counts <- c(cal_counts, cal_count)
    } else if (max_count == mixed_count & no_years == FALSE) {
      mixed_cols <- c(mixed_cols, year_col)
      mixed_counts <- c(mixed_counts, mixed_count)
    }
  }

  calendar_col <- get_column_with_most_valid_years(cal_cols, cal_counts)
  financial_col <- get_column_with_most_valid_years(fin_cols, fin_counts)
  mixed_col <- get_column_with_most_valid_years(mixed_cols, mixed_counts)

  named_vector <- c(
    calendar = calendar_col,
    financial = financial_col,
    mixed = mixed_col
  )

  return(named_vector)
}


#' @title Get the column name that has the highest count value
#'
#' @description Get the columns string that is in the same position as the
#' maximum count value. Designed to select the name of the column that holds the
#' most valid years, where columns gives the names of the columns holding valid
#' years, and counts gives the number of valid years found in each of those
#' columns, though it could be used in other contexts.
#'
#' @param columns character string vector of the same length as counts.
#' @param counts integer vector of the same length as columns.
#'
#' @returns character string. The column name associated with the maximum count.
#' If more than one column is has the maximum number of valid years, a warning
#' is raised stating that more than one column was found of the same type, and
#' which has been chosen. The chosen column will be 'year' if that exists, or
#' the first of the years with the maximum number of valid years.
#'
#' @examples
#' \dontrun{
#' get_column_with_most_valid_years(c("year", "year_notes"), c(25, 2))
#' get_column_with_most_valid_years(
#'     c("year", "year2", "year_notes"), c(25, 25, 2)
#'     )
#' }
get_column_with_most_valid_years <- function(columns, counts) {

  if (length(columns) > 1) {

    selected <- columns[which(counts == max(counts))]


    if (length(selected) > 1) {

      if (any(selected == "year")) {
        chosen <- "year"
      } else {
        chosen <- selected[1]
      }

      warning(
        "More than one column was found with the same type of year: '",
        paste0(columns, collapse = "', '"), "'. More than one of these had ",
        "the maximum number of valid years: ('",
        paste0(selected, collapse = "', '"), "'). '", chosen, "' will be ",
        "used. year_col_pattern may need to be more tightly constrained e.g. ",
        "to '^year$'."
      )

    } else if(length(selected) == 1) {
      chosen <- selected
      warning(
        "More than one column was found with the same type of year: '",
        paste0(columns, collapse = "', '"), "'. The column with the most valid ",
        "year strings is '", selected, "' so this will be seen as the column ",
        "holding this type of year info."
      )
    }

    return (chosen)

  } else if (length(columns) == 1) {

    return(columns)

  } else {

    return(NA)

  }

}


#' @title Add single_year as an option in possible cols
#'
#' @description single_year is just a year and doesn't yet have a column. Check
#' that it only contains one year (either financial or calendar) and if so give
#' it the future column name 'year' and add it to the vector of possible columns
#' from which to make the 'year' column.
#'
#' @param possible_cols named vector in which the name is the type of year
#' column and the value is the name of the column. This function assigns 'year'
#' to 'single_column'.
#' @param use_single_year boolean. If FALSE do nothing.
#' @param year character string or integer containing a valid year. Only
#' used if use_single_year is TRUE. The year with which to populate the 'year'
#' column. Must be of length 1 and must be specified if use_single_year is TRUE.
#' @param single_overrides_all boolean. If TRUE, and use_single_year is
#' TRUE, single_year overrides all other possible_cols. If FALSE or NA,
#' single_year is just appended to possible_cols.
#'
#' @returns named vector possible_cols with a new name appended to it
#' (single_cols) if single_overrides_all is FALSE, or with only single_cols if
#' single_overrides_all is TRUE.
#'
#' @examples
#' \dontrun{
#' possible_cols <- c("financial" = NA, "calendar" = "Year", "mixed" = NA)
#'
#' # single_overrides_all is FALSE
#' add_single_year_to_possibilities(possible_cols, TRUE, "2021/22", FALSE)
#'
#' # single_overrides_all is TRUE
#' add_single_year_to_possibilities(possible_cols, TRUE, "2021/22", TRUE)
#'
#' # Empty possible cols and single year exists
#' possible_cols <- c("financial" = NA, "calendar" = NA, "mixed" = NA)
#' add_single_year_to_possibilities(possible_cols, TRUE, "2021/22", FALSE)
#' }
#'
#'
add_single_year_to_possibilities <- function(
    possible_cols, use_single_year, year, single_overrides_all
) {

  if (all(is.na(possible_cols))) {
    possible_cols <- c()
  }

  if (use_single_year == FALSE) {
    possible_cols["single"] <- NA
    return(possible_cols)
  }

  year_patterns <- make_year_patterns()

  single_year_financial <- str_extract(year, year_patterns["financial"])
  single_year_calendar <- str_extract(year, year_patterns["calendar"])
  single_year_all <- c(single_year_financial, single_year_calendar)
  single_year_check <- length(single_year_all[!is.na(single_year_all)])

  if (single_year_check > 1) {
    stop("single_year contains more than one year. It must only contain one.")
  } else if (single_year_check == 0) {
    stop("single_year does not contain any valid years. Fix the settings.")
  }

  if (single_overrides_all) {
    # Remove any of the year columns found by get_year_column_names
    # and just use the one provided by single_year
    possible_cols <- c(single = "year")

  } else {
    # just append it to the original named vector
    possible_cols["single"] <- "year"
  }

  return(possible_cols)

}

#' @title Select the best option for the year column and add it to the data
#'
#' @description From possible_cols select the column name to use as the 'year'
#' column and add this to the data. If a column called 'year' already exists in
#' the data it will be overwritten.
#'
#' If there is more than one potential year column the order of preference is
#' financial year, then calendar year, then mixed. Only use single year if
#' single_overrides_all is TRUE, or if no other possible_cols exist.
#'
#' Even if single_year is provided, if it is not in possible_cols it will not
#' be used.
#'
#' @param dat dataframe containing the columns that could be used for 'year'.
#' @param possible_cols named vector in which the name is the type of year
#' column and the value is the name of the column. Types of year are
#' 'financial', 'calendar', 'mixed', and 'single'.
#' @param single_year list of length 2: 'year' is a character string containing
#' a valid year; 'warn' is boolean. TRUE if a warning needs to be raised.
#' single_year is only used if use_single_year is TRUE. IF so, the 'year'
#' column is populated with this year value. Must be specified if
#' use_single_year is TRUE. In pub sec this variable is taken either from the
#' file name (which gets its year from the user config selections) or from the
#' information above the data. 'warn' is TRUE when the file name year and the
#' year from above the data both exist but do not match.
#'
#' @returns dataframe. dat with year column added/overwritten with the best
#' year.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'     financial_year = c("2021-22 to 2022-23", "2022_2023"),
#'     calendar_year = c(2021, 2022)
#'     )
#' possible_cols <- c(
#'     calendar = "calendar_year",
#'     financial = "financial_year",
#'     mixed = NA,
#'     single = NA
#'     )
#' add_best_year_column(dat, possible_cols, "2021-22")
#' }
#'@export
add_best_year_column <- function(dat, possible_cols, single_year) {

  if (!is.na(possible_cols["financial"])) {
    column <- possible_cols["financial"]

  } else if (!is.na(possible_cols["calendar"])) {
    column <- possible_cols["calendar"]

  } else if (!is.na(possible_cols["mixed"])) {
    column <- possible_cols["mixed"]

  } else if (!is.na(possible_cols["single"])) {
    # For all other types the column is already in dat, but for single year
    # we have to populate it with the value given for single_year.
    message(
      "Adding year column and populating with '", single_year[["year"]], "'."
      )
    if (single_year[["warn"]]) {
      warning(
        "Year has been taken from either the file name or the information ",
        "above the data, but they do not match each other. If the year column ",
        "is incorrect please contact a developer to edit the setting for ",
        "use_year_from_filename_over_year_above_table. See earlier messages ",
        "for more detail."
        )
    }
    output <- mutate(dat, year = single_year[["year"]])
    return(output)

  } else {
    stop(
      "No valid year columns have been found. Please edit the year col ",
      "pattern in settings so that it picks up the correct column, or set ",
      "single_year_of_data to true."
      )
  }

  output <- rename(dat, year = !!sym(column))

  return(output)

}


#' @title Standardise year column so that financial years are yyyy-yy
#'
#' @description Where financial years are not in the standard format of
#' YYYY-YY, convert them to this format. Accepted input formats are:
#' YYYY_YY or YYYY_YYYY and any number of spaces either side of the underscore,
#' YYYY-YY or YYYY-YYYY and any number of spaces either side of the dash,
#' YYYYtoYY, and YYYY_to_YY.
#'
#' Calendar years (YYYY) remain as they are.
#'
#' @param dat dataframe with a column of the name given in `column`.
#' @param column character string. The name of the column holding years to
#' convert.
#'
#' @returns dataframe dat with a column called year containing the same as
#' `column` but with financial years standardised.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(fy = c("2021_22", "2020", "now 2022-2023", "other", "2021-22 to 2022-23"))
#' standardise_year(dat, "fy")
#' }
#' @export
standardise_year <- function(dat, column = NA) {

  message("Standardising format of year.")

  if (is.vector(dat)) {
    dat <- data.frame(year = dat)
    column <- "year"
    return_to_vector <- TRUE
  } else {
    return_to_vector <- FALSE
  }

  types <- get_year_names_by_type(dat, column)
  type <- names(types[which(!is.na(types))])
  patterns <- make_year_patterns()

  if (type == "mixed" | type == "financial") {

    message("standardising financial year format to YYYY-YY")

    short_form <- dat %>%
      mutate(year = case_when(
        str_detect(!!sym(column), patterns["long_form_financial"]) ~
          convert_long_form_fy_to_short(!!sym(column)),
        TRUE ~ !!sym(column),
      ))

    output <- short_form %>%
      mutate(year = str_replace(year, patterns["fy_separators"], "-")) %>%
      mutate(year = str_squish(
        str_replace(year, patterns["fy_separators"], "-")
      ))

  } else {
    output <- dat %>%
      mutate(year = str_extract(!!sym(column), patterns["calendar"]))
  }

  if (return_to_vector) {
    return(as.vector(output$year))
  } else {
    return(output)
  }

}

#' @title Convert long form calendar years to short form
#'
#' @description Where financial year end is four characters e.g. YYYY-YYYY,
#' remove the first two digits of the end year to get YYYY-YY. This should
#' only be called on columns that are known to contain valid years as it does
#' not year format specific - it just removes the preantepenultimate and
#' antepenultimate characters.
#'
#' @param year character string. A single year or a vector of single years.
#'
#' @returns character string. year in short form. If year is a calendar year or
#' a financial year already in short from no action is taken.
#'
#' @examples
#' \dontrun{
#' convert_long_form_fy_to_short(c("2021-2022", "2022 to 2023"))
#' }
#' @export
convert_long_form_fy_to_short <- function(year) {
  first_half <- substr(year, 1, nchar(year)-4)
  last_half <- substr(year, nchar(year)-1, nchar(year))
  paste0(first_half, last_half)
}


#' @title Set values to NA when year has more than 7 characters
#'
#' @description If year column contains an invalid entry such as multi-years
#' (e.g. '2020-21 to 2022-23'), year_type is set to NA. 'year' will also be set
#' to NA for these cases if year_to_na is TRUE.
#'
#' @param dat dataframe. Must contain columns called 'year' and 'year_type'.
#' @param year_to_na boolean. If TRUE, year and year type will
#' both be NA when more than one year, or no valid years are found. If FALSE,
#' only year_type, and entries where NO years are found will be changed to NA.
#' Defaults to FALSE. In pub sec this variable is specified by
#' multi_year_entries_as_na.
#'
#' @examples
#' \dontrun{
#' dat <- tibble(
#'    year = c(NA, "note", "2021-22", "2021-22 to 2022-23"),
#'    year_type = c(NA, NA, "financial", "financial")
#'    )
#' clear_invalid_year_and_type(dat, TRUE)
#' clear_invalid_year_and_type(dat, FALSE)
#' }
#' @export
clear_invalid_year_and_type <- function(dat, year_to_na) {

  if (!'year' %in% names(dat)) {
    stop("dat must contain a column called 'year'.")
  }
  if (!'year_type' %in% names(dat)) {
    stop("dat must contain a column called 'year_type'.")
  }

  message("Removing year types that are not valid")

  # We need a column to hold notes about invalid years that are removed, but
  # must ensure we don't overwrite an existing column of that name.
  if (!'year_notes' %in% names(dat)) {
    dat <- mutate(dat, year_notes = NA)
  }

  patterns <- make_year_patterns()

  # Because this function is carried out on beheaded data, if there are e.g.
  # notes under the data in the year column, but nothing in the value columns
  # for those rows, these rows will be flagged as is_blank=TRUE. Where this is
  # the case it would be confusing to give a warning that the year is invalid.
  # We therefore flag both no_years and no_years_inc_blank so that we can
  # control when a warning is given.
  invalid_flagged  <- dat %>%
    mutate(matches = str_extract_all(year, patterns[as.character(year_type)])) %>%
    rowwise() %>%
    mutate(multi = length(matches) > 1,
           multi_not_blank = length(matches) > 1 & is_blank == FALSE,
           no_years = is.na(year_type),
           no_years_not_blank = is.na(year_type) & is_blank == FALSE) %>%
    ungroup() %>%
    select(-matches)

  year_NA_count <- sum(is.na(dat$year))
  none_count <- sum(invalid_flagged$no_years)
  none_not_blank_count <- sum(invalid_flagged$no_years_not_blank)
  multi_count <- sum(invalid_flagged$multi)
  multi_not_blank_count <- sum(invalid_flagged$multi_not_blank)
  invalid_count <-  sum(none_count) - year_NA_count
  multi_to_remove <- multi_count > 0 & year_to_na

  if (invalid_count > 0) {

    if (any(none_not_blank_count > 0, multi_not_blank_count > 0)) {
      warning(
        "Year is not valid for ", invalid_count, " entries: 'year' is set to ",
        "NA for these rows."
      )
    } else {
      message(
        "Setting year_type to NA where year is not valid. year_type will be NA ",
        "for rows with no valid years. All value rows have valid years."
      )
    }

    invalid_as_na <- invalid_flagged %>%
      mutate(
        year_notes = case_when(
          no_years == TRUE & is.na(year_notes) & !is.na(year) ~
            paste0("'", year, "' is invalid so removed from 'year'."),
          no_years == TRUE & !is.na(year_notes) & !is.na(year) ~
            paste0(
              year_notes, "| '", year, "' is invalid so removed from 'year'."
              ),
          .default = as.character(year_notes)
        ),
        year_type = case_when(
          no_years == TRUE ~ NA,
          .default = as.character(year_type)
        ),
        year = case_when(
          no_years == TRUE ~ NA,
          .default = as.character(year)
        )
      )
  } else {
    invalid_as_na <- invalid_flagged
  }

  if (multi_to_remove) {
    message(
      "Setting year to NA where it contains multiple years such as ",
      "'YYYY-YY to YYYY-YY'. This can be changed in the settings. The ",
      "original year for the row is recorded in the notes column."
    )

    invalid_as_na <- invalid_as_na %>%
      mutate(
        year_notes = case_when(
          multi == TRUE & is.na(year_notes) ~
            paste0("'", year, "' is multiple years so removed from 'year'."),
          multi == TRUE & !is.na(year_notes) ~
            paste0(
              year_notes, "| '", year,
              "' is multiple years so removed from 'year'."
            ),
          .default = as.character(year_notes)
        ),
        year = case_when(
          multi == TRUE ~ NA,
          .default = as.character(year)
        ),
        year_type = case_when(
          multi == TRUE ~ NA,
          .default = as.character(year_type)
        )
      )

  } else if (multi_count > 0) {

    if (multi_not_blank_count > 0) {
      message(
        "Setting year_type to NA where it contains multiple years such as ",
        "'YYYY-YY to YYYY-YY'. These multi-years can be removed by setting ",
        "multi_year_entries_as_na to true."
      )
    }

    invalid_as_na <- invalid_as_na %>%
      mutate(
        year_type = case_when(
          multi == TRUE ~ NA,
          .default = as.character(year_type)
        )
      )

  }

  if (invalid_count > 0 | multi_count > 0)  {
    output <- invalid_as_na %>%
      select(-c(multi, no_years, multi_not_blank, no_years_not_blank))

    return(output)

  } else {
    message("All years were valid.")
    return(dat)
  }

}
