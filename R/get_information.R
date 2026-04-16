#' @title Extract metadata from a dataframe and from other supplied information.
#'
#' @description Get units, title, dropdown cell contents, year, and vintage
#' values from metadata held above the data.
#'
#' @details
#' This is used in public sector to look for the relevant information at the top
#' of the sheet and above the subtables. Where there is conflicting title,
#' units, or vintage information between the sheet and subtable metadata, rules
#' are implemented by the called functions to select the correct one.
#'
#' Conflicts between the year found in sheet and subtable metadata are handled
#' outside this function (by get_year_for_use_in_data) as it is more complex.
#'
#' Title and units are simply extracted from the information above the data.
#' Vintage and dropdown values also use information supplied from the user
#' settings and other places (i.e. release number).
#'
#' Note on the dropdown cell value: In some DLUHC (MHCLG) datasets we have to
#' use the LA_dropdown tab. In these sheets there is a dropdown at the top that
#' controls what data are shown on the sheet, and therefore what data are
#' imported. If the wrong value is selected in the dropdown, users need to be
#' made aware of this, hence including it in the metadata.
#'
#' @param sheet_dat dataframe in xlsx_cells format, containing metadata from
#' the top of the sheet.
#' @param dropdown_pattern character string. regular expression to match the
#' expected contents of the dropdown cell. In pub sec this variable is specified
#' by dropdown_pattern.
#' @param single_vintage character string. Vintage as specified in the settings.
#' In pub sec this variable is specified by single_vintage.
#' @param release_number integer between 1 and 9 or NA.
#' @param table_dat dataframe in xlsx_cells format, containing metadata from
#' above the table (i.e. after the first header row, but before the table first
#' header row). Only relevant for datasets with multiple tables in a sheet.
#' @param sheet_title character string. The title at the top of the sheet.
#' @param sheet_units character string. Units taken from the top of the sheet.
#'
#' @returns named list giving character string values for 'units', 'dropdown'
#' 'title', 'year', and 'vintage'. 'year' can be a vector with more than one
#' element. All others have a single element.
#'
#' @examples
#' \dontrun{
#' sheet_dat <- data.frame(
#'     character = c(
#'         "All data for 2022-23 to 2026-27",
#'         "£ million unless otherwise stated",
#'         "England"
#'         ),
#'     address = c("A1", "A2", "A4"),
#'     row = c(1, 2, 4),
#'     col = 1
#'     )
#' table_dat <- data.frame(
#'     character = c("Data for 2024-25", "£ thousand"),
#'     address = "A40",
#'     row = 40,
#'     col = 1
#'     )
#' dropdown_pattern <- "(?i)Eng"
#' single_vintage <- "final"
#'
#' sheet_metadata <- get_metadata(
#'     sheet_dat, dropdown_pattern, single_vintage
#'     )
#' table_metadata <- get_metadata(
#'     sheet_dat, dropdown_pattern, single_vintage, NA, table_dat,
#'     sheet_metadata[['title']], sheet_metadata[['units']]
#'     )
#' }
#' @export
get_metadata <- function(
    sheet_dat, dropdown_pattern = NA, single_vintage = NA, release_number = NA,
    table_dat = NA, sheet_title = NA, sheet_units = NA
    ) {

  # Both the sheet and table metadata tables are required when getting vintage
  # for tables. However, for all other types of metadata only one metadata table
  # is used. We thus need to specify which is the focal metadata table (dat).
  if (all(!is.data.frame(table_dat), is.data.frame(sheet_dat))) {
    dat <- sheet_dat
  } else if (is.data.frame(table_dat)) {
    dat <- table_dat
  } else {
    stop("No data found.")
  }

  if (! all(c("row", "col", "character") %in% names(dat))) {
    stop(
      "Data must be imported using xlsx_cells and contain the following ",
      "columns: 'row', 'col', 'character'."
      )
  }
  current_units <- get_units(dat)

  dropdown <- get_dropdown_value(dat, dropdown_pattern)

  message("Getting title.")
  all_metadata <- dat$character
  current_title <- all_metadata[!is.na(all_metadata)][1]
  message("Title found: '", current_title, "'.")

  # If year is mentioned in the title ignore years mentioned in the rest of the
  # metadata, as the year in the title is more likely to be the year of the data
  year <- extract_all_years(current_title)
  blank_year <- any(all(is.na(year)), all(year == ""))
  if (blank_year) {
    year <- extract_all_years(dat)
  }

  # We only get vintage for metadata above a table, not from the top
  # of the sheet because it would give the same result and messages for both
  if (is.data.frame(table_dat)){
    vintage <- get_vintage(
      single_vintage, release_number, sheet_title, sheet_dat, current_title,
      table_dat
    )
    # If table units is not blank use that, otherwise use sheet, otherwise NA.
    units <- select_units(sheet_units, current_units)
  } else {
    vintage <- NA
    units <- current_units
  }

  metadata_list <- list(
    "units" = units, "dropdown" = dropdown, "title" = current_title,
    "year" = year, "vintage" = vintage
  )

  return(metadata_list)
}


#' @title Get the row number of the first header row.
#'
#' @description
#' Identify the row number of the first header in the dataset based on
#' a specified regular expression, desired instance of the match (1st, 2nd etc),
#' and the header row offset. 
#' 
#' @details
#' If the header row offset is not specified, the
#' header row is assumed to be the row on which the specified instance of the
#' match occurs. However if the matching cell is known to be found on e.g. the
#' row after the first header row, offset_by (as -1) can be used.
#'
#' If the specified instance or row is invalid, warnings are issued, and the
#' function defaults to the first valid instance.
#'
#' @param dat A data table of the Excel file, in xlsx_cells format.
#' @param pattern A string pattern to search for in the data to identify the
#'  header row. In pub sec this variable is specified with header_identifier.
#' @param instance An integer specifying which instance of the header identifier
#' pattern to use. Defaults to 1 (the first instance). In pub sec this variable
#' is specified with header_identifier_instance.
#' @param offset_by An integer specifying the number of rows to offset
#' the header row. Defaults to 0. In pub sec this variable is specified with
#' header_row_offset. If the row the pattern is found on is the row after the
#' first header row, offset_by will be 1. If the pattern is on the row before
#' the first header row, offset_by will be -1.
#'
#' @return The row index of the header based on the specified pattern and
#' instance. Returns `NA` if no match is found. Returns the number of the first
#' populated row if no pattern is provided.
#'
#' @examples
#' \dontrun{
#' |row | data title         |                   |
#' |----|--------------------|-------------------|
#' |2   | million            |                   |
#' |3   |                    |                   |
#' |4   | education services | highways services |
#' |5   | primary services   | roads             |
#' |6   |      50            |      10           |
#'
#' first header row in the above table is row 4,
#' first row of data is row 6
#'
#' dat <- data.frame("character" = c("title", "million", NA,
#'                                  "Education services", "Highways services",
#'                                  "primary services", "roads",
#'                                  NA, NA),
#'                   "row" = c(1:3, 4, 4, 5, 5, 6, 6),
#'                   "numeric" = c(rep(NA, times = 7), 50, 10))
#'
#' get_header_row(dat, "services", 1, 0, 1)
#' }
#' @md
#'@export
get_header_row  <- function(dat, pattern, instance = 1, offset_by = NA) {

  if (is.na(offset_by)) {
    offset_by <- 0
  } else {
    offset_by <- as.integer(offset_by)
  }

  if (is.na(instance)) {
    instance = 1
  } else {
    instance <- as.integer(instance)
  }

  if (is.na(pattern)) {
    message("First header row is set as the first populated row of the table.")

    first_populated <- dat %>%
      filter(is_blank == FALSE) %>%
      filter(row >= min(row))

    return(min(first_populated$row))
  }

  # Get the instances of the header identifier
  header_row <- find_all_instances(dat, pattern)

  # Check if the header identifier was found
  if (length(header_row) == 0) {
    stop(
      "No header identifier found in the data. Please check the pattern: ",
      pattern
    )
  }

  header_row_number <- get_loc_from_instance(header_row, instance, offset_by)

  message(
    "The first header row has been identified as row ", header_row_number
  )

  # Return the header row number
  return(header_row_number)
}


#' @title Identify the first row or column on which a string occurs
#'
#' @description Takes an xlsx_cells dataframe and a string known to be
#' in the row or column you wish to identify but not in any previous rows/cols.
#' Returns the row/col number from the original excel sheet.
#' This function is not case sensitive - it will for example identify
#' pattern = "Blue" as a match with "blue" in the dataframe.
#'
#' @param dat  An xlsx_cells dataframe
#' @param pattern string
#' @param direction "row" or "col". Default is row
#'
#' @return numeric. The first row/col from the xlsx on which known_string
#'        occurs.Defaults to 1 if dat is not an xlsx_cells df.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame("character" = c("title", "million", NA,
#'                                  "Education services", "Highways services",
#'                                  NA),
#'                   "row" = c(1:3, 4, 4, 5),
#'                   "numeric" = c(rep(NA, times = 5), 50))
#' get_first_instance(dat, "services")
#'}
#' @export
get_first_instance <- function(dat, pattern, direction = "row") {

  message(
    "finding the first location that matches pattern '", pattern, "'."
  )
  locations <- find_all_instances(dat, pattern, direction)

  if (length(locations) > 0) {
    message(
      length(locations), " matches found for pattern '", pattern, "'. ",
      "Returning the first location: ", min(locations)
    )
    return(min(locations))

  } else {
    stop("No matches found for pattern: '", pattern, "'.")
  }

}


#' @title Find all rows or columns where a pattern occurs
#'
#' @description Take an xlsx_cells dataframe and a pattern known to be
#' in the character cells of the rows or columns you wish to identify. Note that
#' numeric and date cells are ignored even if pattern is a number.
#'
#' @param dat  An xlsx_cells dataframe
#' @param pattern character vector of length 1. Regular expression to be matched.
#' @param direction "row" or "col". Default is row. If col, the column numbers in
#' which the pattern occurs is returned.
#'
#' @return numeric vector. A list of unique rows/cols from the xlsx where the
#'         pattern occurs.
#'         Returns an empty vector if no matches are found.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame("character" = c("title", "million", NA,
#'                                  "Education services", "Highways services",
#'                                  NA),
#'                   "row" = c(1:3, 4, 4, 5),
#'                   "numeric" = c(rep(NA, times = 5), 50))
#' find_all_instances(dat, "services")
#'}
find_all_instances <- function(dat, pattern, direction = "row") {

  if (all(c("character", direction) %in% colnames(dat)) == FALSE) {
    stop(
      "Data must be imported using xlsx_cells and contain columns named ",
      "'character' and ",
      direction
    )
  }

  tryCatch(
    {
      locations <- grep(
        pattern,
        dat$character
      )

      if (length(locations) > 0) {
        # Extract all matching rows/columns and retain only unique values.
        # locations is just an index, not the actual row/column number.
        # Because xlsx_cells dfs have a row per cell, there are many rows for
        # each row in the original dataframe. So, unlike with other ways of
        # reading a dataframe, first_row_location does not equal row number.
        # To get that we need this next line that specifies whether to return
        # row or col
        all_instances <- unique(dat[locations, ][[direction]])

        return(all_instances)

      } else {
        warning(
          "Failed to find the pattern '", pattern, "' in the source data. ",
          "Please notify a developer as changes in the data may have occurred ",
          "since the last publication that require updates to sheet settings."
        )
        return(numeric(0))  # Return an empty numeric vector if no matches
      }

    },
    error=function(e) {
      message(
        "An error occurred in find_all_instances. Please contact a developer."
      )
      print(e)
    }
  )
}


#' @title Get row or column number from a vector of possibilities
#'
#' @description Given a vector of row or column numbers in which a match to a
#' given pattern was found, use a provided index (instance) and/ or an offset
#' value to find the desired row or column number.
#'
#' @param possibilities integer vector. The row or column numbers on which a
#' pattern match was found. These must be in numeric order.
#' @param instance integer. The index of the intended pattern match.
#' @param offset_by integer. The number to subtract from the intended number in
#' possibilities.
#'
#' @returns integer.
#'
#' @examples
#' \dontrun{
#' get_loc_from_instance(c(3, 6, 9), 2, -1)
#' }
get_loc_from_instance <- function(possibilities, instance, offset_by) {

  if (length(instance) > 1) {
    stop("instance (c(", paste0(instance, collapse = ", "), ")) must be a ",
    "single value.")
  }
  if (instance > length(possibilities)) {
    warning(
      "The specified instance (", instance,
      ") exceeds the number of instances found (", length(possibilities), "). ",
      "Returning the first instance instead."
    )
    instance <- 1
  }
  if (instance < 1) {
    stop(
      "instance (", instance, ") must be positive. Check that instance and ",
      "offset_by are correct "
    )
  }
  if (length(offset_by) > 1) {
    stop("offset (c(", paste0(offset_by, collapse = ", "), ")) must be a ",
    "single value.")
  }

  # Get the row/column number of the specified instance
  number <- possibilities[instance]

  # Check if the header row number is valid
  if (offset_by != 0) {
    number <- number - offset_by
  }
  return(number)
}


#' @title Get column names that match regular expressions
#'
#' @description For each pattern supplied, the column name that matches is 
#' returned as a value in a named list. The new name for the column is given
#' as the name in the list (taken from 'identitites').
#'
#' @param dat dataframe containing the column names to find.
#' @param identities vector of character strings. Each is a name that will be
#' given to the column whose name matches the relevant pattern.
#' @param patterns vector of character strings. Each is a regular expression
#' that is used to identify the column to rename. Must be given in the same
#' order as columns.
#'
#' @returns named vector - names are from identities, values are the names of
#' the columns that matched the pattern. If the number of matches is not 1 a
#' warning is given and NA is returned for that item.
#'
#' @examples
#' dat <- data.frame(
#'   Year = "2010",
#'   Value = 1,
#'   `Item name` = "foo"
#' )
#'
#' colnames <- get_colnames_from_pattern(
#'     dat, c("year", "item"), c("(?i)year", "(?i)item")
#'     )
#' @export
get_colnames_from_pattern <- function(dat, identities, patterns) {

  if (length(patterns) != length(identities)) {
    stop(
      "Different lengths for columns and patterns. They must contain the same ",
      "number of elements."
    )
  }

  ids <- c()
  column_names <- c()

  for (i in 1:length(patterns)) {

    if (is.na(patterns[i])) {
      ids <- c(ids, identities[i])
      column_names <- c(column_names, NA)
      next
    }

    col_index <- grep(patterns[i], names(dat))

    if (length(col_index) == 0) {

      ids <- c(ids, identities[i])
      column_names <- c(column_names, NA)
      warning("No column names match pattern: '", patterns[i], "'. ")

    } else if (length(col_index) > 1) {

      ids <- c(ids, identities[i])
      column_names <- c(column_names, NA)
      warning(
        "Multiple column names match pattern :'", patterns[i], "' ('",
        paste(names(dat)[col_index], collapse = "', '"),
        "')."

      )

    } else {

      column_name <- names(dat)[col_index]

      column_names <- c(column_names, column_name)
      ids <- c(ids, identities[i])
    }

  }
  names(column_names) <- ids

  return(column_names)
}


#' @title Get column names that contain a pattern
#'
#' @description Return all column names from a dataset that match a regular
#' expression.
#' 
#' @details See get_colnames_from_pattern for a similar function. 
#'
#' @param dat dataframe
#' @param pattern string. Regular expression you want to match
#' @returns vector of character strings. Names of the columns that match the
#' given pattern. If no names are found a character vector of length 0 is
#' returned.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(fin_year_end = "2020", "Year" = ("2021"), "other" = NA)
#' get_matching_colnames(dat, "(?i)year")
#' }
#' @export
get_matching_colnames <- function(dat, pattern){

  matched_names <- grepl(pattern, names(dat))
  col_names <- names(dat)[matched_names]

  return(col_names)
}


#' @title Get unique column letters from col number in an xlsx_cells dataset.
#'
#' @description Use cell addresses in an xlsx_cells dataframe to get the Excel
#' column letters for a string of comma-separated column  numbers.
#' Required for messages to users - users are used to
#' looking up a  column in an Excel file based on letters not numbers.
#'
#' @details
#' For alternative code that gets excel column letters from column number 
#' without an xlsx_cells dataframe for lookup, see
#' https://stackoverflow.com/questions/9905533/convert-excel-column-alphabet-e-g-aa-to-number-e-g-25
#'
#' @param dat Dataframe. Imported using xlsx_cells
#' @param col_numbers vector of integers
#'
#' @returns character string of comma separated column letters.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   "row" = c(1, 1, 1, 2, 2, 2),
#'   "col" = c(1, 2, 27, 1, 2, 27),
#'   "address" = c("A1", "B1", "AA1", "A2", "B2", "AA2")
#' )
#'
#' get_col_letters_as_string(dat, c(1, 2))
#' }
get_col_letters_as_string <- function (dat, col_numbers) {

  dat %>%
    filter(col %in% col_numbers) %>%
    mutate(col_letter = str_extract(address, "[A-Z]*")) %>%
    distinct(col_letter) %>%
    pull(col_letter) %>%
    unlist() %>%
    paste0(collapse = ", ")

}


#' @title Select a single units value
#'
#' @description Choose between units taken from the top of the sheet, and units
#' taken from above the subtable. Units taken from above the subtable take
#' precendence. If neither are available, return NA.
#'
#' @param sheet_units character string. Units taken from the top of the sheet.
#' @param table_units character string. Units taken from above the subtable.
#'
#' @returns character string. Units from above the subtable unless they are NA,
#' in which case units from the top of the sheet.
#'
#' @examples
#' \dontrun{
#' select_units("millions", NA)
#' select_units("millions", "thousands")
#' select_units(NA, "thousands")
#' }
select_units <- function(sheet_units, table_units) {

  if (!is.na(table_units) & table_units != "") {
    message("Units taken from above the table: '", table_units, "'.")
    return(table_units)
  } else if (!is.na(sheet_units) & sheet_units != "") {
    message("Units taken from above the table: '", sheet_units, "'.")
    return(sheet_units)
  } else {
    message("No units found")
    return(NA)
  }

}


#' @title Extract information about units
#'
#' @description Extract unit labels such as 'thousand', 'million', '000s', or
#' '£ Millions' from the character column of data imported using
#' tidyxl::xlsx_cells().
#'
#' @param dat dataframe imported using tidyxl::xlsx_cells() containing the cells
#' that are likely to include information about units.
#'
#' @returns A string containing extracted unique unit labels separated by
#' commas, or an empty string if no units are found.
#'
#' @examples
#' \dontrun{
#' info_at_top_of_sheet <- data.frame(
#'     character = c(
#'         "Revenue in £ millions (millions)",
#'         "All figures are in thousands of pounds (£ thousand)."
#'         )
#'     )
#'
#' units <- get_units(info_at_top_of_sheet)
#' }
#' @export
get_units <- function(dat) {

  message("Getting units")
  pattern <- "((?i)thousand|(?i)000s)|(?i)million|(?i)(\\s|\\(|^)(count)(\\s|\\)|$)"
  units_extracted <- extract_matches(dat$character, pattern)

  # remove repeats within a single entry e.g. 'thousand (£ thousand)'
  # should only return 'thousand'
  if (is.list(units_extracted)) {
    units <- paste(
      unique(
        unlist(units_extracted[!is.na(units_extracted)])
      ),
      collapse = ', '
    )
  } else {
    units <- paste(
      unique(units_extracted[!is.na(units_extracted)]),
      collapse = ', '
    )
  }

  if (units == "") { units <- NA }

  if(is.na(units)) {
    message("No units found.")
  } else {
    message("Units found: ", units)
  }

  return(units)
}


#' @title Extract patterns from a vector of strings
#'
#' @description Look for a pattern and return the parts of the strings that
#' matched as a list. Converts numeric input to strings.
#'
#' @param dat  vector of numeric or string elements.
#' @param pattern character string. A single regular expression.
#'
#' @returns List of character strings. Each item of the input is returned in
#' it's own vector, with each match given as an item in that vector. The output
#' is therefore a list of vectors. NA is returned for elements of the input that
#' do not contain a match to the pattern.
#'
#' @examples
#' numbers <- extract_matches(c( "a2", "2 and 3", "none"), '[0-9]')
#'
#' @export
extract_matches <- function(dat, pattern) {

  ifelse(stringr::str_detect(dat, pattern),
         stringr::str_extract_all(dat, pattern), NA)

}


#' @title Get the value from a dropdown cell above the data.
#'
#' @description Get the value of the last cell found to match a specified
#' regular expression.
#'
#' @details
#' This function was created for use in public sector to get information from a
#' dropdown cell by using a regular expression to identify the contents of the
#' dropdown. An assumption is made that the dropdown cell is closer to the table
#' than any other information above the table.
#'
#' @param dat dataframe imported using xlsx_cells. Contains metadata.
#' @param pattern character string regular expression to match the expected
#' contents of the dropdown cell. In pub sec this variable is specified by
#' dropdown_pattern.
#'
#' @returns dataframe or tibble. dat is returned with the newly created column.
#' If only one of new_column or pattern is NA, an error is raised. If both
#' new_column and pattern are NA, dat is returned with no changes.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'     address = c("A1", "A2"),
#'     row = 1,
#'     col = 1:2,
#'     character = c("Total data for England",
#'     "England (adjusted, excluding double counting)")
#'     )
#'
#' dropdown_value <- get_dropdown_value(dat, "England")
#' }
#' @export
get_dropdown_value <- function(dat, pattern=NA) {

  if (is.na(pattern)) {
    return(NA)
  }

  message("Getting info from the dropdown cell.")

  # In case the data have been misordered, it is best to use the row and col
  # columns that are in xlsx_cells dataframes to make sure we select the last
  # occurrence of the match.
  dropdown_entry <- dat %>%
    filter(str_detect(character, pattern)) %>%
    arrange(row, col) %>%
    slice_tail(n = 1) %>%
    pull(character)

  if (length(dropdown_entry) > 0) {
    message("Value found: '", dropdown_entry, "'.")
  } else {
    message(
      "No dropdown value found to match the dropdown pattern supplied in ",
      "the settings."
    )
  }

  return(dropdown_entry)

}


#' @title Get the release number from an xlsx_cells dataframe
#'
#' @description Get the release number (first, 1st, second, 2nd etc) from the
#' character column of a dataframe imported using tidyxl::xlsx_cells. Written
#' for use with DLUHC (MHCLG) Revenue expenditure provisional and final data.
#'
#' @param dat character string vector.
#'
#' @return integer. Version number between 1 and 9. If multiple possibilities
#' are found the highest number is returned with a warning. If none are found,
#' or id dat is NULL NA is returned with a warning.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'     character = c(
#'        "title - First release",
#'        "This is release 1 not release 2",
#'        "next will be release 25"
#'      ))
#'  get_release_number(dat)
#' }
#' @export
get_release_number <- function(dat) {

  if (is.null(dat)) {
    return(NA)
  }

  message("Getting release number.")

  info <- dat[!is.na(dat)]

  if (length(info) == 0) {
    stop("No information found in the front page.")
  }

  # Assume that the release number will never go above 7. This should help
  # reduce the risk of non-release numbers being returned. It is set as a
  # variable here so if this decision changes it will change throughout the
  # function.
  number_pattern <- "release\\s*[1-7]"
  # Using the first three letters slightly reduces the risk of typos causing
  # the release number to not be found.
  number_text <- c(
    "first\\s*release", "second\\s*release", "third\\s*release",
    "fourth\\s*release", "fifth\\s*release", "sixth\\s*release",
    "seventh\\s*release"
  )
  number_text_pattern <- paste0(number_text, collapse = "|")
  all_number_patterns <- paste0(number_text_pattern, "|", number_pattern)

  # Narrow down the text to the small sections likely to contain the
  # release number so we don't pick up numbers related to other things.
  pattern <- paste0(
    "(?i)", all_number_patterns, collapse = ""
  )
  matches <- unlist(
    str_extract_all(info, pattern)
  )

  if (length(matches[!is.na(matches)]) == 0) {
    warning(
      "No release number found. Please contact a developer to fix the pattern."
    )
    return(NA)
  }

  release_matches <- unique(matches)

  # If release number is given as a string e.g 'first', 'second' etc, we
  # need to convert it to a number, but numbers can just be left as they are
  number_lookup <- data.frame(
    pattern = paste0("(?i)", number_text),
    number = 1:length(number_text)
  )

  release_numbers <- c()

  for (item in release_matches) {
    text_number_matches_item <- str_detect(item, number_lookup$pattern)
    number_matches_item <- str_detect(item, "[1-7]")

    if (any(text_number_matches_item)) {
      number <- number_lookup$number[text_number_matches_item]
      release_numbers <- c(release_numbers, number)
    } else if (number_matches_item) {
      number <- str_extract(item, "[1-7]")
      release_numbers <- c(release_numbers, number)
    }

  }

  possibilities <- unique(release_numbers)

  if (length(possibilities) > 1) {
    release <- max(possibilities)
    warning(
      "Multiple potential release numbers found (",
      paste0(possibilities, collapse = ", "),
      "). Assuming release number is ", release
    )
  } else {
    release <- possibilities
    message("Release number found to be: ", release)
  }

  return(release)
}


#' @title Record subtable title and it's location
#'
#' @description Find and record character strings and their row numbers. 
#' This function is written for locating subtitles but could be used for other
#' cases.
#' 
#' @details A set of regular expression patterns is used to locate a row. This
#' row can be the row of the cell containing the subtitle, or a row that is a 
#' known distance from the subtitle. The row that you want to locate must have 
#' matches to _all_ provided patterns. Once that row number 
#' is identified, the row containing the subtitles can be identified through
#' its relative position to the found row. If it is known to be two rows above,
#' use offset = -2. 
#' 
#' Ideally, the patterns provided will match the subtable title
#' rows, in which case offset is 0 and does not need to be specified. Once the
#' target row is identified, if there is more than one character cell in that
#' row, the index of the required cell can be specified using horizontal_index.
#'
#' @param dat dataframe imported using xlsx_cells and not yet pivotted i.e. all
#' character strings are still in the character column.
#' @param column character string. The name of the new column to store the
#' subtable title. In pub sec this variable is specified with
#' subtable_title_column.
#' @param patterns character string vector. Any number of regular expressions.
#' The row number that is identified will be the one(s) that have a match to
#' EVERY pattern provided. In pub sec this variable is specified with
#' subtable_title_patterns.
#' @param row_offset integer. The number to add to the row that matches
#' patterns to get the row number on which the subtitle is found, e.g if the
#' row below the found pattern contains the subtitle, use 1. if the
#' row above the found pattern contains the subtitle, use -1.
#' @param horizontal_index character string. An index number where 1 is the first
#' populated cell, and -1 is the last.
#'
#' @returns dataframe with a column for row and a a column named using the
#' 'column' arg containing the subtitles.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'     row = rep(c(1:5), each = 3),
#'     col = rep(c(1:3), times = 5),
#'     character =
#'         c("million", NA, "table A",
#'           NA, "col A", "col B",
#'           "c", NA, NA,
#'           NA, NA, "table B",
#'           NA, "col A", "col B"
#'           ),
#'     numeric = c(rep(NA, 7), 1, 2, rep(NA, 6)),
#'     is_blank = c(FALSE, TRUE, FALSE, TRUE, rep(FALSE, 5),
#'                  rep(c(TRUE, TRUE, FALSE), 2))
#'     )
#' get_subtitles(dat, "subtitle", c("(?i)a", "(?i)col.*b"), -1, -1)
#' }
#' @export
get_subtitles <- function(
    dat, column, patterns, row_offset, horizontal_index
    ) {

  if (all(
    is.na(column), is.na(patterns), is.na(row_offset), is.na(horizontal_index)
    )) {
    return(NA)
  }

  if ( any(is.na(column), all(is.na(patterns))) ) {
    stop(
      "One of subtable_title_column or subtable_title_patterns is supplied ",
      "but not the other. If one is supplied they both must be.")
  }

  message("Getting subtable titles from within table.")

  if (is.na(row_offset)) {
    row_offset <- 0
  }
  if (is.na(horizontal_index)) {
    horizontal_index <- 1
  }

  cell_check <- dat

  rows <- dat %>%
    distinct(row) %>%
    mutate(pattern_count = 0)

  for (i in 1:length(patterns)) {

    cell_check <- cell_check %>%
      mutate(
        cell_match = case_when(
          str_detect(character, patterns[i]) ~ TRUE,
          .default = FALSE
        )
      )

    # get the row numbers where a match was found in any of the cells
    row_check <- cell_check %>%
      group_by(row) %>%
      summarise(match = sum(cell_match) > 0)

    # If a pattern is found in the row add to the count of matched patterns for
    # that row.
    rows <- rows %>%
      left_join(row_check, by = "row") %>%
      mutate(
        pattern_count = case_when(
          match == TRUE ~ pattern_count + 1,
          .default = pattern_count
        )
      ) %>%
      select(-match) %>%
      ungroup()
  }

  if (! any(rows$pattern_count == length(patterns)) ) {
    stop(
      "No rows in the data contain all the patterns for identifying subtable ",
      "titles held within the table itself. Please contact a developer to ",
      "update the settings."
      )
  }

  subtitle_rows <- rows %>%
    filter(pattern_count == length(patterns)) %>%
    mutate(rows_with_subtitles = as.integer(row) + as.integer(row_offset)) %>%
    pull(rows_with_subtitles)

  # We now have all the non-blank cells in the rows that contain subtitles,
  # but we need to identify the exact cell (i.e. the column as well as the row).
  # The user can specify the index for this as a positive or negative number.
  populated_cells <- dat %>%
    filter(row %in% subtitle_rows) %>%
    filter(is_blank == FALSE) %>%
    group_by(row) %>%
    mutate(row_num = row_number(),
           max_num = max(row_num))

  if (horizontal_index > 0) {
    subtitles <- populated_cells %>%
      filter(row_num == horizontal_index)
  } else if (horizontal_index < 0) {
    subtitles <- populated_cells %>%
      mutate(reverse_index = max_num + horizontal_index + 1) %>%
      filter(row_num == reverse_index)
  }

  output <- subtitles %>%
    mutate(!!sym(column) := character) %>%
    ungroup() %>%
    select(c(row, !!sym(column)))

  return (output)
}
