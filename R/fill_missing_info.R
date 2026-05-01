#' @title Fill missing info in un-pivotted data
#'
#' @description Fill missing information in both rows and columns of
#' un-pivotted data (i.e. a standard data frame rather than xlsx_cells data).
#'
#' @details
#' This function includes some functions that deal with time periods
#' (split_date_to_columns, and split_year_and_vintage), but most of that is
#' done in add_time_period_columns.
#'
#' Whilst vintage is added as a column if vintage_with_year_col_pattern is
#'specified, it will not at this point be refined e.g 'forecast' will not be
#' converted to 'budget' by this function. To do so replace_string variables
#' need to be specified, and are called y functions later in tidy_sheet.
#'
#' For more details see documentation for rename_reserved_colnames, fill_blanks,
#' add_single_value_columns, add_totals_as_column, add_subtable_names,
#' add_row_headers_column, split_date_to_columns, split_year_and_vintage, and
#' add_metadata_columns.
#'
#' @param dat dataframe.
#' @param xlsx_cells_names character string vector. Column names of the source
#' data which will have been imported using xlsx_cells.
#' @param fill_columns_column_names charater string vector. The names of the
#' columns in which information above or below NA rows will be used to replace
#' the NAs.
#' @param fill_columns_fill_dirs character string vector. Must be the same
#' length as fill_columns_column_names Values allowed are "down", "up", "downup"
#' (i.e. first down and then up) or "updown" (first up and then down). If NA,
#' and fill_columns_column_names is not NA, it defaults to 'down'.
#' @param single_value_names vector of column names. Must
#' be the same length as single_value_values.
#' @param single_value_values vector of values. The order and number of
#' values must correspond to the order of single_value_names.
#' @param main_dropdown_name character_string. The name of the column you want to
#' populate with main_dropdown_value. In pub sec this variable is specified
#' using dropdown_name.
#' @param main_dropdown_value character_string. The value found in the dropdown
#' cell at the top of the sheet.
#' @param table_dropdown_name character_string. The name of the column you want
#' to populate with table_dropdown_value.
#' @param table_dropdown_value character_string. The value found in the dropdown
#' cell above the table.
#' @param name_for_total_column character string. The name of the column in
#' which to put the names of the groups that have a total.
#' @param col_with_totals_pattern character string. Regular expression matching
#' the name of the column currently containing the totals.
#' @param total_column_fill_dir character string. Default is 'up'. The direction
#' that names of the totals will be filled in. If totals are below the
#' information they sum, the direction should be 'up'. If totals are above the
#' items they sum, then fill_dir should be set to 'down' Options: "down", "up".
#' "downup" (i.e. first down and then up) or "updown" (first up and then down)
#' can theoretically also be used.
#' @param subtable_names dataframe with an integer column called 'row' and
#' a character column giving the subtitle found on that row. The name of this
#' character column is the name the column will have in the returned data. In
#' pub sec this is created by get_subtitles() using the settings
#' subtable_title_column, subtable_title_patterns, subtitle_offset, and
#' subtitle_horizontal_index
#' @param name_for_group_row_column character string. The name of the column to
#' put row header information in when putting row headers that other
#' descriptions are nested under into their own column.
#' @param name_for_nested_row_column character string. The name of the column
#' in which to put information that falls under row headers. Used when putting
#' row headers that other descriptions are nested under into their own column.
#' @param col_with_row_headers_pattern character string. Regular expression
#' matching the name of the column currently containing both the grouping
#' variable and the nested variable. Used when putting row headers that other
#' descriptions are nested under into their own column.
#' @param row_header_fill_dir string. The direction that headers will
#' be filled in. If header rows are above the information nested under them,
#' the direction should be 'down'. If header information is below the info
#' nested under it, then direction should be set to 'up' Options: "down", "up".
#' "downup" (i.e. first down and then up) or "updown" (first up and then down)
#' can also be used. If other row_headers settings are not NA, the default is
#' 'down'.
#' @param group_row_na_identifier string. Either the name of a column in dat
#' that contains NAs for rows containing the grouping variable, or 'all' (the
#' default), in which case grouping variable rows are identified by being
#' associated with an otherwise blank row.
#' @param POSIX_column character string. The exact name of the column containing
#' the date.
#' @param vintage_with_year_col_pattern character string. A single regular
#' expression to match a column containing both the year and the vintage. Year
#' can be financial or calendar. Accepted vintages are budget, provisional,
#' forecast, final, and actual (not case sensitive), though these could be
#' expanded upon.
#' @param title character string. The title of the sheet.
#' @param table_title character string. The title of the table.
#' @param units character string. The units of the table.
#' @param vintage character string. The vintage of the table (budget,
#' provisional, or final).
#' @param supplier character string. The data supplier, e.g. scottish_gov,
#' MHCLG etc.
#' @param source_group character string. The name of the source (the name of
#' the collection of data from a webpage) e.g. revenue_expediture_budget.
#' @param dataset character string. The name of the dataset.
#'
#' @returns dataframe with additions made.
#' @export
fill_missing_info <- function(
    dat, xlsx_cells_names, fill_columns_column_names, fill_columns_fill_dirs,
    single_value_names, single_value_values, main_dropdown_name,
    main_dropdown_value, table_dropdown_name, table_dropdown_value,
    name_for_total_column, col_with_totals_pattern,
    total_column_fill_dir, subtable_names, name_for_group_row_column,
    name_for_nested_row_column, col_with_row_headers_pattern,
    row_header_fill_dir, group_row_na_identifier, POSIX_column,
    vintage_with_year_col_pattern, title, table_title, units, vintage, supplier,
    source_group, dataset
    ) {

  reserved_names <- c(
    "title", "supplier", "source", "dataset", "value", "non_numeric_value"
  )
  names_protected <- rename_reserved_colnames(dat, reserved_names)

  blanks_filled <- fill_blanks(
    names_protected, fill_columns_column_names, fill_columns_fill_dirs
  )

  single_value_columns_added <- blanks_filled %>%
    add_single_value_columns(
      single_value_names, single_value_values, "single_value settings"
    ) %>%
    add_single_value_columns(
      main_dropdown_name, main_dropdown_value, "dropdown"
    ) %>%
    add_single_value_columns(
      table_dropdown_name, table_dropdown_value, "table dropdown"
    )

  totals_as_column <- add_totals_as_column(
    single_value_columns_added, xlsx_cells_names, name_for_total_column,
    col_with_totals_pattern, total_column_fill_dir
  )

  with_subtable_names <- add_subtable_names(totals_as_column, subtable_names)

  row_headers_as_column <- add_row_headers_column(
    with_subtable_names, xlsx_cells_names, name_for_group_row_column,
    name_for_nested_row_column, col_with_row_headers_pattern,
    row_header_fill_dir, group_row_na_identifier
  )

  date_split <- split_date_to_columns(row_headers_as_column, POSIX_column)

  vintage_split <- split_year_and_vintage(
    date_split, vintage_with_year_col_pattern
    )

  metadata_added <- add_metadata_columns(
    vintage_split, title, table_title, units, vintage, supplier,
    source_group, dataset
  )

  return(metadata_added)
  }

#' @title Vertically fill blanks in a descriptor column.
#'
#' @description Fill blanks within a column with the first populated entry
#' either above or below the blank.
#'
#' @details
#' This function is useful for data with layouts such as the following:
#' | __group__ | __subgroup__ | __value__ |
#' |----|----|----|
#' | cat | seval | 10 |
#' |     | margay | 2 |
#' |     | ocelot | 15 |
#' | dog | dhole | 1 |
#' |     | dingo | 15 |
#'
#' The group column may or may not have contained merged cells but each row in
#' group will be given a value if this function is used:
#' | __group__ | __subgroup__ | __value__ |
#' |----|----|----|
#' | cat | serval | 10 |
#' | cat  | margay | 2 |
#' | cat | ocelot | 15 |
#' | dog | dhole | 1 |
#' | dog | dingo | 15 |
#'
#' @md
#'
#' @param dat dataframe.
#' @param column_names charater string vector. The names of the columns to
#' apply the function to. In pub sec this variable is specified using
#' fill_columns_column_names.
#' @param fill_dirs character string vector. Must be the same length as
#' column_names. Values allowed are "down", "up", "downup" (i.e. first down and
#' then up) or "updown" (first up and then down). Defaults to 'down'. In pub sec
#' this variable is specified using fill_columns_fill_dirs.
#'
#' @returns dat with blanks filled.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'     var1 = c("colour", NA, NA, "wavelength"),
#'     var2 = c("red", "green", "blue", "short")
#'     )
#' fill_blanks(dat, "var1", "down")
#' }
#'
#' @export
fill_blanks <- function(dat, column_names, fill_dirs="down") {

  if (all(is.na(column_names), !is.na(fill_dirs))) {
    stop(
      "fill_columns_fill_dirs is specified but not fill_columns_column_names."
    )
  }

  if (all(is.na(column_names))) {
    return (dat)
  }

  if (all(is.na(fill_dirs))) {
    fill_dirs <- rep("down", length(column_names))
  }

  if (length(column_names) != length(fill_dirs)) {
    stop(
      "fill_columns_column_names must contain the same number of elements as ",
      "fill_columns_fill_dirs. Please amend the settings."
    )
  }

  if (!all(fill_dirs %in% c("down", "up", "downup", "updown"))) {
    stop(
      "fill_columns_fill_dirs must only contain the following: 'down', 'up', ",
      "'updown', 'downup' "
    )
  }

  message(
    "Filling blanks in the columns: '", paste0(column_names, collapse = "', '"),
    "'."
  )

  # If data were imported using xlsx_cells row will be in the data, and
  # ordering it is a safety device.
  if ("row" %in% names(dat)) {
    columns_filled <- arrange(dat, row)
  } else {
    columns_filled <- dat
  }

  for (i in 1:length(column_names)) {
    column <- column_names[i]
    fill_dir <- fill_dirs[i]

    columns_filled <- fill(columns_filled, !!sym(column), .direction = fill_dir)
  }

  return(columns_filled)

}


#' @title Add columns with unique values
#'
#' @description Add columns to the data that each have a single specified value.
#' If a column of the given name already exists the action will be skipped and
#' a warning given.
#'
#' @param dat dataframe to which single value columns will be added.
#' @param column_names vector of column names. Must
#' be the same length as single_value_values. In pub sec this variable
#' is specified by single_value_names.
#' @param column_fillers vector of values. The order and number of
#' values must correspond to the order of single_value_names. In pub sec
#' this variable is specified by single_value_values.
#' @param description character string. Default is NA. Used in messaging.
#'
#' @returns the dataframe with new columns added and filled. Returns a warning
#' if a column of the given name already exists. If only one of column_names or
#' column_fillers is missing an error is raised. If both column_names and
#' column_fillers are NA, dat is returned with no changes.
#'
#' @examples
#' single_value_names <- "description"
#' single_value_values <- "net current expenditure"
#' dat <- data.frame(a = 1:3, b = 4:6)
#' add_single_value_columns(
#' dat, single_value_names, single_value_values
#' )
#' @export
add_single_value_columns <- function(
    dat, column_names = NA, column_fillers = NA, description = NA) {

  # column_names and column_fillers must both exist for columns to be added.
  missing_var_count <- sum(
    is.na(column_names),
    is.na(column_fillers)
  )

  if (missing_var_count == 2) {
    return(dat)
  }

  if (! is.na(description)) {
    message("Creating a column for the value from the ", description)
  } else {
    message("Creating a column with a single value")
  }
  if (missing_var_count == 1) {
    stop("One of either the column to name or the value of the column is ",
         "missing. No columns added. Please contact a developer to add the ",
         "missing variable to the settings")
  }

  # column_names and column_fillers must be of the same length as each new
  # column can have a different value.
  if (length(column_names) != length(column_fillers)) {
    stop("Names and values must be the same length. ",
         "Please contact a developer to rectify this.")
  }

  message("Creating new columns with single unique values.")

  for (i in seq_along(column_names)) {

    column_name <- column_names[i]
    column_value <- column_fillers[i]

    if (column_name %in% names(dat)) {
      warning("Column '", column_name, "' ",
              "is specified, but it already contains values ",
              "in the data. The action to fill it with '", column_value,
              "' will be skipped.")
    } else {

      # If the column value is missing it is NA, not "NA" and will cause an
      # error to be raised at the start of the function, however "NA" will be
      # read as a string and can be used as a column filler.
      if (column_value == "NA") {
        dat[[column_name]] <- NA
      } else {
        dat[[column_name]] <- column_value
      }
      message(
        "New column created: '", column_name, "' filled with '", column_value,
        "'."
      )
    }
  }

  return(dat)
}


#' @title Give the name of the group that has totals in a new column
#'
#' @description If a descriptor column has items followed by the totals of those
#' items, this function can be used to move the name of the totalled group into
#' a separate column and associate it with the items above it.
#'
#' @details
#' For example:
#'
#' | __description__ | __value__ |
#' |---|---|
#' | red | 10 |
#' | blue | 15 |
#' | Total squares | 25 |
#' | red | 1 |
#' | green | 15 |
#' | total circles | 16 |
#'
#' becomes:
#' | __species__ | __group__ | __value__ |
#' |---|---|---|
#' | red | squares |10 |
#' | blue | squares | 15 |
#' | Total | squares | 25 |
#' | red | circles | 1 |
#' | green | circles |15 |
#' | total | circles | 16 |
#'
#' @md
#'
#' @param dat dataframe
#' @param xlsx_cells_names character string vector. The names of columns in
#' dataframes imported using xlsx_cells. These columns are not included when
#' searching for column names that match regular expressions.
#' @param totals_col character string. The name of the column in which to put
#' the names of the groups that have a total. In pub sec, this variable is
#' specified with name_for_total_column.
#' @param from_pattern character string. Regular expression matching the name of
#' the column currently containing both the totals. In pub sec this variable is
#' specified by col_with_totals_pattern
#' @param fill_dir string. Default is 'up'. The direction that names of the
#' totals will be filled in. If totals are below the information they sum,
#' the direction should be 'up'. If totals are above the items they sum,
#' then fill_dir should be set to 'down' Options: "down", "up".
#' "downup" (i.e. first down and then up) or "updown" (first up and then down)
#' can theoretically also be used.In pub_sec this variabble is specified with
#' total_column_fill_dir.
#'
#' @returns tibble. dat but with a new column for the group that are totalled.
#' If the row does not have a total associated with it, the new column contains
#' the same entry as that in the 'from' column.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'     item = c("A", "B", "total letts", "1.2.3", "total code", "total other"),
#'     numeric = c(1, 2, 3, 1, 1, 7)
#'     )
#'
#' add_totals_as_column(
#'     dat, xlsx_cells_names = "numeric",
#'     totals_col = "col1", from_pattern = "item",
#'     fill_dir = NA
#'     )
#' }
#' @export
add_totals_as_column <- function(
    dat, xlsx_cells_names, totals_col, from_pattern, fill_dir = "up"
) {

  if(all(is.na(totals_col), is.na(from_pattern))) {
    return(dat)
  } else if (any(is.na(totals_col), is.na(from_pattern))){
    stop(
      "At least one of name_for_totals_column, and col_with_totals_pattern ",
      "has not been supplied. If one is given, all others must also ",
      "be supplied in the settings."
    )
  }

  if (is.na(fill_dir)) {
    fill_dir <- "up"
  }

  message(
    "Creating column called '", totals_col,
    "' to hold the names of groups with totals."
  )

  possibilities <- setdiff(names(dat), xlsx_cells_names)
  from <- get_row_headers_colname(possibilities, from_pattern)

  newdat <- dat %>%
    # if 'total' is in the entry, assume this is the grouping variable,
    # and remove 'total' from the string
    mutate(!!sym(totals_col) := ifelse(
      str_detect(!!sym(from), "^(?i)total"),
      str_squish(str_remove(!!sym(from), "^(?i)total")),
      NA)
    ) %>%
    # Fill in the blanks
    fill(!!sym(totals_col), .direction = fill_dir) %>%
    # some items may not have any sub-items, for these we can assume that the
    # nested item and the group are the same
    mutate(!!sym(totals_col) := ifelse(
      is.na(!!sym(totals_col)),
      !!sym(from),
      !!sym(totals_col)
    ))

  return(newdat)
}


#' @title Get the column name of the column containing row headers/totals
#'
#' @description Get the column name that matches the provided pattern for
#' the column containing row headers. If there is no match, the first possible
#' column name is returned.
#'
#' @details The wording used in different releases of the same data may
#' change slightly. As exact column names are therefore unlikely to be known for
#' descriptor columns in the source data, a pattern is provided instead. This
#' function is specifcally for use in add_row_headers_column and
#' add_totals_as_column: if there is no match, the first possible column name
#' is returned. The messages are also specific to add_row_headers_pattern
#'
#' See get_matching_colnames and get_colnames_from_pattern for more general
#' functions with a similar purpose.
#'
#' @param possibilities character string vector. The names of possible columns.
#' This can be all column names, or a subset in which the match is expected.
#' @param pattern character string. A regular expression to match a column name.
#'
#' @returns The name of the column matching the pattern. If more than one match
#' is found, the first is returned with a warning. If none are found NA is
#' returned with a warning. If pattern is NA, NA is returned.
#'
#' @examples
#' \dontrun{
#' get_row_headers_colname(c("column 1", "column 2"), ".*2")
#' }
get_row_headers_colname <- function(possibilities, pattern) {

  if (is.na(pattern)) {
    return (NA)
  }

  matches <- possibilities[str_detect(possibilities, pattern)]

  if (length(matches) > 1) {
    column <- matches[1]
    warning(
      "More than one column was identified when looking for the column ",
      "containing row headers/totals using the pattern '", pattern, "'. ",
      "The first matching column name will be used by default: '", column,
      "'. If this causes issues downstream, please contact a developer to ",
      "edit the relevant pattern setting."
    )
  } else if (length(matches) == 0) {
    column <- possibilities[1]
    warning(
      "No column was identified when looking for the column containing row ",
      "headers/totals using the pattern '", pattern, "'. The first descriptor ",
      "column will be used by default: '", column, "'. If this causes ",
      "issues downstream, please contact a developer to edit the relevant ",
      "pattern setting."
    )
  } else {
    column <- matches
    message(
      "Column identified as: '", column, "'."
    )
  }

  return (column)
}


#' @title Put row headers that others are nested under into their own column.
#'
#' @description Create a column for grouping variables that are in the same
#' column as their nested items.
#'
#' @details
#' This function is for datasets where there are grouping variables as row names
#' that don't have a value associated with them because they are just the group
#' under which other items sit. Put these grouping items in a column in
#' their own right, and the nested items in another column.
#'
#' Turn data that look like this:
#'
#' |   __item__   | __value)__ |
#' |--------------|---------|
#' | Education    |         |
#' |    primary   |    1    |
#' |    secondary |    3    |
#' | Transport    |         |
#' |    roads     |    5    |
#'
#' Into this:
#'
#' | __item__   | __group__ | __value__ |
#' |--------------|------------|---------|
#' |    primary   | Education  |   1     |
#' |    secondary | Education  |   3     |
#' |    roads     | Transport  |   5     |
#' @md
#'
#' @param dat dataframe or tibble. Post being imported by xlsx_cells and
#' unpivotted. Must include a column called 'row'.
#' @param xlsx_cells_names character string vector. The names of columns in
#' dataframes imported using xlsx_sells. These columns are not included when
#' searching for column names that match regular expressions.
#' @param headers_col character string. The name of the column to put the row
#' header information in. In pub sec, this variable is specified with
#' name_for_group_row_column
#' @param nested_col character string. The name of the column in which to put
#' the information that falls under row header. In pub sec, this variable is
#' specified with name_for_nested_row_column.
#' @param from_pattern character string. Regular expression matching the name of
#' the column currently containing both the grouping variable and the nested
#' variable.
#' @param fill_dir string. Default is 'down'. The direction that headers will
#' be fillled in. If header rows are above the information nested under them,
#' the direction should be 'down'. If header information is below the info
#' nested under it, then dirction should be set to 'up' Options: "down", "up".
#' "downup" (i.e. first down and then up) or "updown" (first up and then down)
#' can also be used.
#' @param group_row_na_identifier string. Either the name of a column in dat
#' that contains NAs for rows containing the grouping variable, or 'all' (the
#' default), in which case  grouping variable rows are identified by being
#' associated with an otherwise  blank row.
#' @returns tibble. dat but with a new column for group_row
#'
#' @examples
#' \dontrun{
#' sample_data <- data.frame(
#'   address = c('B3', 'B4', 'B5', 'B6', 'B7'),
#'   row = c(3:7),
#'   col = 2,
#'   is_blank = c(TRUE, FALSE, FALSE, TRUE, FALSE),
#'   data_type = c('blank', rep('numeric', 2), 'blank', 'numeric'),
#'   numeric = c(NA, 1, 3, NA, 5),
#'   subservice = c('education', 'primary',  'secondary', 'Transport', 'roads')
#' )
#'
#' xlsx_cells_names <- c(
#'     "address", "row", "col", "is_blank", "data_type", "numeric"
#'     )
#' add_row_headers_column(
#'     dat = sample_data, xlsx_cells_names = xlsx_cells_names,
#'     headers_col = 'description_1', nested_col = 'description_2',
#'     from_pattern = '(?i)service', fill_dir = 'down', NA)
#' }
#' @export
add_row_headers_column <- function(
    dat, xlsx_cells_names, headers_col, nested_col, from_pattern,
    fill_dir = "down", group_row_na_identifier = "all"
) {

  if(all(is.na(headers_col), is.na(nested_col), is.na(from_pattern))) {
    return(dat)
  } else if (any(is.na(headers_col), is.na(nested_col), is.na(from_pattern))){
    stop(
      "At least one of name_for_group_row_column, name_for_nested_row_column",
      ", or col_with_row_headers_pattern has not been supplied. If one is ",
      "given, all others must also be supplied in the settings."
    )
  }

  if (is.na(group_row_na_identifier)) {
    group_row_na_identifier <- "all"
  }

  if (is.na(fill_dir)) {
    fill_dir <- "down"
  }

  message("Splitting the column containing row headers into two columns.")

  possibilities <- setdiff(names(dat), xlsx_cells_names)
  message("Getting the name of the column containing row headers.")
  from <- get_row_headers_colname(possibilities, from_pattern)

  if (headers_col == from & ! nested_col %in% names(dat)) {
    message("creating column '", nested_col, "' from '", from, "'.")
    dat <- dat %>%
      mutate(!!sym(nested_col) := !!sym(from))
  }

  if (group_row_na_identifier == "all") {

    message("Isolating rows in '", from, "' where all other columns are blank.")
    # get cases where there are ONLY blanks associated with the row
    not_groups <- dat %>%
      filter(is_blank == FALSE) %>%
      distinct(!!sym(from)) %>%
      pull()
    potential_groups <- dat %>%
      filter(is_blank == TRUE) %>%
      distinct(!!sym(from)) %>%
      pull()
    groups_array <- setdiff(potential_groups, not_groups)

    column_created <- dat %>%
      mutate(!!sym(headers_col) := ifelse(
        !!sym(from) %in% groups_array,
        !!sym(from), NA))

  } else if (group_row_na_identifier %in% names(dat)) {
    message(
      "Isolating rows in '", from, "' where", group_row_na_identifier,
      "is blank."
      )

    column_created <- dat %>%
      mutate(!!sym(headers_col) :=
               ifelse(is.na(!!sym(group_row_na_identifier)),
                      !!sym(from), NA))

  } else {
    stop(
      "group_row_na_identifier must be either 'all' or the name of a coumn in ",
      "the preprocessed data. Please contact a developer to fix this."
    )
  }

  # TODO:
  # in scot gov LFR_CR data some nested_row_1 items are incorrectly identified
  # as group_row items because one or more of the data columns is blank
  # FIX! (check if still relevant)

  message("Filling blanks in '", headers_col, "'.")
  row_headers_added_as_column <- column_created %>%
    # need to make sure data are in order of row so that fill works correctly
    arrange(row) %>%
    fill(!!sym(headers_col), .direction = fill_dir)

  if (! nested_col %in% names(row_headers_added_as_column)) {
    message("creating column '", nested_col, "' from '", from, "'.")
    row_headers_added_as_column <- row_headers_added_as_column %>%
      rename(!!sym(nested_col) := !!sym(from))
    }

  return(row_headers_added_as_column)
}


#' @title Add single value (metadata) variables as columns
#'
#' @description Include any variables given in dots (...) as a column in dat.
#' Each variable must be of length 1. The name of the column will be taken
#' as the name of the variable.
#'
#' @details
#' This function has a similar effect to add_single_value_columns, in that
#' it add columns to the data where the value of each column is the same on
#' every row. However, add_single_value_columns takes the column name and value
#' as separate vectors. add_single_value_columns can be controlled by user
#' settings. add_metadata_columns is not, so will add a column and fill it with
#' NA if there is no value for the variable in question.
#'
#' @param dat dataframe to which the ... params should be added as columns.
#' @param ... variable names, each of which contains a single value.
#'
#' @returns dat with additional columns
#'
#' @examples
#' \dontrun{
#' dat <- tibble(
#'     Year = c(2023, 2023),
#'     Value = c(1, 2),
#'     vintage = c("provisional", "final")
#' )
#' units <- c("count", "thousands")
#' title <- c("Counts by year")
#' vintage <- "final"
#'
#' # units will not be added as a column because it is of length >1,
#' # vintage will not be added because it already exists as a column
#' # title will be added:
#' add_metadata_columns(dat, title, units, vintage)
#' }
#' @export
add_metadata_columns <- function(dat, ...) {

  values <- list(...)
  col_names <- unlist(substitute(...()))

  accepted_values <- list()
  accepted_names <- list()

  failed_colnames <- c()
  failed_lengths <- c()

  for (i in 1:length(values)) {

    if (length(values[[i]]) > 1 ) {
      failed_colnames <- c(failed_colnames, as.character(col_names[[i]]))
      failed_lengths <- c(failed_lengths, length(values[[i]]))

    } else {
      accepted_values <- append(accepted_values, values[[i]])
      accepted_names <- append(accepted_names, col_names[i])
    }
  }

  if (length(failed_colnames) > 0) {
    warning(
      "Variables must be of length 1. The following column(s) have not been ",
      "added: '", paste0(failed_colnames, collapse = "', '"), "', as they are ",
      "of length(s): ", paste0(failed_lengths, collapse = ", "), "."
    )
  }

  accepted_values <- unlist(accepted_values)

  if(length(accepted_values) > 0) {
    for (i in 1:length(accepted_names)){

      if (as.character(accepted_names[[i]]) %in% names(dat) == FALSE) {
        message(
          "Adding '", accepted_names[[i]], "' with a value of '",
          accepted_values[i], "' as a column to the data."
        )
        dat <- mutate(dat, !!accepted_names[[i]] := accepted_values[i])

      } else {
        warning(
          accepted_names[[i]], " is already a column in the data so will not ",
          "be overwritten with information taken from outside the table."
        )
      }
    }
  }

  return (dat)
}


#' @title Separate year and vintage found in the same column into new columns
#'
#' @description In the rare case that vintage and year are given in the same
#' column, separate them out into separate columns called 'year' and 'vintage'
#' If those columns already exist, rename the originals so they don't get
#' overwitten.
#'
#' @param dat Dataframe.
#' @param pattern character string. A single regular expression to match the
#' column containing the year and the vintage. Year can be financial or
#' calendar. Accepted vintages are budget|provisional|forecast|final (not case
#' sensitive).
#'
#' @returns dat dataframe with new columns 'year' and 'vintage'. If either of
#' these columns already existed, the originals are renamed to '_year' and
#' '_vintage'
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'      year_and_vintage = c("2023 final", "2024 provisional", "2025 budget"),
#'      year = c("2023", NA, NA),
#'      vintage = "final"
#'      )
#' split_year_and_vintage(dat, "year.*vintage")
#' }
#' @export
split_year_and_vintage <- function(dat, pattern) {

  if (is.na(pattern)) {
    return(dat)
  }

  message("Splitting year and vintage into separate columns.")

  column <- get_matching_colnames(dat, pattern)

  if (length(column) == 0) {
    stop(
      "vintage_with_year_col_pattern is specified as '", pattern, "' ",
      "in the settings but no matching column has been found. ",
      "Year and vintage will not be split into separate columns. If this ",
      "causes issues please contact a developer."
    )
  } else if (length(column) > 1) {
    stop(
      "multiple matching column names have been found for ",
      "vintage_with_year_col_pattern in the sttings ('", pattern, "'). ",
      "Year and vintage will not be split into separate columns. If this ",
      "causes issues please contact a developer."
    )
  }

  if ("vintage" %in% names(dat)) {
    year_and_vintage <- rename(dat, `_vintage` = vintage)
    message(
      "Column 'vintage' already exists so will be renamed '_vintage' to ",
      "prevent it being overwritten"
      )
  } else {
    year_and_vintage <- dat
  }

  if ("year" %in% names(dat)) {
    year_and_vintage <- rename(year_and_vintage, `_year` = year)
    message(
      "Column 'year' already exists so will be renamed '_year' to ",
      "prevent it being overwritten"
    )
  }

  vintage_patterns <- "(?i)(budget|provisional|forecast|final|actual)"

  year_and_vintage <- year_and_vintage %>%
    get_year(from = column) %>%
    rowwise() %>%
    mutate(
      vintage = paste0(unlist(
        str_extract_all(str_to_lower(!!sym(column)), vintage_patterns)
      ), collapse = ", ")
    ) %>%
    ungroup()

  return(year_and_vintage)
}


#' @title Add subtable titles to the relevant rows of the data
#'
#' @description add subtable titles to all rows they apply to. It is assumed
#' that all rows require a subtable title entry and that the subtable title is
#' at the top of each subtable.
#'
#' @param dat dataframe with 'row' and 'col' columns.
#' @param subtable_names dataframe with an integer column called 'row' and
#' a character column giving the subtitle found on that row. The name of this
#' character column is the name the column will have in the returned data.
#'
#' @returns dataframe dat with subtable names added.
#'
#' @examples
#' \dontrun{
#'
#' subtable_names <- data.frame(
#'     row = c(1, 3),
#'     subtitle = c("table A", "table B")
#'     )
#' dat <- data.frame(row = 1:4, col = 2, value = 10:13)
#' add_subtable_names(dat, subtable_names)
#' }
add_subtable_names <- function(dat, subtable_names) {

  if (all(is.na(subtable_names))) {
    return(dat)
  }

  message("Adding subtable titles from within table.")

  subtitle_colname <- setdiff(names(subtable_names), "row")

  output <- dat %>%
    full_join(subtable_names, by = "row") %>%
    arrange(row, col) %>%
    fill(!!sym(subtitle_colname), .direction = "down")

  return (output)
}


#' @title Split a date formatted column into columns for day, month, and year
#'
#' @description Where a column can be unambiguously converted to a POSIXt
#' object, split the column out into day, month, and year columns. If it cannot
#' be converted, an error is raised.
#'
#' @details
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
