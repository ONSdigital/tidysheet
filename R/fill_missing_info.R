#' @title fill blanks in a descriptor column
#'
#' @description If data have row headers that are nested, with some being in
#' e.g. merged cells, this function can be used to fill the blanks with the
#' descriptors above or below.
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
#' a separate column and associate it with the items above it. For example:
#'
#' \strong{item} \cr
#' A \cr
#' B \cr
#' Total letters \cr
#' 1.2.3 \cr
#' Total code \cr
#'
#' becomes:
#' \tabular{ll}{
#' \strong{item} \tab  \strong{group} \cr
#' A \tab              letters \cr
#' B \tab              letters \cr
#' Total \tab          letters \cr
#' 1.2.3 \tab          code \cr
#' Total \tab          code \cr
#' }
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
#'
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
  from <- get_col_with_row_headers_name(possibilities, from_pattern)

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


#' @title Get the name of the column containing row headers
#'
#' @description If a column contains row headers, use the provided pattern
#' to get the name of that column.
#'
#' @param possibilities character string vector. The names of possible columns.
#' In pub sec this will be the names of the left headers.
#' @param pattern character string. A regular expression that matches the name of the
#' column with row headers. In pub sec this variable is specified with
#' col_with_row_headers_pattern.
#'
#' @returns The name of the column matching the pattern. If more than one match
#' is found, the first is returned with a warning. If none are found the first
#' of the possibilities is found with a warning. If pattern is NA, NA is
#' returned.
#'
#' @examples
#' \dontrun{
#' get_col_with_row_headers_name(c("column 1", "column 2"), "2")
#' }
get_col_with_row_headers_name <- function(possibilities, pattern) {

  if (is.na(pattern)) {
    return (NA)
  }

  matches <- possibilities[str_detect(possibilities, pattern)]

  if (length(matches) > 1) {
    column <- matches[1]
    warning(
      "More than one column was identified when looking for the column ",
      "containing row headers/ totals using the pattern '", pattern, "'. ",
      "The first matching column name will be used by default: '", column,
      "'. If this causes issues downstream, please contact a developer to ",
      "edit the relevant pattern setting."
    )
  } else if (length(matches) == 0) {
    column <- possibilities[1]
    warning(
      "No column was identified when looking for the column containing row ",
      "headers/ totals using the pattern '", pattern, "'. The first left ",
      "header column will be used by default: '", column, "'. If this causes ",
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
#' This function is for datasets where there are grouping variables as row names
#' that don't have a value associated with them because they are just the group
#' under which other items sit. Put these grouping items in a column in
#' their own right, and the nested items in another column.
#'
#' Turn data that look like this:
#'
#' |     from     | numeric |
#' |--------------|---------|
#' | Education    |         |
#' |    primary   |    1    |
#' |    secondary |    3    |
#' | Transport    |         |
#' |    roads     |    5    |
#'
#' Into this:
#'
#' |     from     | row_header | numeric |
#' |--------------|------------|---------|
#' |    primary   | Education  |   1     |
#' |    secondary | Education  |   3     |
#' |    roads     | Transport  |   5     |
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
  from <- get_col_with_row_headers_name(possibilities, from_pattern)

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
#' Each variable must be of length 1.
#'
#' This function has a similar effect to add_single_value_columns, in that
#' it add columns to the data where the value of each column is the same on
#' every row. However, add_single_value_columns takes the column name and value
#' as separate vectors. add_single_value_columns is more appropriate to use if
#' you only want more control over which datasets are given the column in
#' question (by using settings). add_metadata_columns is more appropriate if
#' you want all columns to be added to all datasets, even if they are NA.
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


#' @title Fill missing financial year start values.
#'
#' @description Fill in missing fy_start values for rows where year_type is
#' 'financial'.
#'
#' @details If a row has 'financial' as year_type and fy_start is missing (or
#' the column does not exist), extract the first four digits from the year
#' column and use this for fy_start. Rows with other year_type values are left
#' unchanged. This function is intended for use with datasets that contain
#' both calendar and financial years but could be used in other scenarios.
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
#'
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
#'
split_year_and_vintage <- function(dat, pattern) {

  if (is.na(pattern)) {
    return(dat)
  }

  message("Splitting year and vintage into separate columns.")

  column <- get_matching_colnames(dat, pattern)

  if (length(column) == 0) {
    stop(
      "vintage_with_year_column is specified as '", pattern, "' ",
      "in the settings but no matching column has been found. ",
      "Year and vintage will not be split into separate columns. If this ",
      "causes issues please contact a developer."
    )
  } else if (length(column) > 1) {
    stop(
      "multiple matching column names have been found for ",
      "vintage_with_year_column in the sttings ('", pattern, "'). ",
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
