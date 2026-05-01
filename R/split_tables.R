#' @title Split xlsx_cells data into a list of tables
#'
#' @description Split a dataframe imported using tidyxl::xlsx_cells into a
#' list of tables. 
#' 
#' @details
#' The dataframe can be split horizontally or vertically or
#' any combination of those. Splits are done iteratively, with each split
#' resulting in two new tables. The number of times the table is split is
#' equal to the number of elements passed in the variables.
#'
#' @param dat dataframe imported using tidyxl::xlsx_tables()
#' @param directions character string vector the same length as the number of
#' splits required. Must only contain 'col' and 'row'. In pub sec this variable
#' is specified using table_split_dirs.
#' @param table_ids integer vector. The full table is id 1. Table 1 will be
#' split into table 2 and 3. If split direction is col, table 2 is to the left,
#' table 3 to the right. If split direction is row, table 2 is at the top, table
#' 3 below. Length of vector is equal to the number of splits required. In pub
#' sec this variable is specified using tables_to_split.
#' @param patterns character string vector. Regular expression patterns that
#' match an entry in the first column or row of where the table in question will
#' be split.  In pub sec this variable is specified using split_patterns.
#' @param pattern_instances character or integer vector. If characters they must
#' be convertible to integers. If the split location is at the second occurrence
#' of the pattern, the pattern_instance would be 2. Optional, but if specified
#' for any split, it must be specified for all.
#' @param pattern_offsets character or integer vector. If characters they must
#' be convertible to integers. If the split location is the row or column after
#' the occurrence of the pattern, the pattern_offset would be -1. If it is the
#' row/column before it would be 1. Optional, but if specified for any split,
#' it must be specified for all.
#' @param keep integer vector. The IDs of the tables to keep. Default is NA, in
#' which case all tables are kept. In pub sec this variable is specified by
#' tables_to_process.
#'
#' @returns dat split into a list of tables.
#'
#' @examples
#' \dontrun{
#' # 3 tables - one above the other on the left, and an overlapping one on the
#' # right.
#' # The first split has to be vertical (as table c overlaps the two tables
#' # stacked to the left of it), so the first complete table is table c.
#' # The second split is horizontal, and tables are returned top to bottom so
#' # the second table is table a and the third is table b.
#'
#' dat <- data.frame(
#'     row = c(1, 2, 2, 3, 3, 4, 4, 4, 5, 5, 6, 6, 6, 7, 7, 8, 8),
#'     col = c(1, 1, 2, 1, 2, 1, 2, 3, 3, 4, 1, 3, 4, 1, 2, 1, 2),
#'     character = c(
#'         "table a",
#'         "x", "value",
#'         "a1", NA,
#'         "a2", NA, "table c",
#'         "y", "value",
#'         "table b", "c1", NA,
#'         "z", "value",
#'         "b1", NA
#'         ),
#'     numeric = c(
#'         rep(NA, 4), 1, NA, 1, rep(NA, 5), 3, rep(NA, 3), 2
#'         )
#'     )
#' get_tables_as_list(dat, c("col", "row"), c(1, 2), c("table c", "table b"))
#' }
#' @export
get_tables_as_list <- function(
    dat, directions, table_ids, patterns, pattern_instances, pattern_offsets,
    keep = NA
    ){

  if (all(
    is.na(directions), is.na(table_ids), is.na(patterns), is.na(keep),
    is.na(pattern_instances), is.na(pattern_offsets)
    )) {
    return(list(dat))
  }

  message("Splitting tables into subtables.")

  if (any(is.na(directions), is.na(table_ids), is.na(patterns))) {
    stop(
      "At least one, but not all of `directions` (table_split_dirs), ",
      "`table_ids` (tables_to_split), `patterns` (table_split_patterns), and ",
      "`keep` (tables_to_process) has been specified.  If one is specified, ",
      "they all must be."
    )
  }

  if (length(directions) != length(table_ids) |
    length(table_ids) != length(patterns)) {
    stop(
      "table_ids (tables_to_split), directions (table_split_dirs), and ",
      "patterns (table_split_patterns) must have an equal number of elements"
    )
  }

  if (all(is.na(pattern_instances))) {
    pattern_instances <- rep(1, length(patterns))
  }
  if (any(
    is.na(pattern_instances) | length(pattern_instances) != length(patterns)
    )) {
    stop(
      "The same number of integers must be given for table_split_patterns and ",
      "table_split_pattern_instances."
      )
  }

  if (all(is.na(pattern_offsets))) {
    pattern_offsets <- rep(0, length(patterns))
  }
  if (any(
    is.na(pattern_offsets) | length(pattern_offsets) != length(patterns)
  )) {
    stop(
      "The same number of integers must be given for table_split_patterns and ",
      "table_split_pattern_offsets."
    )
  }

  for (i in 1:length(directions)) {

    split_direction <- directions[i]
    table_id <- table_ids[i]
    split_pattern <- patterns[i]
    pattern_instance <- pattern_instances[i]
    pattern_offset <- pattern_offsets[i]

    if (i == 1) {
      tables <- list(dat)
    }
    tables <- split_tables(
      tables, split_pattern, pattern_instance, pattern_offset, split_direction,
      table_id
      )
  }
  # remove the tables that were split into smaller sections so no row is
  # given in more than one table
  used_tables_removed <- remove_used_tables(tables, table_ids, keep)
  return(used_tables_removed)
}

#' @title split an xlsx_cells dataframe into a list of two dataframes
#'
#' @description Split one of the dataframes in a list of dataframes in two. The
#' split is done either by row or by column (specified by direction), where
#' the first match to pattern is found in the first row (if direction is row) or
#' column of the 2nd of the new dataframes.
#' 
#' @details
#' Where this is the dataframe:
#'
#' |           | __col 1__ | __col 2__ | __col 3 __ | __col 4__ | __col 5__ |
#' |-----------|-----------|------------|-----------|-----------|-----------|
#' | row 1     |   A       |            |           |  B        |           |
#' | row 2     |   C1      |   1        |           |  D1       |  3        |
#' | row 3     |   C2      |   2        |           |  D2       |  4        |
#'
#' Split would be by column because the tables are side-by-side not one above
#' the other.
#' pattern could be "D", because this is first found in the first column of the
#' second table (the table to the right).
#'
#' @md 
#' 
#' @param dat list of dataframes originally created from a single dataframe
#' imported using tidyxl::xlsx_cells().
#' @param pattern character string. Regular expressions that first
#' matches a string in the first column of the second table.
#' @param instance integer or integer as character string. The instance of the
#' pattern match (1st 2nd, 3rd etc).
#' @param offset_by integer or integer as character string. The number of rows
#' or columns to subtract from the row/column on which the pattern match was
#' found in order to get to the row/column of the split.
#' @param direction character string. Either 'col' if the table is to be split
#' by column, or 'row' if it is to be split by row.
#' @param table_id integer. The full table is id 1. Table 1 will be
#' split into table 2 and 3. If split direction is col, table 2 is to the left,
#' table 3 to the right. If split direction is row, table 2 is at the top, table
#' 3 below.
#'
#' @returns  A list of dataframes, each containing a subset of dat corresponding
#' to a table.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   "row" = c(1, 1, 1, 2, 2, 2),
#'   "col" = c(1, 2, 3, 1, 2, 3),
#'   "address" = c("A1", "B1", "C1", "A2", "B2", "C2"),
#'   "character"= c(NA,NA,NA,"table 1","NA","table 2")
#' )
#' split_tables(list(dat), "table 2", "col", 1)
#' }
split_tables <- function(
    dat, pattern, instance, offset_by, direction, table_id
    ) {

  if (
    all(is.na(pattern), is.na(instance), is.na(offset_by), is.na(direction),
        is.na(table_id))
    ) {
    return (dat)
  }

  if (is.na(pattern)) {
    stop(
      "pattern is required"
    )
  }

  if (is.list(dat)) {
    if (is.null(nrow(dat[[1]]))) {
      stop("dat must be a list")
    }
  } else {
    stop("dat must be a list")
  }
  if (is.character(instance)) {
    instance <- as.integer(instance)
  }
  if (is.character(offset_by)) {
    offset_by <- as.integer(offset_by)
  }

  table_id <- as.numeric(table_id)
  table_to_split <- dat[[table_id]]

  message("Finding location at which to split table ", table_id, ".")
  split_loc <- get_split_loc(
    table_to_split, pattern, instance, offset_by, direction
    )

  if (any(is.null(split_loc), is.na(split_loc))) {
    stop(
      "Tables cannot be split. This may happen if the pattern used to identify ",
      "where to split the table no longer occurs in the data. ",
      "Please contact a developer to edit the settings."
    )
  } else {
    tables <- create_table_list(dat, table_id, split_loc, direction)
  }

  return(tables)

}


#' @title Get the column number for the start of each table
#'
#' @description Get the number of the row or column that will come immediately
#' after the split (i.e. the first row or col of the 2nd table). If tables are
#' stacked one above the other, the split will be done by row and the split loc
#' will be a row number. If the tables are side-by-side, the split will be done
#' by column (col) and the split loc will be a column number.
#'
#' @param dat dataframe imported using tidyxl::xlsx_cells().
#' @param pattern character string. Regular expression that matches a string
#' in the first row or column after where the split needs to happen.
#' @param instance integer. The instance of the pattern match (1st, 2nd etc).
#' @param offset_by integer. The number of rows or columns to subtract from the
#' row/column on which the pattern match was found.
#' @param direction character string. Either 'col' if the table is to be split
#' by column, or 'row' if it is to be split by row.
#'
#' @returns integer indicating the column or row at which the second table
#' starts.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   "row" = c(1, 1, 1, 2, 2, 2),
#'   "col" = c(1, 2, 3, 1, 2, 3),
#'   "address" = c("A1", "B1", "C1", "A2", "B2", "C2"),
#'   "character"= c(NA,NA,NA,"Education Services","NA","NA")
#' )
#' get_split_loc(dat, '(?i)education', 'col')
#' }
get_split_loc <- function(dat, pattern, instance, offset_by, direction) {

  if (direction == "row") {
    message("Getting row number to split table at")
  } else if (direction == "col") {
    message("Getting col number to split table at")
  } else {
    stop ("Direction must be 'row' or 'col'")
  }

  possibilities <- find_all_instances(dat, pattern, direction)
  split_loc <- get_loc_from_instance(possibilities, instance, offset_by)

  return(split_loc)
}


#' @title Add two new tables to the list by splitting an existing table
#'
#' @description Split one of the tables in a list of tables in two, based on a
#' provided split location and whether it should be split by row or column. Add
#' the two new tables to the end of the list of tables.
#'
#' @param dat list of dataframes originally imported using tidyxl::xlsx_cells().
#' @param table_id integer. The list index of the table that is to be split.
#' @param split_loc iteger giving the column or row number at which the second
#' new table starts.
#' @param direction character string. Either 'col' if the table is to be split
#' by column, or 'row' if it is to be split by row.
#'
#' @returns list of dataframes. dat with two new tables appended to the end.
#'
#' @examples
#' \dontrun{
#' table <- data.frame(
#'   "row" = c(1, 1, 1, 2, 2, 2),
#'   "col" = c(1, 2, 3, 1, 2, 3),
#'   "address" = c("A1", "B1", "C1", "A2", "B2", "C2"),
#'   "character"= c(NA,NA,NA,"table 1","NA","table 2")
#' )
#' dat <- list(table)
#'
#' create_table_list(dat, 1, 3, 'col')
#' }
create_table_list <- function(dat, table_id, split_loc, direction) {

  from_table <- dat[[table_id]]
  table_1_index <- length(dat) + 1
  table_2_index <- table_1_index + 1

  message(
    "Splitting table ", table_id, " at ", direction, " ", split_loc, " ",
    "to create tables ", table_1_index, " and ", table_2_index, "."
    )

  if (split_loc > max(from_table[direction])) {
    stop(
      "The split_loc is higher than any available ", direction, ". ",
      "Please contact a developer."
    )
  }

  dat[[table_1_index]] <- filter(from_table, !!sym(direction) < split_loc)

  dat[[table_2_index]] <- filter(from_table, !!sym(direction) >= split_loc)

  return(dat)

}


#' @title Clean up list of tables so that each cell is only represented once
#'
#' @description Remove tables from the list of tables that are no longer
#' required. For example, table 1 will always be removed because table 1 is
#' used to create tables 2 and 3.
#'
#' @param dat list of dataframes.
#' @param used_tables iteger vector. The indices of the tables to remove from
#' dat
#' @param keep integer vector. The IDs of the tables to keep. Default is NA, in
#' which case all tables are kept.
#'
#' @returns list of dataframes.
#'
#' @examples
#' \dontrun{
#' dat_1 <- data.frame(a = 1)
#' dat_2 <- data.frame(b = 2)
#' dat_3 <- data.frame(c = 3)
#' dat <- list(dat_1, dat_2, dat_3)
#' remove_used_tables(dat, 1)
#' }
remove_used_tables <- function(dat, used_tables, keep = NA) {

  message(
    "Removing tables that have been used to create subtables: ",
    paste0(used_tables, collapse = ", ")
    )

  keep <- as.integer(keep)

  if (any(keep %in% used_tables)) {
    not_allowed <- which(keep %in% used_tables)
    stop(
      "You have selected to keep the following subtable that have been used to",
      " create further subtables: '", paste(not_allowed, collapse = ", "),
      "'. Please correct the settings so that no subtables used to create ",
      "further subtables are selected to be kept for processing."
    )
  }

  used_tables_ordered <- sort(used_tables)
  tables_to_keep <- setdiff(1:length(dat), used_tables_ordered)

  # The user may alternatively choose to only keep a subset of the subtables.
  # In this case we should tell the user which tables were available but not
  # kept.
  if (! all(is.na(keep))) {
    removed <- setdiff(tables_to_keep, keep)
    message(
      "Removing tables that have not been selected to keep in settings: ",
      paste0(removed, collapse = ", ")
      )
    tables_to_keep <- as.integer(keep)
  }

  message("Retaining subtables: ", paste0(tables_to_keep, collapse = ", "))

  return(dat[tables_to_keep])
}
