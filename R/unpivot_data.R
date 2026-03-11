#' @title take non-tidy data out of xlsx_cells layout
#'
#' @description Convert untidy Excel data to tidy data with one numeric value
#' per row and any number of descriptor columns. dat is tidy because it is
#' xlsx_cells data but it has one row of data for every cell in a sheet, so is
#' not useful for subsequent purposes. This function is largely based on
#' unpivotr.
#'
#' @param dat Dataframe created using tidyxl::xlsx_cells().
#' @param columns_to_create character vector. The names of columns that will
#' be created during behead. The top header in dat will be pivoted to a
#' column that is given the first name in columns_to_create. If there is a
#' second row of headers, they will be pivoted to a column that is given the
#' second name in columns_to_create etc.
#' @param first_header_row integer. the number of the first row in which there
#' are headers.
#' @param tolerance numeric between 0 and 1. Defaults to 0.4. If tolerance is
#' 0.4, the column will be considered 'numeric' if 40% or more of the cells
#' in that row are numeric. 0.4 works for the vast majority of datasets.
#' However, if, for example, the data has many header rows but very few numeric
#' data rows, or has many suppressed values in the first couple of value columns
#' you may need to decrease the tolerance. If the last descriptive column in the
#' left block contains a lot of cells with numbers, you may need to set the
#' tolerance to higher than 0.4.
#' @param left_headers character vector or NA. Optional. The names to be given
#' to the columns containing descriptor data (to the left of the data). If
#' a column in the left block has previously been removed by
#' columns_to_remove_patterns, it must not be included in left_headers.
#' @param minimum_number_of_consecutive_columns integer. Defaults to 2. If more
#' than 2 consecutive numeric columns are required to identify the start of the
#' right block of numeric data, specify the number required. i.e. if only
#' columns 3 and 4 contain numbers, this counts as 2 consecutive columns and
#' minimum_number_of_consecutive_columns would not need to be specified. If,
#' however, columns 3 and 4 contain numbers but are in the left block of
#' descriptors, and columns 6, 7, and 8 contain numbers and are the first
#' columns of the right block, minimum_number_of_consecutive_columns would be 3.
#' @param right_block_offset integer. The value to add to the identified column
#' number. If the last of the descriptive columns appear(s) numeric use a
#' positive offset. If the first of the numeric right block columns contains a
#' very low proportion of numbers and for some reason you can't change the
#' tolerance, you could use a negative offset value.
#' @param header_to_split character string or NA. Optional. The name of the
#' column whose information is to be split.
#' @param header_split_to vector of character strings or NA. The new names of
#' the columns that the split data will be saved to. There must be one name for
#' each chunk of the split string, so the length of header_split_to will be one
#' greater than the length of split_points.
#' @param split_points vector of character strings that code for regular
#' expressions or NA. See the Wiki or build_regex() for more information on how
#' these codes are written. Each one will only be matched once, so any repeats
#' must be explicit.
#' @param column_to_right_of_data_name_pattern Character string or NA. Regular
#' expression that matches the name of a descriptor column to the right of the
#' numeric data in the excel file. Only one can be given. If more are needed the
#' function will need to be updated.
#' @param tidy_data boolean. Defaults to FALSE. Use TRUE when data only have one
#' column of numeric values.
#' @param tidy_notes_name character string. Only used if tidy_data is true.
#' Only required if there is a character column to the right of tidy data,
#' in which case it is used as a name for that column.
#'
#' @export
unpivot_data <- function(dat,
                         columns_to_create,
                         first_header_row,
                         tolerance = 0.4,
                         left_headers,
                         minimum_number_of_consecutive_columns,
                         right_block_offset,
                         header_to_split, header_split_to, split_points,
                         column_to_right_of_data_name_pattern,
                         tidy_data,
                         tidy_notes_name
) {

  if (is.na(tidy_data)) {
    tidy_data <- FALSE
  }

  if (tidy_data) {
    return(reformat_tidy_data(dat, columns_to_create, tidy_notes_name))
  }

  if (all(tidy_data == FALSE, is.na(columns_to_create))) {
    stop("When tidy_data is FALSE, columns_to_create is required.")
  }

  if (length(header_to_split) > 1) {
    stop("More than one header_to_split provided. Please correct the settings.")
  }

  if (!is.na(header_to_split) & !header_to_split %in% columns_to_create) {
    stop("header_to_split must be present in the columns_to_create list")
  }

  if (is.na(tolerance)) {
    tolerance <- 0.4
  }

  header_row_count <- length(columns_to_create)
  first_data_row <- first_header_row + header_row_count

  message(
    "The first row of data has been identified as row ",
    first_data_row, "."
  )

  # assume the first column of the right block is the first of consecutive
  # numeric columns (assuming no blank cols between them) -
  # where a numeric column is defined as one where more than a given percentage
  # of it's cells hold numeric data (the % is supplied by tolerance numeric*100)
  data_below_headers <-  filter(dat, row >= first_data_row)
  numeric_column_locs <- get_vector_locs_of_type(
    dat = data_below_headers, datatype = "numeric", tolerance = tolerance,
    direction = "col", include_blanks = FALSE
    )

  tolerance <- as.numeric(tolerance)

  if(length(numeric_column_locs) == 0) {
    stop(
      "No columns that are ", tolerance*100,
      "% numeric have been found, so the split between the left block of ",
      "descriptor columns and the right block of numeric columns ",
      "could not be identified. Please contact a developer so they can edit ",
      "the settings (numeric_tolerance) for this dataset. For developers: ",
      "If there is only one column of numeric data, please check if the data ",
      "conform to tidy_data requirements (see pub_sec Wiki sheet_structure ",
      "pages for guidance). If so you will need to update sheet_structure to ",
      "reflect this."
    )
  }

  first_right_header_col <- get_first_data_col_number(
    dat, numeric_column_locs,
    minimum_number_of_consecutive_columns, right_block_offset
  )

  message("First numeric column: ", first_right_header_col)

  left_block_cols_to_name <- get_left_col_names(
    dat, first_right_header_col, left_headers, first_data_row
    )
  message(
    "Left block column names: '",
    paste0(left_block_cols_to_name, collapse = "', '"), "'.")

  outer_col_location <- get_outer_descriptor_col_loc(
    dat, header_row_count, first_right_header_col, first_header_row,
    column_to_right_of_data_name_pattern
    )

  no_duplicated_headers <- remove_duplicated_header_rows(
    dat, first_header_row, header_row_count
    )

  right_beheaded <- no_duplicated_headers %>%
    get_right_block(first_right_header_col, excluded = outer_col_location) %>%
    unpivot_right_block(columns_to_create) %>%
    remove_underscore_lines(columns_to_create) %>%
    split_to_multiple_columns(header_to_split, header_split_to, split_points)

  left_beheaded <- behead_left_block(
    no_duplicated_headers, left_block_cols_to_name, first_header_row,
    header_row_count
  )

  left_and_right_joined <- join_left_and_right_unpivotted_data(
    left = left_beheaded,
    right = right_beheaded,
    column_names = names(no_duplicated_headers),
    first_header_row = first_header_row
  ) %>%
    mutate(is_blank = ifelse(set_is_blank_to_true, TRUE, is_blank),
           data_type = ifelse(set_is_blank_to_true, "blank", data_type)) %>%
    select(-set_is_blank_to_true)

  if (!is.na(outer_col_location)) {

    outer_col_returned <- add_columns_to_right_of_data_back_in(
      no_duplicated_headers, left_and_right_joined, outer_col_location,
      first_header_row, header_row_count
    )

  } else { outer_col_returned <- left_and_right_joined }

  cleaned <- outer_col_returned %>%
    remove_remnants_from_behead(first_data_row) %>%
    remove_non_numeric_rows(
      c(columns_to_create, left_block_cols_to_name), header_split_to
      )

  return(cleaned)
}


#' @title Reformat tidy data
#'
#' @description
#' Where the raw data are already in a tidy format with one numeric value per
#' row, a single row of column headers, and a single data type in each column,
#' this function is used to take the data out of xlsx_cells format.
#'
#'  Note that if there are ANY character strings in the 'value' column, that
#'  column will be treated as a character column not numeric. It is assumed
#'  that if data really are in a tidy format, this will not occur. However a fix
#'  may need to be coded in if this ever comes up.
#'
#' @param dat xlsx_cells data
#' @param columns_to_create character string. Optional. Used as the name of a
#' column that will contain the original name of the value column.
#' @param tidy_notes_name character string. Only required if there is a
#' character column to the right of tidy data, in which case it is used as a
#' name for that column.
#'
#' @returns dat as a tibble with the same layout as the raw data.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'     row = c(1, 1, 2, 2),
#'     col = c(1, 2, 1, 2),
#'     data_type = c(rep("character", 3), "numeric"),
#'     character = c("description", "value", "A", NA),
#'     numeric = c(NA, NA, NA, 100),
#'     is_blank = FALSE,
#'     sheet = "Sheet1"
#'     )
#'  reformat_tidy_data(dat)
#' }
#' @export
reformat_tidy_data <- function(
    dat, columns_to_create = NA, tidy_notes_name = NA
    ) {

  message("Returning data to original layout.")

  if (length(columns_to_create) > 1) {
    stop(
      "When tidy_data is true, only one columns_to_create should be provided. ",
      "Only value columns should be included. For character columns on the ",
      "left use left_headers. For a character column to the right use ",
      "tidy_notes_name."
    )
  }

  beheaded <- dat %>%
    behead("up", header) %>%
    select(row, data_type, header, character, numeric, is_blank, sheet) %>%
    filter(is_blank == FALSE) %>%
    spatter(header)

  value_column_name <- beheaded %>%
    select(where(is.numeric), -row) %>%
    names()

  if (is.na(columns_to_create) | columns_to_create == "value") {
    message(
      "The name of the original value column will be put in a column called '",
      value_column_name, "'. If you would like it to have a different name, ",
      "supply a string to columns_to_create.")
    description_added <- beheaded

  } else {
    description_added <- beheaded %>%
      mutate(!!sym(columns_to_create) := value_column_name)
  }

  if ("<NA>" %in% names(description_added)) {
    if (!is.na(tidy_notes_name)) {
      renamed <- description_added %>%
        # numeric is what non-tidy data value columns will be
        rename(!!sym(tidy_notes_name) := `<NA>`,
               numeric = !!sym(value_column_name))
    } else {
      warning(
        "Unexpected column found. This has been named 'notes'. To suppress ",
        "this warning please use the tidy_notes_name setting."
        )
      # numeric is what non-tidy data value columns will be
      renamed <- description_added %>%
        rename(notes = `<NA>`,
               numeric = !!sym(value_column_name))
    }
  } else {
    # numeric is what non-tidy data value columns will be
    renamed <- description_added %>%
      rename(numeric = !!sym(value_column_name))
  }

  return (renamed)

}


#' @title Get the locations of columns or rows of a given datatype
#'
#' @description Return the vector of original column numbers for which a
#' given datatype is present for a given proportion of rows.
#' This is required because xlsx_cells data are given a row per cell.
#' Column datatypes for the original columns are not therefore assigned on
#' import.
#'
#' @param dat xlsx_cells datafarame.
#' @param datatype character. the data_type of interest. One of:
#' 'numeric', 'character', 'logical', 'date', 'blank', or 'error'
#' @param tolerance integer. 0-1 inclusive. The proportion of cells in a column
#' that need to be of the given datatype for a column to be returned.
#' @param direction Whether to look at columns ('col') or rows ('row'.
#' Defaults to "col".
#' @param include_blanks bool. Defaults to TRUE. If false cells that ate blank
#' are not included in the calculation of the proportion of cells of type.
#'
#' @returns vector of integers referring to row of column locations in the raw
#' Excel data.
#'
#' @examples
#' \dontrun{
#' dat looks like this in excel
#' | 1 | A | 1 | 1 |
#' | 2 | B | 2 | 2 |
#' | 3 | C | 3 | 3 |
#' | 4 | D |   |   |
#' | 5 | E |   |   |
#'
#' dat <- data.frame(
#'   "col" = rep(1:4, each = 5),
#'   "row" = rep(1:5, times = 4),
#'   "data_type" = c(
#'     rep("numeric", 5),
#'     rep("character", 5),
#'     rep(c(rep("numeric", 3), rep("blank", 2)), times = 2)
#'     ),
#'   "character" = c(rep(NA, 5), LETTERS[1:5], rep(NA, 10)),
#'   "numeric" = c(1:5, rep(NA, 5),
#'                 rep(c(1:3, rep(NA, 2)), times = 2))
#'   )
#'
#' likely_number_cols <- get_vector_locs_of_type(dat, "numeric", 0.6)
#' definite_number_cols <- get_vector_locs_of_type(dat, "numeric", 1, "col")
#' number_rows <- get_vector_locs_of_type(dat, "numeric", 0.5, "row")
#' }
#' @md
#' @export
get_vector_locs_of_type <- function(dat, datatype, tolerance, direction="col", include_blanks=TRUE) {

  tolerance <- as.numeric(tolerance)

  if (between(tolerance, 0, 1) == FALSE) {
    stop("tolerance must be between 0 and 1")
  }

  if (direction == "col") {
    opposite_direction <- "row"
  } else {
    opposite_direction <- "col"
  }

  if (include_blanks == TRUE) {
    df_for_data_length <- dat
  } else {
    df_for_data_length <- dat %>%
      filter(is_blank==FALSE)
  }

  data_length <- df_for_data_length %>%
    distinct(!!sym(opposite_direction)) %>%
    nrow()

  # in some datasets, characters are used in place of numeric data to indicate
  # suppressed values. These need to be counted as numeric rather than character
  characters_classed_as_numeric <- c("[z]", "-")

  numeric_characters_corrected <- dat %>%
    mutate(data_type =
             ifelse(
               character %in% characters_classed_as_numeric,
               "numeric",
               data_type))

  locations <- numeric_characters_corrected %>%
    filter(data_type == datatype) %>%
    group_by(!!sym(direction)) %>%
    count() %>%
    filter(n >= tolerance*data_length) %>%
    pull(!!sym(direction))

  return(sort(locations))

}


#' @title get the number of the first column with numeric data
#'
#' @description Find the first of consecutive existing columns that have been
#' identified as numeric. The number of consecutive columns required is set
#' using minimum_number_of_consecutive_columns which defaults to 2. If a
#' column has been removed because it is e.g. blank, it does not count. So if
#' the data columns c=start at column 3 (C), column 4 (D) has been removed, and
#' columns 5 - 7 (E-G) are numeric, the first data col number will be returned
#' as 3 (not 5).
#'
#' @param dat dataframe imported using xlsx_cells
#' @param locs integer. A vector holding the numbers of columns identified as
#' holding numeric data.
#' @param min_consecutives integer. The number of consecutive values required.
#' Defaults to 2 i.e. if only columns 3 and 4 contain numbers, this counts as 2
#' consecutive columns and min_consecutives would not need to be specified. If,
#' however, columns 3 and 4 contain numbers but are in the left block of
#' descriptors, and columns 6, 7, and 8 contain numbers and are the first
#' columns of the right block, we would specify min_consecutives as 3. In pub
#' sec this variable is specified by minimum_number_of_consecutive_value_columns.
#' @param offset integer. The value to add to the identified column number. If
#' the last of the descriptive columns appear(s) numeric use a positive offset.
#' If the first of the numeric right block columns contains a very low
#' proportion of numbers and for some reason you can't change the tolerance, you
#' could use a negative offset value.
#'
#' @returns integer. The original column number of the first data column
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'     row = c(1:3),
#'     col = c(1, 3, 5),
#'     value = c(NA, 10, 20)
#'     )
#' get_first_data_col_number(dat)
#' }
get_first_data_col_number <- function(
    dat, locs, min_consecutives = 2, offset = 0
    ) {

  message(
    "Getting the number of the first column in the right block of numeric data."
  )

  if (is.na(min_consecutives)) { min_consecutives <- 2}
  if (is.na(offset)) { offset <- 0}

    number_of_rows <- length(unique(dat$col))

  # create a table to allow us to find consecutive numeric columns even if they
  # are separated by blank or removed columns.
  # e.g:
  # A    B D
  # Eng  1 2
  #
  # B and D should be identified as consecutive numeric columns even though
  # column C (col 3) is missing
  existing_columns <- data.frame(
    index = 1:number_of_rows,
    actual_colum_number = unique(dat$col)) %>%
    mutate(index_updated = ifelse(actual_colum_number %in% locs, index, NA))

  first_of_consecutives <-  get_first_of_consecutives(
    existing_columns$index_updated, min_consecutives, offset
  )

  # refer back to the lookup table to find the original column number
  col_number <- existing_columns %>%
    filter(index_updated == first_of_consecutives) %>%
    pull(actual_colum_number)

  return(col_number)
}



#' @title Get the first integer followed by consecutive numbers
#'
#' @description Get the first number in a sequence where the next x numbers are
#' consecutive
#'
#' For example, if you pass it a vector of numbers that are column positions
#' where the datatype is numeric, it will return the position of the first
#' column where both it and the following column are numeric.
#'
#' @param sequence vector of integers
#' @param x integer. The number of consecutive values required. Defaults to 2
#' i.e. if only columns 3 and 4 contain numbers, this counts as 2 consecutive
#' columns and x would not need to be specified. If, however columns 3 and 4
#' contain numbers but are in the left block of descriptors, and columns 6, 7,
#' and 8 contain numbers and are the first columns of the right block, we would
#' specify x as 3. In pub sec this variable is specified by
#' minimum_number_of_consecutive_value_columns.
#' @param offset integer. The value to add to the identified column number. If
#' the last of the descriptive columns appear(s) numeric use a positive offset.
#' If the first of the numeric right block columns contains a very low
#' proportion of numbers and for some reason you can't change the tolerance, you
#' could use a negative offset value.
#'
#' @returns integer. If there are consecutive values, this will be the first of
#' the consecutive values. If there are no consecutive values a warning will
#' be given and the first value returned.
#'
#' @examples
#' \dontrun{
#' sequence <- c(1, 1, 1, 3, 5, 6, 8, 9, 9, 10, 12, 14, 15, 16, 17, 19)
#' get_first_of_consecutives(sequence)
#' get_first_of_consecutives(sequence, 2)
#' get_first_of_consecutives(sequence, 3)
#' }
#' @export
get_first_of_consecutives <- function(
    sequence, min_consecutives = 2, offset = 0
    ) {

  if (is.na(min_consecutives)) { min_consecutives <- 2 }
  if (is.na(offset)) { offset <- 0 }

  # when setting the minimum number of consecutive columns it is easier to count
  # all consecutive columns including the first. But for the purposes of the
  # code we only want to count the consecutives following the first, so need to
  # subtract 1.
  min_consecutives <- min_consecutives - 1

  sequence <- unique(sequence)
  consecutives_together <- split_by_consecutives(sequence)
  consecutive_sets <- lengths(consecutives_together) > min_consecutives

  if (any(consecutive_sets)) {
    first_set <- consecutives_together[consecutive_sets==TRUE][[1]]
  } else {
    # If there are no consecutive values, return the first occurrence
    first_set <- sequence[1]
    warning(
      "No consecutive values were found. This may have resulted in an error ",
      "in identifying the row or column on which data start. The first column ",
      "identified as numeric will be assumed to be the first of the columns ",
      "the right block of numeric data."
    )
  }

  output <- first_set[1] + offset

  if(output < 1) {
    stop(
      "The number cannot be less than 1. The right_block_offset setting may ",
      "need to be updated.")
  }

  return(output)

}


#' @title split a numeric vector into sets of consecutive numbers
#'
#' @description split a numeric vector into sets of consecutive numbers.
#'
#' @param x numeric vector
#'
#' @returns list where each element of the list is a vector of consecutive
#' integers.
#'
#' @examples
#' \dontrun{
#' x <- c(1, 1, 3, 5, 6, 7, 10, 23, 24)
#' split_by_consecutives(x)
#' }
split_by_consecutives <- function(x) {

  ordered <- sort(x)
  output <- split(ordered, cumsum(c(1, diff(ordered) != 1)))

  return(output)

}


#' @title Get the column names for the left block.
#'
#' @description Get the names of the columns to the left of the numeric columns
#' (referred to here as the left block).
#'
#' Sometimes columns in the left block are all named in the raw
#' data, sometimes none of them are named, and sometimes only some are, with
#' others left blank.
#'
#' We have the option to provide names for columns in the left header block in
#' the data dictionary (left_headers), however this is risky as suppliers could
#' change the number or order of those columns.
#'
#' A third option is to use placeholder names (e.g. 'column_1', 'column_2' etc)
#'
#' Rules:
#' - If there is the correct number of names in the data (existing names) use
#'    these, even if names have been given in the data dictionary (left_headers).
#' - If the number of names given in the dict is too many do not use any of them.
#' - If there are names missing from the existing names, fill them with
#'    placeholder names, but use the ones that are given.
#' - Use all of left_headers if no names already exist in the data AND the
#'    correct number of names has been supplied.
#'
#' @param dat dataframe. Must be xlsx_cells format.
#' @param first_right_header_col int. The number of the first column of data.
#' @param left_headers character vector. Can be NA (see rules above).
#' @param first_data_row integer. The row number of the first line of data
#' (below the headers).
#'
#' @returns character vector with names of columns in the left block of data.
#'
#' @examples
#' \dontrun{
#'
#' dat <- data.frame(
#'     row = c(1, 1, 1, 2, 2, 2),
#'     col = c(1, 2, 3, 1, 2, 3),
#'     data_type = c(rep("blank", 2), rep("character", 3), "numeric"),
#'     character = c(NA, NA, "right", "a", "b", NA)
#'     )
#'
#' get_left_col_names(dat, 3, c("alt 1", "alt 2"), 2)
#' }
get_left_col_names <- function(
    dat, first_right_header_col, left_headers, first_data_row
) {

  message("Getting left header names.")

  # to avoid errors in assigning names to columns, make sure cols are ordered
  ordered_cols <- arrange(dat, col)

  # first get the column ID's (locations) of populated columns in the left block
  col_locs_to_name <- ordered_cols %>%
    filter((col < first_right_header_col)
           & data_type != "blank") %>%
    distinct(col)

  existing_names <- ordered_cols %>%
    filter((col < first_right_header_col)
           & (row < first_data_row)
           & data_type == "character") %>%
    distinct(character, col, row) %>%
    right_join(col_locs_to_name, by = "col") %>%
    arrange(col)

  # In nhs digital SALT 2021-22 there is a row with empty strings in the left
  # block headers, and another row with filled strings. Treat the empty ones
  # like NAs
  non_blank_names <- existing_names %>%
    mutate(character = ifelse(str_squish(character) == "",
                              NA,
                              character))

  # In the NI council tax and NNDR data 2023-24 the table title (the year)
  # is in the same row as the headers, so it has not been removed, and now
  # looks like it could be a left block header because it is above the data and
  # to the left of the first data column.
  # DLUHC borrowing and investments has row headers
  # Assume that there is only ever one row of left block headers: Choose
  # which is the right row by:
  # 1. Are there any rows with the expected number of entries?
  # - if 'yes' limit the options to these
  # 2. Out of the now limited rows, use the row that is closest to the first row
  # of data.
  if (any(!is.na(non_blank_names$character))) {
    nas_removed <- non_blank_names[!is.na(non_blank_names$character),]
    header_row <- max(nas_removed$row, na.rm = TRUE)

    cols_to_name_count <- nrow(col_locs_to_name)
    rows_with_all_cols_filled <- nas_removed %>%
      group_by(row) %>%
      count() %>%
      filter(n == cols_to_name_count) %>%
      pull(row)

    if (length(rows_with_all_cols_filled) > 0) {
      existing_names_chosen_row <- non_blank_names %>%
        filter(row == max(rows_with_all_cols_filled))
    } else {
      existing_names_chosen_row <- non_blank_names %>%
        filter(row == header_row | is.na(row))
    }

    names_excluding_blanks <- existing_names_chosen_row %>%
      filter(is.na(character) == FALSE)

    all_names_exist <- nrow(names_excluding_blanks) == nrow(col_locs_to_name)
    some_names_exist <- any(!is.na(existing_names_chosen_row$character))

  } else {
    all_names_exist <- FALSE
    some_names_exist <- FALSE
  }

  name_count <- length(left_headers[!is.na(left_headers)])
  header_count <- nrow(col_locs_to_name)

  # check the left_headers passed from the data dict
  left_headers_right_length <- name_count == header_count
  too_many_left_headers <- name_count > header_count
  too_few_left_headers <- name_count < header_count

  if (all_names_exist) {
    cols_to_name <- existing_names_chosen_row$character

  } else if (some_names_exist) {
    cols_to_name <- existing_names_chosen_row %>%
      mutate(character = ifelse(
        is.na(character),
        paste0("column_", as.character(col)),
        character
      )) %>%
      pull(character)

    # if no names exist in the data:
  } else if (left_headers_right_length) {
    cols_to_name <- left_headers

  } else {

    if (too_many_left_headers) {
      warning(
        "More left_headers were supplied in the data dict than have been found ",
        "in the data. Is there a column that is usually in the data now missing ",
        "(i.e. columns that contain non-numeric info)? If preprocessed data ",
        "does not have the expected headers please contact a developer."
      )
    } else if (all(too_few_left_headers & length(left_headers) > 0)) {
      warning(
        "Fewer left_headers were supplied in the data dict than exist in the ",
        "left block of columns in the raw data (i.e. columns that contain ",
        "non-numeric info). Placeholder names will be used instead of the ",
        "supplied headers. Is there an extra column in the data compared to ",
        "previous releases? If preprocessed data does not have the expected ",
        "headers please contact a developer."
      )
    }

    cols_to_name <- col_locs_to_name %>%
      mutate(character = paste0("column_", as.character(col))) %>%
      pull(character)
  }

  return(cols_to_name)

}


#' @title Get the column number of an outer right descriptor column.
#'
#' @description Some datasets have the following structure:
#' Descriptor columns e.g. Year > Numeric columns > Descriptor e.g. notes
#'
#' In such cases where the column to the right of the data block is a character
#' column, if it is not dealt with separately the information in it will be lost
#' when the right block is beheaded (unpivotted). This is because the
#' unpivotting only keeps numeric data. We can choose to either delete such
#' columns using settings in sheet_structure, or (in pub sec) use this function
#' to keep them by specifying column_to_right_of_data_name_pattern
#' (name_regex) in settings.
#'
#' This function returns the column number of a descriptor column to the right
#' of the data. However, it will only return the column number if it fulfils
#' the following conditions:
#' - name_regex is not NA
#' - There is wording within the header rows that matches the name_regex.
#' - The string that matches name_regex is found within the LAST 3 columns
#'   that have any information in the header rows.
#'
#' If any of these conditions are not met, NA will be returned.
#'
#' Note: Currently we only have an issue with a single non-numeric descriptor
#' column being to the right of the data (e.g. DLUHC CPR3 year and quarter
#' series 2023-24). Future improvements are required to allow multiple columns.
#'
#' @param dat xlsx_cells data.
#' @param header_row_count integer. the number of header rows.
#' @param first_right_header_col the column number of the first column of the
#' numeric data block.
#' @param first_header_row integer. The row number of the first header.
#' @param name_regex Character string. Regular expression. Only one can be
#' given. If more are needed the function will need to be updated. In pub sec
#' this variable is specified using column_to_right_of_data_name_pattern.
#'
#' @returns integer. The column number or NA (see description for detail).
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#' address = c("A1", "B1", "C1", "A2", "B2", "C2", "A3", "B3", "C3"),
#' row = rep(1:3, each = 3),
#' col = rep(1:3, times = 3),
#' character = c(NA, "education", "Notes",
#'               "Year", "primary", "(see frontsheet)",
#'               "2020", NA, "note 1"),
#' numeric = c(NA, NA, NA, NA, NA, NA, NA, 100, NA)
#' )
#' get_outer_descriptor_col_loc(
#'                    dat,
#'                    header_row_count = 2,
#'                    first_right_header_col = 2,
#'                    first_header_row = 1,
#'                    name_regex = "(?i)note"
#'                    )
#' }
get_outer_descriptor_col_loc <- function (
    dat, header_row_count,
    first_right_header_col, first_header_row, name_regex) {

  if (all(is.na(name_regex))) {

    return (NA)

  } else if (length(name_regex) > 1) {

    warning(
      "More than one pattern has been given for non-numeric columns to ",
      "the right of the numeric data ",
      "(column_to_right_of_data_name_pattern). Only the first will be kept. ",
      "If there is more than one such column in the raw data that you want ",
      "to keep, a developer will need to adapt the following functions: ",
      "get_outer_descriptor_col_loc and add_columns_to_right_of_data_back_in."
    )

    name_regex <- name_regex[1]


  } else {
    message(
      "Getting the column number of the descriptor column to the right of the ",
      "numeric data"
    )
  }

  last_header_row <- first_header_row + (header_row_count - 1)

  right_block_colnames <- dat %>%
    filter(col >= first_right_header_col &
             row %in% first_header_row:last_header_row) %>%
    select(character, row, col, address)

  pattern_match_colnames <- right_block_colnames %>%
    filter(str_detect(character, name_regex) == TRUE)

  col_letters <- pattern_match_colnames %>%
    mutate(col_letter = str_match(address, "[A-Z]*")) %>%
    distinct(col_letter) %>%
    pull(col_letter)

  col_count <- length(unique(pattern_match_colnames$col))

  if (col_count == 0) {
    warning(
      "No columns have been found to the right of the numeric data. Please ",
      "contact a developer for assistance if the info you require is not ",
      "in the output. The setting for column_to_right_of_data_name_pattern ",
      "may need to be updated."
    )

    return(NA)

  } else if (col_count > 1) {
    warning(
      "Multiple columns have been found to the right of the numeric data ",
      "in the following column positions: ",
      paste0(col_letters, collapse = ", "), ". ",
      "Only information in the last of these columns will be considered to be ",
      "a descriptor column on the right of the numeric data. ",
      "Please contact a developer for assistance if the info you require is not ",
      "in the output. The setting for ",
      "column_to_right_of_data_name_pattern may need to be updated."
    )
    pattern_match_colnames <- filter(pattern_match_colnames, col == max(col))
  }

  column_loc <- pull(pattern_match_colnames, col)
  last_col <- max(right_block_colnames$col)
  column_is_last <- last_col == column_loc
  column_is_colse_to_last <- column_loc >= last_col - 2

  if (column_is_last) {

    return (column_loc)

  } else if (column_is_colse_to_last) {

    warning(
      "`name_regex` (column_to_right_of_data_name_pattern) is specified ",
      "as '", name_regex, "' in the settings. The column that matches this ",
      "pattern is not the last column in the data. It is however close to the ",
      "last column so will be processed as a descriptor column. If it is a ",
      "numericcolumn, the data will still be processed as normal. However, if ",
      "thisis not the correct column to use as the descriptor column please ",
      "contact a developer to edit the setting for ",
      "column_to_right_of_data_name_pattern."
      )

    return (column_loc)

  } else {

    warning("`name_regex` (column_to_right_of_data_name_pattern) is specified ",
            "as '", name_regex, "' in the settings, but this is not found ",
            "within 2 columns of the last column in the table. ",
            "If this column contains non-numeric ",
            "information this will not be included in the output. If this column ",
            "needs to be in the output please contact a developer.")

    return (NA)
  }

}


#' @title Only keep the first of multiple rows of matching headers.
#'
#' @description This function exists because in the DLUHC CPR files (e.g.
#' in 2024-25), the year_&_quarter_series table contains two tables stacked
#' vertically that have exactly the same headers because the top table is a
#' filtered version of the table below. This function removes repeated rows of
#' headers, keeping only the first.
#'
#' If there are multiple header rows for any one table (i.e. if header_row_count
#' is greater than 1, duplicated header rows will only be removed if ALL header
#' rows are duplicated. In the example below, the headers of the 2nd table would
#' be removed in set 1, but no headers would be removed from set 2 (a warning is
#' given):
#'
#' Set 1:
#'        > Education
#'  Year  > primary
#' ------ > ----------
#'   2020 > 100,000
#'
#'        > Education
#'  Year  > primary
#' ------ > ----------
#'  2021  >  90,000
#'
#' Set 2:
#' (note that year is replaced with month in the second table)
#'        > Education
#'  Year  > primary
#' ------ > ----------
#'   2020 > 100,000
#'
#'        > Education
#'  Month > primary
#' ------ > ----------
#'   Jan  >  8,000
#'
#' @param dat dataframe. Data imported using tidyxl::xlsx_cells.
#' @param first_header_row integer. The row number of the first header.
#' @param header_row_count integer. The number of header rows (not including
#' repeated sets).
#'
#' @returns dat with repeated headers removed.
#'
#' @examples
#' \dontrun{
#'#  Year > Education
#'# ----- > ----------
#'#  2020 >    22
#'#
#'#  Year > Education
#'# ----- > ----------
#'#  2020 >    22
#'
#' dat <- data.frame(
#'   address = c("A1", "B1", "A2", "B2", "A3", "B3", "A4", "B4"),
#'   row = rep(1:4, each = 2),
#'   col = rep(1:2, times = 4),
#'   data_type = rep(c(rep("character", 2), rep("numeric", 2)), times = 2),
#'   numeric = rep(c(rep(NA, 2), 2020, 22), times = 2),
#'   character = rep(c("Year", "Education", NA, NA), times = 2)
#'   )
#' remove_duplicated_header_rows(dat, 1, 1)
#'
#' # case where there are partially duplicated headers (2 header rows, but only
#' # one is duplicated)
#' dat <- data.frame(
#'   address = c("A1", "B1", "A2", "B2", "A3", "B3", "A4", "B4"),
#'   row = rep(1:4, each = 2),
#'   col = rep(1:2, times = 4),
#'   data_type = c(
#'       rep("character", 3), "blank", rep("numeric", 2), "character", "blank"
#'       ),
#'   numeric = c(rep(NA, 4), 1, 2, NA, NA),
#'   character = c("a1", "b1", "a2", NA, NA, NA, "a2", NA)
#'   )
#' remove_duplicated_header_rows(dat, 1, 2)
#' }
remove_duplicated_header_rows <- function (
    dat, first_header_row, header_row_count
    ) {

  duplicated_counts <- tibble(row_num = as.integer(), count = as.integer())
  duplicated_rows <- NULL

  for (i in 1:header_row_count) {
    row_num <- first_header_row + (i - 1)
    duplicated_row <- get_duplicated_rows(dat, row_num)

    duplicated_counts <- duplicated_counts %>%
      add_row(
        row_num = row_num,
        count = length(duplicated_row[!is.na(duplicated_row)])
      )
    duplicated_rows <- c(duplicated_rows, duplicated_row)

  }

  all_counts_match_check <- distinct(duplicated_counts, count)

  if (nrow(all_counts_match_check) > 1) {
    warning(
      "Some header rows are duplicated but others are not. This suggests ",
      "there may be multiple tables on the same sheet with different headers. ",
      "If so, data may not be under the expcted headings. Please check the ",
      "raw (downloaded) data has not changed in layout, and that the ",
      "pre-processed/validated data are correct. If you find any issues ",
      "please contact a developer."
    )
    return(dat)
  }

  if (length(duplicated_rows) > 0) {

    message("Removing duplicated header rows.")

    no_duplicated_headers <- dat %>%
      filter(!row %in% duplicated_rows)

    return(no_duplicated_headers)

  } else if (length(duplicated_rows) == 0) {
    return (dat)
  }

}


#' @title Get the row numbers of duplicated rows
#'
#' @description Data imported with tidyxl::xlsx_cells have one row for each
#' cell. As such the usual ways of finding duplicated rows in the raw data
#' (e.g `unique` or  `distinct`) do not work.
#'
#' Given a row number, find any rows that are duplicates of that row.
#'
#' @param dat tidyxl::xlsx_cells dataframe
#' @param row_num integer. The row you want to find duplicates of
#' @returns integer vector. The row numbers of duplicated rows
#' (but not the original row: row_num).
#'
#' @examples
#' \dontrun{
#' example_dat <- data.frame(
#'   row = rep(1:4, each = 2),
#'   col = rep(1:2, by = 4),
#'   character = rep(c("year", "quarter", rep(NA, 2)), 2),
#'   numeric = c(NA, NA, 1, 2, NA, NA, 3, 4)
#' )
#'
#' get_duplicated_rows(example_dat, 1)
#' }
#' @export
get_duplicated_rows <- function(dat, row_num) {

  raw_colnames <- dat$character[dat$row==row_num]
  raw_colnames_no_na <- raw_colnames[!is.na(raw_colnames)]

  cells_by_row <- dat %>%
    filter(character %in% raw_colnames_no_na) %>%
    group_by(row) %>%
    count()

  duplicated_rows <- cells_by_row %>%
    filter(n == length(raw_colnames_no_na) &
             row != row_num) %>%
    pull(row)

  return(duplicated_rows)
}


#' @title Only retain rows that relate to numeric columns in the Excel data.
#'
#' @description Remove the left block of row descriptors and, if it exists, the
#' column of descriptors to the right of the data.
#'
#' @param dat dataframe imported using tidyxl::xlsx_cells.
#' @param first_right_header_col integer. The first column number of the numeric
#' data block (aka the right block).
#' @param excluded integer. The column number of the column of descriptors to
#' the right of the data, or NA.
#'
#' @returns dat with rows that relate to non-numeric columns in the excel data
#' removed.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   address = c("A1", "B1", "C1", "D1", "A2", "B2", "C2", "D2"),
#'   row = c(1, 1, 1, 1, 2, 2, 2, 2),
#'   col = c(1, 2, 3, 4, 1, 2, 3, 4),
#'   data_type = c(rep("character", 5), rep("numeric", 2), "character"),
#'   numeric = c(rep(NA, 5), 100, 22, NA),
#'   character = c("Year", "primary", "secondary", "Notes",
#'                  "2021", NA, NA, "note a"))
#'
#' get_right_block(dat, 2, 4)
#' }
get_right_block <- function(dat, first_right_header_col, excluded) {

  message("Isolating main (right) block of data.")

  if (is.na(excluded)) {
    right_block <- filter(dat, col >= first_right_header_col)
  } else {
    right_block <- filter(dat, col >= first_right_header_col & col != excluded)
  }

  return (right_block)
}


#' @title Behead the main block of data (not including row names).
#'
#' @description xlsx_cells imports data so that each row describes a single cell
#' in an xlsx table. This function re-associates cells with their column headers
#' in a tidy format with one numeric value per row. Each row of Excel column
#' headers are put into columns whose names are specified by columns_to_create.
#'
#' This function assumes that column headers will have the following
#' type of relationship to the data: In the final header row, the header sits
#' directly above the data, and every data column has a cell directly above it
#' with header info in it. All header rows above that are either directly above
#' it or staggered to the left. These can be merged cells.
#'
#' e.g.
#'
#'   A1 >    >
#'  --- > ---> ---
#'   b1 >    > b2
#'  --- > ---> ---
#'   c1 > c2 > c3
#'  --- > ---> ---
#'   10 > 20 > 30
#'
#' becomes:
#'
#'   numeric > description_1 > description_2 > description_3
#'   ------- > ------------- > ------------- > -------------
#'     10    >     A1        >    b1         >   c1
#'     20    >     A1        >    b1         >   c2
#'     30    >     A1        >    b2         >   c3
#'
#' @param dat dataframe. Data imported using tidyxl::xlsx_cells. Data should not
#' include the rows containing row name information (character columns to the
#' left of numeric columns in Excel).
#' @param columns_to_create character string vector. The names of the columns
#' that information in the headers will be unpivotted to.
#'
#' @returns dataframe that is an unpivotted version of the numeric columns in
#' the Excel data.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   address = c("B1", "C1", "B2", "C2"),
#'   row = c(1, 1, 2, 2),
#'   col = c(2, 3, 2, 3),
#'   data_type = c(rep("character", 2), rep("numeric", 2)),
#'   numeric = c(rep(NA, 2), 100, 22),
#'   character = c("primary", "secondary", NA, NA))
#'
#' unpivot_right_block(dat, "description_1")
#' }
unpivot_right_block <- function(dat, columns_to_create) {

  message("Unpivotting main (right) block of data.")

  header_row_count <- length(columns_to_create)

  for (i in 1:header_row_count) {

    if (i != header_row_count) {
      dat <- dat %>%
        behead("up-left", !!sym(columns_to_create[i]))
    } else {
      dat <- dat %>%
        behead("up", !!sym(columns_to_create[i]))
    }
  }

  return(dat)

}


#' @title Remove long lines of underscores from headers.
#'
#' @description Long lines of underscores are used in e.g. some Northern Ireland
#' data to denote a line in one of the headings. This function removes these
#' underscore lines.
#'
#' @param dat dataframe.
#' @param columns character string vector. The names of the columns from which
#' to remove the underscore lines.
#'
#' @returns dat dataframe with underscore lines removed.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame( description_1 = c("__A___"))
#' remove_underscore_lines(dat, "description_1")
#' }
remove_underscore_lines <- function(dat, columns) {

  message(
    "Removing lines of underscores from the following columns: '",
    paste0(columns, collapse = "', '"), "'."
    )
  underscores_removed <- dat

  for (i in 1:length(columns)) {
    column <- columns[i]

    underscores_removed <- underscores_removed %>%
      mutate(!!sym(column) := str_remove_all(!!sym(column), "__+"))

  }

  return(underscores_removed)
}


#' @title Behead xlsx_cells data to the left the required number of times.
#'
#' @description xlsx_cells imports data so that each row describes a single cell
#' in an xlsx table. This function re-associates cells to the left of data with
#' the corresponding values to the right (see example). The cols to the left of
#' the data must contain character data.
#'
#' @param dat dataframe imported using tidyxl::xlsx_cells.
#' @param cols_to_name character string vector. Columns to the left of the data
#' are given these as column names. The length of cols_to_name must equal the
#' number of times you want to behead left. i.e. if there are three character
#' columns to the left of the numeric data in Excel, cols_to_name will have 3
#' elements.
#' @param first_header_row integer. Row numebr the first header is on.
#' @param header_row_count integer. The number of header rows.
#'
#' @returns dataframe that looks like the row descriptor columns in the Excel
#' data.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   address = c("A1", "B1", "C1", "A2", "B2", "C2"),
#'   row = c(1, 1, 1, 2, 2, 2),
#'   col = c(1, 2, 3, 1, 2, 3),
#'   data_type = c(rep("character", 4), rep("numeric", 2)),
#'   numeric = c(rep(NA, 4), 100, 22),
#'   character = c("Year", "primary", "secondary", "2021", NA, NA))
#'
#' behead_left_block(dat, 'Year', 1,  1)
#' }
behead_left_block <- function(
    dat, cols_to_name, first_header_row, header_row_count
    ) {

  column_count <- length(cols_to_name[!is.na(cols_to_name)])

  if (column_count == 0) {
    stop("No names found for descriptor columns to the left of numeric data.")
  }

  left_beheaded <- dat

  for (i in 1:column_count) {

    left_beheaded <- left_beheaded %>%
      unpivotr::behead("left", !!cols_to_name[i])
  }

  if (length(cols_to_name) == 1) {
    # blank rows with character information in the left column get deleted by this
    # left behead when there is only one left header. Bring these back in as they
    # may be needed for add_row_headers_as_column
    last_header <- first_header_row + header_row_count - 1
    # we have to filter on numeric because if there is a character column to the
    # right of the data with a value in, the row of the character cell in the
    # left header block will not be counted as a lost character row.
    data_rows <- left_beheaded %>%
      filter(!is.na(numeric)) %>%
      distinct(row)

    last_left_header_col <- length(cols_to_name) + min(left_beheaded$row)

    rows_in_dat_but_not_left_beheaded <- setdiff(dat$row, left_beheaded$row)

    lost_character_rows <- dat %>%
      filter(
        data_type == "character" & row %in% rows_in_dat_but_not_left_beheaded
      ) %>%
      rename(!!sym(cols_to_name) := character)

    if (nrow(lost_character_rows) > 0) {
      lost_character_rows <- lost_character_rows %>%
        # When there are two columns of left headers, this is what the values for
        # data type would be:
        mutate(set_is_blank_to_true = TRUE)
    }

    all_rows <- left_beheaded %>%
      mutate(set_is_blank_to_true = FALSE) %>%
      bind_rows(lost_character_rows) %>%
      arrange(row, col)

  } else {
    all_rows <- left_beheaded %>%
      mutate(set_is_blank_to_true = FALSE)
  }

  return(all_rows)

}



#' @title Join the main value data to the left block of row descriptor columns.
#'
#' @description The left block contains descriptor columns, the right block
#' contains both the numeric value column and the descriptors whose entries come
#' from the Excel column names of the numeric columns. When they are joined all
#' rows from both blocks are retained.
#'
#' @param left dataframe. The beheaded left (row names) block of data.
#' @param right dataframe. The beheaded right (numeric) block of data.
#' @param column_names character string vector. The names of columns to join on
#' (all shared column names).
#' @param first_header_row integer. The row number of the first header.
#'
#' @returns dataframe that will look like the Excel data but with a tidy layout
#' (one numeric value per row).
#'
#' @examples
#' \dontrun{
#' # taken from results of examples in behead_left_block and behead_right_block
#' # so you can follow it though.
#' left <- data.frame(
#'   address = c("B1", "C1", "B2", "C2"),
#'   row = c(1, 1, 2, 2),
#'   col = c( 2, 3, 2, 3),
#'   data_type = c(rep("character", 2), rep("numeric", 2)),
#'   numeric = c(NA, NA, 100, 22),
#'   character = c("primary", "secondary", NA, NA),
#'   Year = c("Year", "Year", "2021", "2021")
#' )
#'
#' right <- data.frame(
#'   address = c("B2", "C2"),
#'   row = 2,
#'   col = c(2, 3),
#'   data_type = "numeric",
#'   numeric = c(100, 22),
#'   character = NA,
#'   description_1 = c("primary", "secondary")
#' )
#'
#' column_names <- c("address", "row", "col", "data_type", "numeric", "character")
#' join_left_and_right_unpivotted_data(left, right, column_names, 1)
#' }
join_left_and_right_unpivotted_data <- function(
    left, right, column_names, first_header_row
) {

  message("Joining main data (right block) to row descriptors (left block).")

  joined <- left %>%
    # First remove the header row from the left block because otherwise it would
    # be in twice after the join.
    filter(row > first_header_row) %>%
    left_join(right,
              by = column_names)

  return(joined)
}


#' @title Join data to the right of the table to the rest of the data by row.
#'
#' @description When there is non-numeric information to the right of the
#' numeric data it has to be beheaded separately (because it is not numeric).
#' It is referred to here as the 'outer column'.
#'
#' In order to associate the outer column with the correct rows in the
#' main data, this function takes the specified column and uses a full_join
#' to join it to the rest of the data as a column (full join used to ensure we
#' don't lose any rows, though a left_join should also work because of how data
#' have been created).
#'
#' NOTE: This function currently only works for where there is just one column
#' in the right_block. Because of how col_location is created, it will only be
#' of length 1. If more col_locations are provided, an error is raised.
#'
#' @param whole dataframe of all the xlsx_cells data.
#' @param left_and_right dataframe of the beheaded data.
#' @param col_location integer giving the column number of the outer column.
#' @param first_header_row integer giving the row number of the first header.
#' @param header_row_count integer giving the number of header rows.
#'
#' @returns If there is a column to the right of the data, the left_and_right
#' dataframe with this information added to it. If not, left_and_right is
#' returned unchanged.
#'
#' @examples
#' \dontrun{
#' whole <- data.frame(
#'   sheet = "one header test data",
#'   row = c(15, 15, 15, 16, 16, 16),
#'   col = c(1, 2, 3, 1, 2, 3),
#'   character = c("date", "education", "Notes", "2020 Q1", NA, "note 1"),
#'   numeric = c(NA, NA, NA, NA, 100, NA)
#' )
#' left_and_right <- data.frame(
#'   sheet = "one header test data",
#'   row = 16,
#'   col = c(1, 3),
#'   date = c("2020 Q1", NA),
#'   description = c("education", NA),
#'   numeric = c(100, NA),
#'   character = c(NA, "note 1")
#' )
#'
#' add_columns_to_right_of_data_back_in(whole, left_and_right, 3, 15, 1)
#' }
add_columns_to_right_of_data_back_in <- function(
    whole, left_and_right, col_location, first_header_row, header_row_count) {

  if (length(col_location[!is.na(col_location)]) == 0) {

    return(left_and_right)

  } else {
    message(
      "Joining descriptor column to the right of the data back ",
      "to the main dataframe."
    )
  }

  if (length(col_location) > 1) {
    stop("More than one column location has been given for non-numeric ",
         "columns to the right of the numeric data. Please adapt the following ",
         "functions if more than one descriptor column to the right of the numeric ",
         "data needs to be kept: add_columns_to_right_of_data_back_in and ",
         "get_outer_descriptor_col_loc")
  }

  col_names <- whole %>%
    filter(col == col_location
           & row >= first_header_row
           & row < first_header_row + header_row_count
           & !is.na(character)) %>%
    arrange(row) %>%
    pull(character)

  if (length(col_names) > 1) {

    col_name <- paste0(col_names, collapse = "_")

    warning("More than one potential column name was found for the column to ",
            "the right of the data: '", paste0(col_names, collapse = "', '"),
            "'. These strings will be combined to form a ",
            "single column name: '", col_name, "'. If this is incorrect you can change ",
            "it using the standardisation questions.")

  } else {

    col_name <- col_names

    message(
      "A character column has been found to the right of the numeric data ",
      "columns. This will be kept as a descriptor column with it's original ",
      "name: '", col_name, "'."
    )
  }

  col_to_right_of_dat_beheaded <- whole %>%
    filter(col == col_location) %>%
    mutate(!!sym(col_name) := character) %>%
    select(sheet, row, !!sym(col_name))

  joined <- col_to_right_of_dat_beheaded %>%
    full_join(left_and_right,
              by = c("sheet", "row"))

  # The information from the outer column has now been added to the row the
  # data it relates to is on, so we can now drop the rows relating to these cells
  output <- filter(joined, col != col_location)

  return(output)

}

#' @title Remove info from above the top row of data.
#'
#' @description Because of how the data are imported with every cell in the
#' Excel data having it's own row in our input, we are left with rows in the
#' unpivotted data that are no longer needed. These rows describe cells that are
#' part of the headers, and the info we needed from them has already been
#' captured. This function removes those cells.
#'
#' @param dat dataframe of unpivotted xlsx_cells data.
#' @param first_data_row integer stating the first row of numeric data.
#'
#' @returns dataframe. dat with unwanted rows removed.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(character = c("header_1", "a"),
#'                   row = 1:2,
#'                   description_1 = c(NA, "a"))
#' remove_remnants_from_behead(dat, 2)
#' }
remove_remnants_from_behead <- function(dat, first_data_row) {

  if (first_data_row > max(dat$row)) {
    stop("first_data_row is higher than the highest row number in the data.")
  }

  cleaned <- dat %>%
    filter(row >= first_data_row)

  return(cleaned)
}

#' @title Remove rows without a numeric value.
#'
#' @description When unpivoting data some rows may remain that have been used to
#' create a column (see explanation below), but are not part of the required
#' information for that column. This function identifies which they are and
#' removes them.
#'
#' The rows to remove are identified as those for which there are NAs in all the
#' columns that have been created AND the numeric column (we don't want to
#' accidentally delete data).
#'
#' Explanation of how the rows that this function removes are created:
#' Sometimes rows in the xlsx_cells data refer to a column in the raw data
#' that holds useful information that is not part of the main table, and we want
#' to include that info in our output. e.g. if a column to the right of the data
#' contains notes information (as in DLUHC CPR), it should be beheaded in such a
#' way that it gets associated with a numeric row in the xlsx_cells dataframe:
#' In the pub sec settings we would use column_to_right_of_data_name_pattern to
#' associate those character strings to the correct values. This function tidies
#' up after that association is made, by removing the rows related to that
#' column from the unpiovotted data.
#'
#' @param dat dataframe containing the unpivotted data
#' @param columns_to_create character string vector of column names.
#' @param header_split_to character string vector of column names. Often NA.
#'
#' @returns dataframe dat with unwanted rows removed.
#'
#' @examples
#' \dontrun{
#' columns_to_create <- "all_headers"
#' header_split_to <- c("description_1", "description_2")
#' dat <- data.frame(
#'   "numeric" = c(1, 2, NA, NA),
#'   "notes" = c("note 1", "note 2", "note 1", "note 1"),
#'   "description_1" = c(NA, "this", NA, NA),
#'   "description_2" = c("this", "that", NA, NA),
#'   "character" = c(NA, NA, "note 1", "note 2")
#' )
#' }
remove_non_numeric_rows <- function(dat, columns_to_create, header_split_to) {

  candidate_columns <- c(columns_to_create, header_split_to, "numeric")
  cols_to_check_for_na <- candidate_columns[candidate_columns %in% names(dat)]

  dat <- mutate(dat, count = 0)
  for (i in 1:length(cols_to_check_for_na)) {
    dat <- dat %>%
      mutate(count = ifelse(
        is.na(!!sym(cols_to_check_for_na[i])),
        count + 1,
        count
      ))
  }

  nas_in_all_removed <- dat %>%
    filter(count != length(cols_to_check_for_na)) %>%
    select(-count)

  return(nas_in_all_removed)

}

