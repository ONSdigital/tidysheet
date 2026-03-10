#' @title Runner for public sector preprocessing
#'
#' @description Runner script for pre-processing xlsx data for public sector
#' input data at the ONS. Tidy data so that there is only one numeric value per
#' row, with all other columns holding descriptive data for that value.
#' Metadata are added to the output as columns including year, year_type,
#' supplier, source, dataset, title, units, and vintage.
#'
#' The core variables are r_vars and sheet_structure. r_vars is a vector of all
#' names of the possible settings. sheet_structure is a flattened dictionary (a
#' string) that contains only the relevant variables (settings) for the specific
#' dataset, where the key is the name of the variable, and the value is the
#' value. Where an r_vars setting does not have a value in the sheet_structure
#' it is assigned NA.
#'
#' filepath must only contain a single year. Where the data are for
#' a single year/financial year, the year in filepath should be the year the
#' data refer to (i.e. not the publication date). The year given in the filepath
#' must be the correct format (financial or calendar year).
#'
#' @param arg_values character string vector. The values for the following
#' (order is important):
#' "--args", input filepath, regular expression matching the sheet name,
#' output filepath, settings (as a json), file part (integer, nearly always 1).
#' @param to_csv boolean. Default is TRUE. If FALSE the output is returned
#' rather than being saved to csv.
#'
#' @returns saved csv file. File is saved to the specified output_filepath.
#'
#' @examples
#' \dontrun{
#' # If picking up args from the command line:
#' arg_values <- commandArgs(trailingOnly = TRUE)
#'
#' # Without using the command line, this would look like e.g.:
#' arg_values <- c(
#'     "--args",
#'     "D:/some_file-2023_24.xlsx", "(?i)sheet1", "D:/some_file-2023_24.csv",
#'     "{header_identifier: (?i)proportion, columns_to_create: desc1, desc2,}",
#'     "1"
#'     )
#'
#' tidy_sheet(arg_values, FALSE)
#' }
#' @export
tidy_sheet <- function(arg_values, to_csv = TRUE) {

  arg_names <- c(
    "--args",
    "input_filepath", "tab_regex", "output_filepath",
    "sheet_structure_json", "file_part"
  )

  # Join the argument names to their values passed from subprocess.cal
  message("Assigning values from Python to variables:")
  for (i in seq_along(arg_names)) {
    assign(arg_names[i], arg_values[i])
    message(" - ", arg_names[i], ": ", arg_values[i])
  }

  variable_names <- get_variable_names()

  # json from Python comes into R as a string rather than as a dictionary that
  # has keys and values We first need to un-flatten it, to reinstate the
  # key:value structure.
  dict_full <- string_to_dict(sheet_structure_json)
  dict <- remove_invalid_args(dict_full, variable_names)

  # we do not yet have access to the values of variables in the dict as
  # it is passed to R as a dictionary. Variables that are not
  # present in the dict are assigned NA.
  for (variable in variable_names) {
    values <- get_dict_item(variable, dict)
    assign(variable, values)
    if (all(!is.na(values))) {
      message(" - ", variable, " : ", paste0(values, collapse = ", "))
    }
  }

  # Each variable in dict is only specified once, so where there are
  # multiple files to a dataset each variable may contain multiple arguments
  # with one set of arguments for each file (eg MHCLG/DLUHC revenue expenditure
  # bugdet which is now published in two parts, but historically was one file).
  # In such cases, the multifile_arg separator value separates the args for each
  # file, and file_part states which args to keep.
  if (is.na(file_part)) {file_part <- 1}
  message("Splitting arguments by file part.")
  dict_split_by_file <- split_args_by_separator(dict, multifile_arg_separator)
  file_dict <- select_relevant_args(dict_split_by_file, file_part)

  # We need to re-assign the values to variable names now that only the args for
  # the relevant file remain
  message("Assigning values to variables for relevant file part.")
  for (variable in variable_names) {
    values <- get_dict_item(variable, file_dict)
    original_values <- get_dict_item(variable, dict)
    assign(variable, values)
    if (all(!is.na(values)) & length(values) != length(original_values)) {
      message(" - ", variable, " : ", paste0(values, collapse = ", "))
    }
  }

  # Sometimes regular expressions wont pass through python via subprocess.run
  # because they get read as instructions. In such cases we need to write a
  # description that can be converted to a regular expression. However, if a
  # regular expression can be used it should be. As we don't know which patterns
  # need to be converted, we check them all, and just convert the ones that need
  # to be.
  pattern_names <- get_pattern_names()
  for (variable in pattern_names) {
    original_values <- get(variable)
    values <- build_regex(get(variable))
    if (all(original_values %in% values) == FALSE) {
      assign(variable, values)
      message(
        "Pattern '",  paste0(original_values, collapse = "', '"),
        "' translated to regular expression: '",
        paste0(values, collapse = "', '"), "'.")
    }
  }

  if (is.na(header_identifier)) {
    stop("A pattern is required for identifying the first header row.")
  }

  ### Get information from the filename as that has been standardised already---
  directory <- dirname(input_filepath)
  supplier <- basename(dirname(directory))
  source_group <- basename(directory)
  filepath_split <- str_split(input_filepath, "-")[[1]]
  dataset <- filepath_split[length(filepath_split) - 1]
  year_from_filename <- get_year(input_filepath)

  ### Import the data and get all required info from above the tables.----------
  source_data <- get_data(input_filepath, tab_regex)
  front_sheet <- get_data(input_filepath, tab_pattern_front_page)
  release_number <- get_release_number(front_sheet$character)

  ### remove cells--------------------------------------------------------------
  unwanted_cells_removed <- remove_unwanted_cells(source_data, cells_to_remove)

  # remove hidden character strings where the font is the same colour as the
  # background, these need to be removed so that un-pivotting works correctly.
  # e.g. In DLUHC in 2020-21 some white text is included in B6 that messes up
  # the beheading.
  hidden_chars_removed <- remove_hidden_character_strings(
    unwanted_cells_removed, input_filepath, hidden_character_strings_to_remove
  )

  # separate the info above the table from the main table-----------------------

  first_header_row <- get_header_row(
    hidden_chars_removed, header_identifier, header_identifier_instance,
    header_row_offset
  )

  main_table <- filter(hidden_chars_removed, row >= first_header_row)

  info_at_top_of_sheet <- get_info_above_table(
    hidden_chars_removed, first_header_row
    )

  # assume the title is in the first populated character cell until we come
  # across a dataset where this is not the case
  title <- source_data$character[!is.na(source_data$character)][1]

  year_for_column <- get_year_for_use_in_data(
    info_at_top_of_sheet$character, title, year_from_filename,
    use_year_from_filename_over_year_above_table,
    suppress_year_above_table_warning
    )

  # collect and check information above the table ---
  # In some DLUHC datasets we have to use the LA_dropdown tab. In these sheets
  # there is a dropdown at the top that controls what data are shown on the
  # sheet, and therefore what data are imported.
  check_dropdown(dropdown_pattern, info_at_top_of_sheet)
  main_dropdown_value <- get_dropdown_value(info_at_top_of_sheet, dropdown_pattern)

  # applied as a column in the data towards the end of the script
  units_patterns <- "((?i)thousand|(?i)000s)|(?i)million|(?i)(\\s|\\(|^)(count)(\\s|\\)|$)"
  sheet_units <- extract_units(info_at_top_of_sheet, units_patterns)

  ##############################################################################
  # everything from here relates to individual tables

  table_list <- get_tables_as_list(
    main_table, table_split_dirs, tables_to_split, table_split_patterns,
    table_split_pattern_instances, table_split_pattern_offsets,
    tables_to_process
  )

  if (length(table_list) < length(tables_to_process)) {
    stop("Fewer tables found than the number of tables set to be processed.")
  }

  # create a place to store all processed sub-tables
  all_tables <- NULL

  dict_split_by_table <- split_args_by_separator(
    file_dict, multitable_arg_separator
  )

  for (i in 1:length(table_list)) {

    if (length(table_list) > 1) {
      message("Processing table ", i, ".")
    } else {
      message("Processing single table.")
    }

    table_dict <- select_relevant_args(dict_split_by_table, i)

    free_from_reserved_words <- check_reserved_words(
      table_dict, c("table_split_dirs", exclude_from_reserved_word_check)
    )

    if (length(all.equal(table_dict, dict_split_by_table)) > 1){
      message(
        "Assigning values for table from the nesting dict to variables: "
      )
      for (variable in variable_names) {
        values <- get_dict_item(variable, table_dict)
        original_values <- get_dict_item(variable, file_dict)
        assign(variable, get_dict_item(variable, table_dict))
        if (all(!is.na(values))) {
          message(" - ", variable, " : ", paste0(values, collapse = ", "))
        }
      }
    }

    # build regex from patterns again, as it will only build the regex if all
    # elements of a variable start with ALT_, which, up till this point,
    # would not have been the case for any variables containing the multitable
    # arg separator.
    for (variable in pattern_names) {
      original_values <- get(variable)
      values <- build_regex(get(variable))
      if (all(original_values %in% values) == FALSE) {
        assign(variable, values)
        message(
          "Pattern '",  paste0(original_values, collapse = "', '"),
          "' translated to regular expression: '",
          paste0(values, collapse = "', '"), "'.")
      }
    }

    single_table <- table_list[[i]]

    subtable_names <- get_subtitles(
      single_table, subtable_title_column, subtable_title_patterns,
      subtitle_offset, subtitle_horizontal_index
    )
    # split info from above the table and the table itself
    table_first_header_row <- get_header_row(
      single_table, table_header_identifier,
      table_header_identifier_instance, table_header_row_offset
    )

    main_table <- get_table_data(single_table, table_first_header_row)

    info_above_table <- get_info_above_table(
      single_table, table_first_header_row
    )

    # get info from above the table
    table_units <- extract_units(info_above_table, units_patterns)

    table_dropdown_value <- get_dropdown_value(
      info_above_table, table_dropdown_pattern
    )
    table_metadata <- info_above_table$character
    table_title <- table_metadata[!is.na(table_metadata)][1]

    vintage <- get_vintage(
      single_vintage, release_number, title, table_title,
      info_at_top_of_sheet, info_above_table
    )

    # If table units is not blank use that, otherwise use sheet, otherwise NA.
    units <- get_units(sheet_units, table_units)

    # Where dates are given as headings, and this info appears in the date
    # column of xlsx_cells data we need to be able to access it in the character
    # column. (eg in MHCLG/DLUHC borrowing and investments)
    date_as_character <- convert_date_to_char(main_table)

    rownames_extended <- extend_row_value(
      main_table, extend_row_pattern, extend_row_order, extend_row_with
    )

    # without this step, if there is a blank column in the middle of the dataset,
    # the columns after the blank column will not get correctly beheaded.
    # This does not always remove 'blank' columns created through an editing
    # error by the suppliers (e.g. in dluhc revenue expenditure budget 2020-21),
    # because for some reason the cells are not seen as blank but as character
    # with a string of "..." - these are removed later by remove_no_data_rows
    no_empty_rows <- remove_empty_lines(rownames_extended, "row")
    no_empty_cols <- remove_empty_lines(no_empty_rows, "col")

    metadata_cell_removed <- remove_metadata_cells(
      no_empty_cols, metadata_cells_to_remove_patterns,
      populated_rows_to_check_for_metadata_to_remove
    )

    # Combine cells that have been artificially split by the supplier
    # e.g. in DLUHC council tax and NNDR
    headers_vertically_combined <- combine_rows_by_column(
      metadata_cell_removed, combine_start_row_identifier,
      combine_end_row_identifier
    )

    header_row_count <- length(columns_to_create)

    for_reformatting <- remove_columns(
      headers_vertically_combined, columns_to_remove_patterns,
      columns_to_remove_offset, table_first_header_row, header_row_count
    )

    unpivotted <- unpivot_data (
      for_reformatting,
      columns_to_create,
      table_first_header_row,
      tolerance,
      left_headers,
      minimum_number_of_consecutive_columns, right_block_offset,
      header_to_split, header_split_to, split_points,
      column_to_right_of_data_name_pattern,
      tidy_data,
      tidy_notes_name
    )

    reserved_names <- c(
      "title", "supplier", "source", "dataset", "value", "non_numeric_value"
      )
    names_protected <- rename_reserved_colnames(unpivotted, reserved_names)

    # ---fill missing info (rows and columns)
    blanks_filled <- fill_blanks(
      names_protected, fill_columns_column_names, fill_columns_fill_dirs
    )

    single_value_columns_added <- blanks_filled %>%
      add_single_value_columns(
        single_value_names, single_value_values, "single_value settings"
        ) %>%
      add_single_value_columns(
        dropdown_name, main_dropdown_value, "dropdown"
        ) %>%
      add_single_value_columns(
        table_dropdown_name, table_dropdown_value, "table dropdown"
        ) %>%
      add_single_value_columns(table_title_column, table_title, "table title")

    totals_as_column <- add_totals_as_column(
      single_value_columns_added, names(source_data), name_for_total_column,
      col_with_totals_pattern, total_column_fill_dir
      )

    with_subtable_names <- add_subtable_names(totals_as_column, subtable_names)

    row_headers_as_column <- add_row_headers_column(
      with_subtable_names, names(source_data), name_for_group_row_column,
      name_for_nested_row_column, col_with_row_headers_pattern,
      row_header_fill_dir, group_row_na_identifier
    )

    date_split <- split_date_to_columns(row_headers_as_column, POSIX_column)

    vintage_split <- split_year_and_vintage(
      date_split, vintage_with_year_column
      )

    metadata_added <- add_metadata_columns(
      vintage_split, title, table_title, units, vintage, supplier,
      source_group, dataset
    )

    # ---replace/edit wording
    strings_replaced <- replace_strings(
      metadata_added, replace_string_col, replace_string_from_col_patterns,
      replace_string_to, replace_string_keep_original
    )
    years_generalised <- standardise_mentioned_years(
      strings_replaced, descriptors_to_standardise_year_in, year_for_column$year
    )
    blanks_replaced <- replace_blanks(
      years_generalised, col_pattern_with_blanks_to_replace, col_pattern_to_replace_blanks_with
    )

    quarter_added <- blanks_replaced %>%
      add_quarter_column(quarter_from_col_pattern, quarter_col_name)

    fy_start_added <- quarter_added %>%
      get_fy_start(
        year_from_pattern, fy_from_fy_end, fy_start_from_fy_end, fy_end_pattern,
        calendar_year_to_fy_start, q1_is_jan_to_mar, quarter_col_pattern,
        month_col_pattern
      )

    year_col_cleaned <- process_year_column(
      fy_start_added, year_col_pattern, single_year_of_data, year_for_column,
      single_year_overrides_all, multi_year_range_is_not_valid
    )

    simple_fy_start_added <- fill_missing_fy_start(year_col_cleaned)

    # --- initial removal of rows that might not be identifiable once columns are joined
    NA_rows_removed <- drop_rows_with_NA(
      simple_fy_start_added, col_patterns_to_drop_NA_rows
    )

    # --- join columns
    columns_concatenated <- combine_columns(
      NA_rows_removed, columns_to_combine_patterns, columns_to_combine_combined_names,
      columns_to_combine_counts
    )

    # --- rename entries where the column name was duplicated
    # the names of the columns created by un-pivotting:
    cols_to_group_by <- setdiff(names(columns_concatenated), names(source_data))
    duplicates_renamed <- rename_duplicates(
      columns_concatenated, cols_to_group_by, rename_duplicate_in_col,
      rename_duplicate_pattern, rename_duplicate_index, rename_duplicate_prefix,
      rename_duplicate_expected_freq
    )

    # ---remove information (rows and columns)
    blanks_removed <- remove_is_blank_rows(duplicates_renamed)

    value_rows_removed <- drop_rows_with_values(
      blanks_removed, col_patterns_with_values_to_drop, value_patterns_to_drop
    )
    ignore_cols <- names(source_data)[names(source_data) != "numeric"]
    arrange_by_cols <- c("row", "col")[
      c("row", "col") %in% names(value_rows_removed)
    ]
    duplicates_removed <- deduplicate_data(
      value_rows_removed, ignore_cols, arrange_by_cols
    )
    xlsx_cells_columns_removed <- remove_unwanted_cols(duplicates_removed)

    line_breaks_removed <- remove_line_breaks(xlsx_cells_columns_removed)
    # --- rename columns

    columns_renamed <- rename_columns(
      line_breaks_removed, exclude_names = names(source_data),
      patterns = columns_to_rename_patterns, new_names = columns_to_rename_names
    ) %>%
      rename_value_columns() %>%
      # source referred to as source_group originally as source() is a function
      rename(source = source_group)

    names(columns_renamed) <- tolower(str_squish(names(columns_renamed)))

    check_for_duplicate_names(columns_renamed)

    all_tables <- bind_rows(all_tables, columns_renamed)
  }

  if (to_csv) {
    write.csv(all_tables, output_filepath, row.names = FALSE,
              fileEncoding = "UTF-8")
    message("File saved as ", output_filepath)
  } else {
    return(all_tables)
  }

}


#' @title Get the names of all possible arguments
#'
#' @description So that we can assign NA to any unspecified arguments, this
#' function provides a full list of all arguments.
#'
#' @returns character string vector of all arg names.
get_variable_names <- function() {

  arg_names <- c(
    "cells_to_remove",
    "hidden_character_strings_to_remove",
    "header_identifier", "header_identifier_instance", "header_row_offset",
    "use_year_from_filename_over_year_above_table",
    "suppress_year_above_table_warning",
    "dropdown_pattern", "dropdown_name", "table_dropdown_pattern",
    "table_dropdown_name",
    "table_split_dirs", "tables_to_split", "table_split_patterns",
    "table_split_pattern_instances", "table_split_pattern_offsets",
    "tables_to_process", "multitable_arg_separator",
    "exclude_from_reserved_word_check",
    "table_title_column",
    "subtable_title_column", "subtable_title_patterns", "subtitle_offset",
    "subtitle_horizontal_index",
    "table_header_identifier", "table_header_identifier_instance",
    "table_header_row_offset",
    "single_vintage", "vintage_with_year_column",
    "extend_row_pattern", "extend_row_order", "extend_row_with",
    "metadata_cells_to_remove_patterns",
    "populated_rows_to_check_for_metadata_to_remove",
    "combine_start_row_identifier", "combine_end_row_identifier",
    "columns_to_remove_patterns", "columns_to_remove_offset",
    "columns_to_create", "tolerance",
    "left_headers", "header_to_split", "header_split_to", "split_points",
    "column_to_right_of_data_name_pattern",
    "fill_columns_column_names", "fill_columns_fill_dirs",
    "single_value_names", "single_value_values",
    "name_for_total_column", "col_with_totals_pattern",
    "total_column_fill_dir",
    "name_for_group_row_column", "name_for_nested_row_column",
    "col_with_row_headers_pattern", "row_header_fill_dir",
    "group_row_na_identifier", "POSIX_column",
    "replace_string_col", "replace_string_from_col_patterns", "replace_string_to",
    "replace_string_keep_original",
    "descriptors_to_standardise_year_in",
    "col_pattern_with_blanks_to_replace", "col_pattern_to_replace_blanks_with",
    "rename_duplicate_in_col", "rename_duplicate_pattern",
    "rename_duplicate_index", "rename_duplicate_prefix",
    "rename_duplicate_expected_freq",
    "quarter_from_col_pattern", "quarter_col_name",
    "year_from_pattern", "fy_from_fy_end", "fy_start_from_fy_end",
    "fy_end_pattern",
    "calendar_year_to_fy_start", "q1_is_jan_to_mar", "quarter_col_pattern",
    "month_col_pattern", "year_col_pattern", "single_year_of_data",
    "year_for_column", "single_year_overrides_all", "multi_year_range_is_not_valid",
    "columns_to_rename_patterns", "columns_to_rename_names",
    "col_patterns_to_drop_NA_rows",
    "columns_to_combine_patterns", "columns_to_combine_combined_names",
    "columns_to_combine_counts",
    "col_patterns_with_values_to_drop", "value_patterns_to_drop",
    "multifile_arg_separator", "tab_pattern_front_page",
    "tidy_data", "tidy_notes_name",
    "minimum_number_of_consecutive_columns", "right_block_offset"
  )

  return(arg_names)
}


#' @title Get the names of variables that can be regular expressions
#'
#' @description So that we can convert pattern strings to regular expressions
#' for the correct variables (see build_regex).
#' Note that split_points is intentionally not included because the pattern
#' descriptions used to build the regular expression are themselves required
#' in the split_to_multiple_columns functions.
#'
#' @returns character string vector of variable names.
get_pattern_names <- function() {
  pattern_names <- c(
    "header_identifier", "dropdown_pattern", "table_dropdown_pattern",
    "table_split_patterns", "subtable_title_patterns",
    "table_header_identifier","extend_row_pattern",
    "metadata_cells_to_remove_patterns", "columns_to_remove_patterns",
    "column_to_right_of_data_name_pattern",
    "col_with_totals_pattern", "col_with_row_headers_pattern",
    "replace_string_from_col_patterns", "col_pattern_with_blanks_to_replace",
    "col_pattern_to_replace_blanks_with", "rename_duplicate_pattern",
    "quarter_from_col_pattern", "year_from_pattern",
    "fy_end_pattern", "quarter_col_pattern", "month_col_pattern",
    "year_col_pattern", "columns_to_rename_patterns",
    "col_patterns_to_drop_NA_rows", "columns_to_combine_patterns",
    "col_patterns_with_values_to_drop", "value_patterns_to_drop",
    "tab_pattern_front_page"
  )
  return(pattern_names)
}

