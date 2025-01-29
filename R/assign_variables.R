#' @title assign_variables
#' @description  Puts command line arguments into an environment and passes it back to the main code
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details Puts command line arguments into an environment and passes it back to the main code
#' Assign Variables to Environment : this can be broken out to a library when we refactor 
#' to use our own R package.
#' This function creates a new environment and assigns variables from the provided arguments.
#'
#' @param my_args A character vector containing the arguments. The first element should be "--args",
#' and the subsequent elements should be the actual arguments.
#' @return An environment containing the assigned variables.
#' @examples
#' my_args <- c("--args", "input/file/path", "tab_regex", "output/file/path", "current/location",
#'              "sheet_structure_json", "layout_json", "file_part")
#' my_env <- assign_variables(my_args)
#' @export
assign_variables <- function(my_args) {
  # Create a new environment to store variables
  my_env <- new.env()
  
  # Assign arguments to the environment
  args_names <- c("unused", "input_filepath", "tab_regex", "output_filepath", "current_location", 
                  "sheet_structure_json", "layout_json", "file_part")
  
  message("Applying the command line arguments...")
  for (i in seq_along(args_names)) {
    assign(args_names[i], my_args[i], envir = my_env)
    message(paste(args_names[i], ":", get(args_names[i], envir = my_env)))
  }
  
  # Assign additional variables to the environment
  assign(x = "tab_regex_front_page", value = "((?i)\\bfront[_ ]*page\\b)", envir = my_env)
  
  # Get info about the source from file path and assign to the environment
  assign("directory", dirname(my_env$input_filepath), envir = my_env)
  assign("supplier", basename(dirname(my_env$directory)), envir = my_env)
  assign("source_group", basename(my_env$directory), envir = my_env)
  assign("filepath_split", str_split(my_env$input_filepath, "-")[[1]], envir = my_env)
  assign("dataset", my_env$filepath_split[length(my_env$filepath_split) - 1], envir = my_env)
  
  # List of dictionary items to be used later
  assign(x = "dict_items", value = c(
    "table_heading", 
    "subtable_heading", 
    "subtable_regex", 
    "subtable_layout", 
    "subtable_header_identifier", 
    "group_col", 
    "nested_col_1", # renamed from nested_column_1
    "nested_col_2", # renamed from nested_column_2
    "dropdown_regex", 
    "group_row", 
    "nested_row_1", 
    "nested_row_2",
    "horizontal_split_identifier", 
    "left_headers", 
    "POSIX_column", 
    "group_row_na_identifier", 
    "combine_start_row_identifier", 
    "combine_end_row_identifier", 
    "headers_to_combine", 
    "extend_row_regex", 
    "extend_row_order", 
    "extend_row_with", 
    "col_join_name", 
    "col_join_pairs_1", 
    "col_join_pairs_2",
    "header_identifier",  # renamed from header_identifiers
    "second_identifier",  # renamed from second_identifiers
    "table_split_points", # renamed from table_split_instructions
    "split_points"        # renamed from split_instructions
  ), envir = my_env)
  
  # return the environment for the main code to use with the variables loaded
  return(my_env)
}