#' @title set_global_variables
#' @description  Un-pivot data with one row of any number of columns
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
# Get the current date and time, start f logfile with the datetime as part of the name
#' @examples start_debug_logfile()
set_global_variables <- function() {
  
  # Extracting arguments
  input_filepath <<- my_args[2]
  tab_regex <<- my_args[3]
  output_filepath <<- my_args[4]
  current_location <<- my_args[5]
  sheet_structure_json <<- my_args[6]
  layout_json <<- my_args[7]
  file_part <<- my_args[8]
  tab_regex_front_page <<- "((?i)\\bfront[_ ]*page\\b)"
  
  # Pull in the R functions
  setup_utils_path <<- file.path(current_location, "setup_utils.r")
  preprocessing_utils_path <<- file.path(current_location, "preprocessing_utils.r")
  formatting_path <<- file.path(current_location, "formatting.r")
  formatting_multi_table_path <<- file.path(current_location, "formatting_multi_table.r")
  
  # Get info about the source from file path
  directory <<- dirname(input_filepath)
  supplier <<- basename(dirname(directory))
  source_group <<- basename(directory)
  filepath_split <- str_split(input_filepath, "-")[[1]]
  dataset <<- filepath_split[length(filepath_split) - 1]
  
  # Find sheet names of spreadsheet
  sheets <<- tidyxl::xlsx_sheet_names(input_filepath)
  message(paste("input_filepath:", input_filepath))
  message(paste("tab_regex:", tab_regex))
  message(paste("output_filepath:", output_filepath))
  message(paste("current_location:", current_location))
  message(paste("sheet_structure_json:", sheet_structure_json))
  message(paste("layout_json:", layout_json))
  message(paste("file_part:", file_part))
}


