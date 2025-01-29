helper_script_where_is_function_used <- function(fn, file_list){
  # pass in a list of files (fully addressed) and a function. Shows where it is used
  pattern_no_ref <- paste0("[^:]",fn, "[ ]{0,1}\\(")
  pattern_ref <- paste0("tidysheet::",fn, " *\\(")
  message("pattern_no_ref, ", pattern_no_ref, " pattern_ref:: ", pattern_ref, "\r\n")
  for (this_file in file_list){
    message("this_file, ", this_file)
    lines <- readLines(this_file)

    # remove commented out lines:
    lines <- grep("^[^#]", lines, value = TRUE)

    if (any(grepl(pattern_no_ref, lines))){
      message("No ref uses : ", fn, "\r\n")
      message("this_file, ", this_file, "\r\n")
      found_no_ref <- grep(pattern_no_ref, lines, value = TRUE)
      message("found_no_ref, ", found_no_ref)
    }

    if (any(grepl(pattern_ref, lines))){
      message("Ref uses : ", fn)
      message("this_file, ", this_file)
      found_ref <- grep(pattern_ref, lines, value = TRUE)
      message("found_ref, ", found_ref)
    }


  }
}
