#' @title start_debug_logfile
#' @description  Un-pivot data with one row of any number of columns
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
# Get the current date and time, start f logfile with the datetime as part of the name
#' @examples start_debug_logfile()
start_debug_logfile <- function(){
  current_datetime <- Sys.time()
  # Format the datetime as a numeric string (YYYYMMDD_HHMMSS)
  numeric_datetime <- format(current_datetime, "%Y%m%d_%H%M%S")
  
  # Create a filename using the numeric datetime
  filename_R_log <- paste0("D:/coding_repos/R_", numeric_datetime, ".log")
  
  
  con <- file(filename_R_log)
  sink(con, append=TRUE)
  sink(con, append=TRUE, type="message")
  message("Start of R Log ")
  message("We are in revenue_expenditure")
  
}