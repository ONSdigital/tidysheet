#' @title install_needed_packages
#' @description  Un-pivot data with one row of any number of columns
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
# Get the current date and time, start f logfile with the datetime as part of the name
#' @examples start_debug_logfile()
install_needed_packages <- function(){
  install.packages("D:/coding_repos/cleanr_0.1.0.tar.gz", repos = NULL, type="source")
  library("cleanr")
  # install and load packages ----------------------------------------------------
  packages <- c("dplyr", "tidyr", "stringr", "tidyxl", "unpivotr")
  #install_absent_packages(packages)
  
  suppressMessages(
    for (p in packages) {
      library(p, character.only = TRUE)
    }
  )
  
}