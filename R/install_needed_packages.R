#' @title install_needed_packages
#' @description  TBD
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details install some libraries - probably obsolete
# TBD
#' @examples start_debug_logfile()
install_needed_packages <- function(){
  install.packages("D:/coding_repos/tidysheet_1.0.tar.gz", repos = NULL, type="source")
  library("tidysheet")
  # install and load packages ----------------------------------------------------
  packages <- c("dplyr", "tidyr", "stringr", "tidyxl", "unpivotr")
  #install_absent_packages(packages)

  suppressMessages(
    for (p in packages) {
      library(p, character.only = TRUE)
    }
  )

}
