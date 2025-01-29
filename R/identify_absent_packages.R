#' @title identify_absent_packages
#' @description identify packages that are not already installed
#' @details value
#' identify packages that are not already installed
#'
#' Takes a vector of package names that you need and returns a list of packages 
#' that are not already installed
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @param packages. A vector of packages you want to install if they are not already
#' @return absent_packages A vector of packages not installed
#'
#'
#' @param packages. A vector of packages you want to know if they are installed
#'
#' @examples
#' absent_packages <- identify_absent_packages(c("noSuchPackager", "tidyr"))
#'
#' @name identify_absent_packages
#' @export
identify_absent_packages <- function(packages) {
  
  absent_packages <- setdiff(packages, rownames(installed.packages()))
  return(absent_packages)
  
}