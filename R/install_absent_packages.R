#' @title install_absent_packages
#' @description install packages that are not already installed
#' @details value
#' Takes a vector of package names that you need and installs
#' any packages that are not already installed. Returns a message to say all are
#' already installed, or installs any absent packages, with the usual 
#' install.packages messages.
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @param packages. A vector of packages you want to install if they are not already
#' @return NULL
#' @examples
#' install_absent_packages(c("dpyr", "tidyr"))
#'
#' @export
install_absent_packages <- function(packages) {
  message("new function")
  
  absent_packages <- identify_absent_packages(packages)
  
  if (length(absent_packages) > 0) {
    install.packages(absent_packages,
                     dependencies = TRUE, 
                     type = "win.binary")
  } else {
    message("all packages already installed")
  }
}