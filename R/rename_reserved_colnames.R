#' @title rename_reserved_colnames
#' @description If a column in the data has a name reserved for something else, rename it.
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' If a column in the data has a name reserved for something else, rename it.
#'
#' Column names that match a string listed in reserved_names are given a 
#' postscript.
#'
#' @param dat dataframe
#' @param reserved_colnames  vector of column names (strings) that we don't want 
#' to exist yet in dat (they are reserved for some other future column).
#'
#' @return dat with offending columns renamed
#' @examples
#' dat <- data.frame("group" = c("Fox", "Bear"),
#'                   "value" = c("red", "black"),
#'                   "numeric" = 1:2)
#' newdat <- rename_reserved_colnames(dat, c("value", "source"))
#' @export
rename_reserved_colnames <- function(dat, reserved_names) {
  
  for (item in reserved_names) {
    if (item %in% names(dat)) {
      dat <- rename(dat, !!sym(paste0(item, "_1")) := !!sym(item))
    }
  } 
  return(dat)
}