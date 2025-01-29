#' @title get_matching_colnames
#' @description  TBD
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details
#' Get column names that contain a pattern
#'
#' @param dat dataframe
#' @param pattern string. Regular expression you want to match
#' @returns vector of strings. names of the col names the pattern is found in
#'
#' @example
#' dat <- data.frame(fin_year_end = "2020", "year" = ("2021"), "other" = NA)
#' get_matching_colnames(dat, "year")
#'
#'@export
get_matching_colnames <- function(dat, pattern){

  # look for a string match within cleaned column names so that changes to
  # capitalisation and spacing, and extr info in the column name dont matter
  names_cleaned <- str_squish(tolower(names(dat)))

  matched_names <- grepl(pattern, names_cleaned)
  col_name <- names(dat)[matched_names]

  return(col_name)
}
