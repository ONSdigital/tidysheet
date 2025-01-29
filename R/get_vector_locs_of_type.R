#' @title get_vector_locs_of_type
#' @description  Get the locations of columns or rows of a given datatype
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' Get the locations of columns or rows of a given datatype
#' 
#' xlsx_cells data are given a row per cell. Column datatypes for the original
#' columns are not, therefore, assigned on import.
#' 
#' This function returns the vector of original column numbers for which 
#' a given datatype is present for a given proportion of rows.
#' 
#' @param dat xlsx_cells datafarame.
#' @param datatype character. the data_type of interest. One of:
#'     'numeric', 'character', 'logical', 'date', 'blank', or 'error'
#' @param tolerance integer. 0-1 inclusive. The proportion of cells in a column 
#'     that need to be of the given datatype for a column to be returned.
#'     
#' @examples 
#' 
#' dat looks like this in excel
#' | 1 | A | 1 | 1 |
#' | 2 | B | 2 | 2 |
#' | 3 | C | 3 | 3 |
#' | 4 | D |   |   |
#' | 5 | E |   |   |
#' 
#' 
#' dat <- data.frame(
#'   "col" = rep(1:4, each = 5),
#'   "row" = rep(1:5, times = 4),
#'   "data_type" = c(
#'     rep("numeric", 5),
#'     rep("character", 5), 
#'     rep(c(rep("numeric", 3), rep("blank", 2)), times = 2)
#'     ),
#'   "character" = c(rep(NA, 5), LETTERS[1:5], rep(NA, 10)),
#'   "numeric" = c(1:5, rep(NA, 5), 
#'                 rep(c(1:3, rep(NA, 2)), times = 2))
#'   )
#' 
#' likely_number_rows <- get_vector_locs_of_type(dat, "numeric", 0.6)
#' definite_number_cols <- get_vector_locs_of_type(dat, "numeric", 1, "col")
#' number_rows <- get_vector_locs_of_type(dat, "numeric", 0.5, "row")
#' 
get_vector_locs_of_type <- function(dat, datatype, tolerance, direction="col") {
  
  if (between(tolerance, 0, 1) == FALSE) {
    stop("tolerance must be between 0 and 1")
  }
  
  if (direction == "col") {
    opposite_direction <- "row"
  } else {
    opposite_direction <- "col"
  }
  
  data_length <- nrow(distinct(ungroup(dat), !!sym(opposite_direction)))
  
  # in some datasets, characters are used in place of numeric data to indicate
  # suppressed values. These need to be counted as numeric rather than character
  characters_classed_as_numeric <- c("[z]", "-")
  
  numeric_characters_corrected <- dat %>% 
    mutate(data_type = 
             ifelse(
               character %in% characters_classed_as_numeric,
               "numeric",
               data_type))
  
  locations <- numeric_characters_corrected %>% 
    filter(data_type == datatype) %>% 
    group_by(!!sym(direction)) %>% 
    count() %>% 
    filter(n >= tolerance*data_length) %>%   
    pull(!!sym(direction))
  
  return(sort(locations))
  
}

