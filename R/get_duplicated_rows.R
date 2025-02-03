#' @title get_duplicated_rows
#' @description  Get the row numbers of duplicated rows
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details
#' Get the row numbers of duplicated rows
#'
#' Data imported with tidyxl::xlsx_cells have one row for each cell. As such
#' the usual ways of finding duplicated rows in the raw data (e.g `unique` or
#' `distinct`) do not work.
#'
#' Given a row number, find any rows that are duplicates of that row.
#'
#' @param dat tidyxl::xlsx_cells dataframe
#' @param row_num integer. The row you want to find duplicates of
#' @returns integer vector. The row numbers of duplicated rows
#'          (but not the original row: row_num).
#'
#' @examples
#' example_dat <- data.frame(
#'   row = rep(1:4, each = 2),
#'   col = rep(1:2, by = 4),
#'   character = rep(c("year", "quarter", rep(NA, 2)), 2),
#'   numeric = c(NA, NA, 1, 2, NA, NA, 3, 4)
#' )
#'
#' get_duplicated_rows(example_dat, 1)
#' @export
get_duplicated_rows <- function(dat, row_num) {

  raw_colnames <- dat$character[dat$row==row_num]
  raw_colnames_no_na <- raw_colnames[!is.na(raw_colnames)]

  cells_by_row <- dat %>%
    filter(character %in% raw_colnames_no_na) %>%
    group_by(row) %>%
    count()

  duplicated_rows <- cells_by_row %>%
    filter(n == length(raw_colnames_no_na) &
             row != row_num) %>%
    pull(row)

  return(duplicated_rows)
}
