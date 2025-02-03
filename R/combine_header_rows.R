#' @title combine_header_rows
#' @description  Combine header rows that are meant to be one row.
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details
#' Combine header rows that are meant to be one row.
#'
#' Use patterns passed from the data dictionary to find the first and last of
#' the rows that need to be combined into a single value (per column), and
#' concatenate those values.
#'
#' This was written to address an issue in the England NNDR and council tax
#' data (the diff spreadsheet).
#'
#' @param dat Tidyxl format dataframe (Data imported with tidyxl::xlsx_cells,
#' which have one row for each cell).
#' @param combine_start_row_identifier (str). A word or pattern that first
#' appears in the first of the header rows that needs to be combined.
#' @param combine_endt_row_identifier (str). A word or pattern that first
#' appears in the last of the header rows that needs to be combined.
#'
#' @return dat with rows up to the end_row_identifier row concatenated onto the
#' start_row_identifier row for each column. Of the concatenated rows, only the
#' first is kept, the others are removed from the data.
#'
#' @examples
#'
#' |      || this    || another |
#' |      ||---------||---------|
#' |      || is one  || heading |
#' |      ||---------||---------|
#' | year || heading || here    |
#' |------||---------||---------|
#' | 2020 ||   1162  ||  1034   |
#'
#' The data shown above is input by xlsx_cells to look like this (order is important):
#'
#' dat <- data.frame(
#'     "row" = c(1, 1, 2, 2, 3, 3, 3, 4, 4, 4),
#'     "col" = c(2, 3, 2, 3, 1, 2, 3, 1, 2, 3),
#'     "character" = c( "this", "another", "is one", "heading", "year", "heading", "here", NA, NA, NA),
#'     "numeric" = c(rep(NA, 7), 2020, 1162, 1034),
#'     "data_type" = c(rep("character", 7), rep("numeric", 3))
#' )
#'
#' result <- combine_header_rows(dat, "this", "here")
#'
#' # view in a normal layout (this will reintroduce removed rows as blanks):
#' rectify(result)
#' @export
combine_header_rows <- function(dat, combine_start_row_identifier, combine_end_row_identifier){

  start_row <- get_first_instance(dat, combine_start_row_identifier)
  end_row <- get_first_instance(dat, combine_end_row_identifier)
  rows_to_combine <- seq(start_row, end_row, by=1)
  rows_to_remove_after_combine <- rows_to_combine[2:length(rows_to_combine)]

  combined_header_cells <- dat %>%
    filter(row %in% rows_to_combine) %>%
    group_by(col) %>%
    mutate(character_to_add = paste0(character, collapse = " ")) %>%
    # we only need one header row (of the rows being combined) now they have been
    # combined, so just keep one and label them all as being on the same row
    filter(row_number()==1)

  # put the new headers back into the main dataset without chanding the order
  combined_headers_in_dat <- dat %>%
    left_join(combined_header_cells, by = names(dat))

  output <- combined_headers_in_dat %>%
    mutate(character = case_when(
      !is.na(character_to_add) ~ as.character(character_to_add),
      TRUE ~ as.character(character))) %>%
    filter(row %in% rows_to_remove_after_combine == FALSE |
             # make sure we don't lose any headers that were on a row that has been
             # combined in other columns (e.g. 'year' in the example)
             (row %in% rows_to_remove_after_combine & character==character_to_add)) %>%
    select(-character_to_add)

  return(output)
}
