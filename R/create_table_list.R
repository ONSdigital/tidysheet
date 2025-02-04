#' @title create_table_list
#' @description Create a list of tables by segmenting header rows based on column positions.
#' @author Emma Wood \email{Emma.Wood@ons.gov.uk}
#' @details
#' Create a list of tables by segmenting header rows based on column positions.
#'
#' This function loops through the starting column positions of tables and extracts
#' segments from `header_rows_combined`, storing them as individual tables in a list.
#'
#' @param header_rows_combined A dataframe containing column positions and other relevant data.
#' @param table_start_cols A numeric vector specifying the starting column indices for each table.
#' @param table_count An integer representing the number of tables to extract.
#'
#' @return A list of dataframes, each containing a subset of `header_rows_combined` corresponding to a table.
#'
#' @examples
#' header_rows_combined <- data.frame(
#'   "row" = c(1, 1, 1, 2, 2, 2),
#'   "col" = c(1, 2, 3, 1, 2, 3),
#'   "address" = c("A1", "B1", "C1", "A2", "B2", "C2"),
#'   "character"= c(NA,NA,NA,"Education Services","NA","NA")
#' )
#' table_start_cols <- 1
#' table_count <- 1
#' table_list <- create_table_list(header_rows_combined, table_start_cols, table_count)
#' @export
create_table_list <- function(header_rows_combined, table_start_cols, table_count) {

  table_list <- vector(mode = "list", length = table_count)

  for(i in seq_along(table_start_cols)) {
    if (i == length(table_start_cols)) {
      next_table_start <- max(header_rows_combined$col) + 1
    } else {
      next_table_start <- table_start_cols[i + 1]
    }

    table_list[[i]] <- header_rows_combined %>%
      filter(col >= table_start_cols[i] & col < next_table_start)

  }
  if (!is.list(table_list)) warning("table_list is not a list!, header_rows_combined has not been succesfully turned to list, please contact developer.")
  return(table_list)
}
