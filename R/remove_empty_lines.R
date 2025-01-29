#' @title remove_empty_lines
#' @description  remove empty columns or empty rows
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' remove empty columns or empty rows
#' 
#' @param dat Dataframe. Imported using xlsx_cells
#' @param column str. Either "row" or "col" 
#' @export
remove_empty_lines <- function(dat, column) {
  
  blanks_identified <- dat %>% 
    mutate(no_data = case_when(
      data_type == "blank" ~ TRUE,
      !is.na(formula) & numeric == 0 ~ TRUE,
      data_type == "character" & stringr::str_trim(character) == "" ~ TRUE,
      TRUE ~ FALSE
    )
    )
  
  empty_lines_removed <- blanks_identified %>% 
    group_by(!!sym(column)) %>% 
    mutate(col_all_blank = all(no_data == TRUE)) %>%
    filter(col_all_blank == FALSE) %>% 
    select(-c(col_all_blank, no_data)) %>% 
    ungroup()
  
  ncol_dat <- nrow(distinct(dat, !!sym(column)))
  ncol_new_dat <- nrow(distinct(empty_lines_removed, !!sym(column)))
  num_removed <- ncol_dat - ncol_new_dat
  
  if (column == "row") {
    remaining <- unique(empty_lines_removed$row)
    all <- unique(dat$row)
  } else if (column == "col") {
    # work out what the letters of the removed columns are (in the data they are recorded as numbers in the 'col' column, so we can get the letter from the address column instead)
    remaining <- unique(stringr::str_remove_all(empty_lines_removed$address, "[0-9]"))
    all <- unique(stringr::str_remove_all(dat$address, "[0-9]"))
  }
  removed <- paste0(setdiff(all, remaining), collapse = ", ")
  
  
  if (num_removed > 0){
    warning(paste0(num_removed, " empty ", column, "s removed from source data: ", removed))
  }
  
  return(empty_lines_removed)
}