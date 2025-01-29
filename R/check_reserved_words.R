#' @title check_reserved_words
#' @description checks if any reserved words are used, halts if they are
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details Check for reserved words in sheet_structure settings
#'
#' This function checks if any reserved words are used in the sheet_structure settings.
#' Reserved words can cause issues and should be avoided.
#' These keys need to avoid the reserved words, however, the function is written
#' to look at the items only, so new keys can be added.
#' group_col, nested_column_1 , nested_column_2, group_row, nested_row_1, 
#' nested_row_2, left_headers
#' Returns true if not, else halts. 
#' 
#' @param nesting_dict A dictionary containing the sheet structure settings.
#' @return A logical value indicating if reserved words are used in the sheet_structure settings.
#' @examples check_reserved_words(nesting_dict_without_reserved)
#' 
#' @export
check_reserved_words <- function(check_nesting_dict) {
  reserved_words <- c('date', 'sheet', 'address', 'row', 'col', 'is_blank', 'content', 'data_type',
                      'error', 'logical', 'numeric', 'character', 'character_formatted', 'formula',
                      'is_array', 'formula_ref', 'formula_group', 'comment', 'height', 'width',
                      'col_outline_level', 'style_format', 'local_format_id')
  flattened_list <- unlist(check_nesting_dict[2][[1]])
  
  found_words <- intersect(flattened_list, reserved_words)
  if (length(found_words)>0) {
    print(paste0('Reserved word ', found_words, ' used in dict, stopping'))
    stop('Reserved word used in dict, stopping')
  } else {
    # print('no reserved words found')
  }
  return(TRUE)
}
