#' @title split_original_header
#' @description  Split sub-service and transaction information into their own columns
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details 
#' Split sub-service and transaction information into their own columns
#' 
#' Some headers contain both grouping variable and nested variable names. In 
#' these cases we want to split the information into separate columns.
#' split_points are used to determine where each split should occur. 
#' The order these are given is important: the first col_split_point separates
#' group_col from nested_col_2. If that pattern does not exist, it is assumed that
#' nested_col_2 should be a copy of group_col. The second col_split_point separates
#' nested_col_2 from nested_col_1.
#' 
#' This is e.g. the case for for sub-service (grouping var) and transaction 
#' (nested under sub-service) dluhc revenue expenditure final data from
#' 2021-22. 
#' 
#'
#' @param dat dataframe
#' @param columns vector of strings giving (in order) New column names for 
#' group_col, nested_column_1, and nested_column_2 (nested_column_2 is optional). 
#' @param split_points vector of strings giving (in order) the regex for 
#' where to split the original header to give group_col, nested_col_1 and 
#' (optionally) nested_column_2. The first col_split_point separates
#' group_col from nested_col_2. The second splits nested_col_2 from nested_col_1
#' @param original_header. string. Column name containing the info we want to 
#' put in the new columns 
#' 
#' @return tibble. Tidy dataframe. 
#'
#' @examples
#' dat <- data.frame(
#'   "original_header" = c("Education:\n secondary_education\r\n � thousand",
#'                         "Education\n � thousand")
#' )
#' columns <- c("service", "units", "subservice")
#' split_points <- c( "\\:\\s*(\r\n|\r|\n)", "(\r\n|\r|\n)\\s*\\�")
#' result <- split_original_header(dat, columns, split_points, "original_header")
#'
#' @export
split_original_header <- function(dat, columns, split_points, original_header) {
  
  columns <- as.vector(na.omit(columns))
  
  group_col <- columns[1]
  nested_column_1 <- columns[2]
  
  if (length(columns) > 2) {
    nested_column_2 <- columns[3]
  }
  
  split_to_use <- dat %>% 
    mutate(original_header = str_to_lower(!!sym(original_header))) %>% 
    # some columns in dluhc capital expenditure final 2022-23 have only got
    # group_col and nested_col_1, but no nested_col_2. The first col_split_point
    # is not used ofr these strings so we need to treat these lines differenctly
    mutate(initial_split = str_split_fixed(original_header, split_points[1], 2)) %>% 
    mutate(use_split_point_1 = case_when(
      initial_split[,2]==""|is.na(initial_split[,2]) ~ FALSE,
      TRUE ~ TRUE
    )) %>% 
    select(-c(initial_split))
  
  group_col_extracted <- split_to_use %>% 
    mutate(original_header = str_to_lower(!!sym(original_header))) %>% 
    mutate(!!sym(group_col) := case_when(
      use_split_point_1==TRUE ~ 
        str_split_fixed(original_header, split_points[1], 2)[,1],
      use_split_point_1==FALSE ~ 
        str_split_fixed(original_header, split_points[2], 2)[,1])) %>% 
    mutate(header_remaining = ifelse(
      !is.na(original_header),
      suppressWarnings(
        # `fixed` used so that if there are braces in the group_col string, 
        # they are not treated as regex special characters
        str_remove(original_header, fixed(!!sym(group_col)))
      ),
      NA
    )) %>% 
    mutate(header_remaining = trim_separators(header_remaining, c(":","-"))) %>% 
    mutate(!!sym(group_col) :=
             ifelse(
               !!sym(group_col) == "",
               NA,
               str_squish(!!sym(group_col))
             )
    )
  
  if (length(split_points) > 1) {
    
    nested_columns_extracted <- group_col_extracted %>% 
      mutate(!!sym(nested_column_2) := case_when(
        use_split_point_1 == TRUE ~
          str_split_fixed(header_remaining, split_points[2], 2)[,1],
        use_split_point_1 == FALSE ~ !!sym(group_col))
      ) %>% 
      mutate(!!sym(nested_column_1) := 
               suppressWarnings(
                 str_remove(
                   # `fixed` used so that if there are braces in the 
                   # nested_column_2 string, they are not treated as special 
                   header_remaining, fixed(!!sym(nested_column_2))
                 )
               )
      ) %>% 
      # clean up the output by removing hyphens and colons from the start and end
      mutate(!!sym(nested_column_1) := 
               trim_separators(!!sym(nested_column_1), c(":","-", enc2utf8("\u00A3")))
      ) %>% 
      mutate(!!sym(nested_column_2) := 
               trim_separators(!!sym(nested_column_2), c(":","-", enc2utf8("\u00A3")))
      ) %>% 
      # if there is nothing after the split, the result should be NA, not ""
      mutate(!!sym(nested_column_1) := 
               ifelse(
                 !!sym(nested_column_1) == "",
                 NA,
                 !!sym(nested_column_1)
               )
      ) %>% 
      select(-header_remaining)
    
  } else {
    nested_columns_extracted <- group_col_extracted %>% 
      rename(!!sym(nested_column_1) := header_remaining) %>% 
      # when there are 3 columns and 2 splits, the second entry equals the first 
      # if there is no split pattern, so replicate that behavior here
      # This makes the validation more robust, as validation does not like NAs
      mutate(!!sym(nested_column_1) := 
               ifelse(
                 is.na(!!sym(nested_column_1)) & !is.na(!!sym(group_col)),
                 !!sym(group_col),
                 !!sym(nested_column_1)
               )
      )
  }    
  
  output <- nested_columns_extracted %>% 
    select(-c(use_split_point_1)) %>% 
    # if no split patterns were found in the original header, there will be no
    # information in the `columns`. I think it is better to fill the first column
    # with the original header than to leave it blank. This then replicates what
    # happens with three columns for data with two columns, i.e. the last 
    # column can contain an NA but previous columns cannot
    mutate(!!sym(group_col) := 
             ifelse(
               is.na(!!sym(group_col)) & is.na(!!sym(nested_column_1)),
               !!sym(original_header),
               !!sym(group_col)
               #     ),
               # !!sym(nested_column_1) := 
               #   ifelse(
               #     is.na(!!sym(group_col)) & is.na(!!sym(nested_column_1)),
               #     !!sym(original_header),
               #     !!sym(nested_column_1)
             )
    )
  
  return(output)
  
}