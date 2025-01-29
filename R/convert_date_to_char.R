#' @title convert_date_to_char
#'
#' @description
#' convert dates to character strings
#' if the data_type is 'date' and the date column is not NA
#' Update the data_type and date columns to be consistent.
#'
#' @param source_df dataframe
#' @return transformed_date_df dataframe
#'
#' @examples
#' convert_date_to_char(source_df)
#' @export
convert_date_to_char <- function(source_df) {

  if (any(!is.na(source_df$date))) {

    transformed_date_df <- source_df %>%
      mutate(character = case_when(
        data_type == "date" ~ as.character(date),
        TRUE ~ character
      )) %>%
      mutate(date = NA,
             data_type = case_when(
               data_type == "date" ~ "character",
               TRUE ~ data_type
             ))
  } else{

    transformed_date_df <- source_df
  }

  return(transformed_date_df)
}
