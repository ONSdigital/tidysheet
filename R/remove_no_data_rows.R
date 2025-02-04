#' @title remove_no_data_rows
#' @description TBD
#' @details
#' Remove rows without data
#'
#' Return the original dataframe minus the rows that only have NAs in the
#' columns specified in col_names
#'
#' Rows are only removed if all col_names have NAs in the same row.
#' Error raised if input is not a dataframe.
#' Warning is given if any rows are removed
#'
#'
#' @param df Dataframe
#' @param col_names character vector between length 1 and 3. The quoted names of
#' columns used to identify rows that need to be removed
#'
#' @return Character vector or NA.
#'
#' @examples
#' test_df <- data.frame("A" = c(1, 2, NA), "B" = c("a", "b", NA))
#' new_df <- remove_no_data_rows(test_df, c("A", "B"))
#' print(new_df)
#'
#' @export
remove_no_data_rows <- function(df, col_names) {

  if (!is.data.frame(df)) {

    stop("The Input is not a dataframe")
  }

  # Check if the column names are valid
  if (any(is.character(col_names)) == FALSE) {
    stop("Column names should be strings.")
  }

  if (length(col_names) < 1) {
    return(df)
    warning("no col_names provided: data has not been assessed to see if data was entered outside of the table bounds")
  }

  if (all(col_names %in% names(df)) == FALSE) {
    stop("Invalid column name")
  }

  if (any(duplicated(col_names)) == TRUE) {
    stop("Column names in col_names should all be different.")
  }

  col_names_as_sym <- syms(col_names)

  # Count the number of rows with mutual NA values in the specified columns
  # I'm sure there is a better way to do this than with if statements, but
  # I don't think we will ever need more than 3 columns to base deletion on
  if (length(col_names) == 1) {

    cases_identified <- df %>%
      dplyr::mutate(remove =
                      ifelse(
                        is.na(!!sym(col_names[1])) | !!sym(col_names[1]) == "NA",
                        TRUE, FALSE))

  } else if (length(col_names) == 2) {

    cases_identified <- df %>%
      dplyr::mutate(remove =
                      ifelse(
                        (is.na(!!sym(col_names[1])) | !!sym(col_names[1]) == "NA") &
                          (is.na(!!sym(col_names[2])) | !!sym(col_names[2]) == "NA"),
                        TRUE, FALSE))

  } else if (length(col_names) >= 3) {

    cases_identified <- df %>%
      dplyr::mutate(remove =
                      ifelse(
                        (is.na(!!sym(col_names[1])) | !!sym(col_names[1]) == "NA") &
                          (is.na(!!sym(col_names[2])) | !!sym(col_names[2]) == "NA") &
                          (is.na(!!sym(col_names[3])) | !!sym(col_names[3]) == "NA"),
                        TRUE, FALSE))

  }

  if (length(col_names) > 3) {
    warning("More than 3 col_names were provided. Only the first 3 were used")
  }

  num_removed <- cases_identified %>%
    dplyr::filter(remove == TRUE) %>%
    nrow()

  # Remove rows with mutual NA values in the specified columns
  df_filtered <- cases_identified %>%
    dplyr::filter(remove == FALSE) %>%
    dplyr::select(-remove)

  # Print the number of removed rows
  if (num_removed > 0){
    warning(paste0(num_removed, " blank entries removed - these may have been caused by cells with entries outside the bouds of the table"))
  }

  # Return the filtered data frame
  return(df_filtered)
}
