#' @title rename_duplicates
#' @description TBD
#' @details
#-----------------------
#' Overwrite selected entries when two or more rows cannot be uniquely identified
#'
#' When there are two or more columns with the same name in the raw data, this
#' can result in rows of pre-processed data being indistinguishable from each
#' other except by their value.
#'
#' This function takes arguments given in the data dictionary to identify
#' which of the original columns should be renamed and what it/they should be
#' prefixed with. Any number of the repeats can be renamed e.g. if the raw data
#' contains two columns called total, the user may choose to rename only one
#' of them (and can choose which using rename_duplicate_index). Alternatively,
#' they could choose to rename both. See args_for_rename_duplicate.
#'
#' Only one set of repeats can be handled: If there are two lots of
#' duplicated names in the data e.g. two columns called 'total' and 3 called
#' 'subtotal', the function currently only allows one of those to be handled -
#' the column name of interest is identified by rename_duplicate_regex.
#'
#' Note: Whilst the entry that is renamed is a column in the raw data it is an
#' entry in a column in dat due to earlier un-pivotting.
#'
#' @param dat tibble of un-pivotted data. Must include a column with the name
#' given in rename_duplicate_in_col.
#' @param cols_to_group_by vector of strings giving the names of columns that will
#' have been created during un-pivotting. The entries in these columns of dat
#' would uniquely identify each row if there were no duplicate column names in
#' the raw data.
#' @param args_for_rename_duplicate named list. Must include the following:
#' - rename_duplicate_in_col (str) The name of the column the duplicate is in in dat.
#'       Vector of length 1.
#' - rename_duplicate_regex (str) The regex pattern that identifies the duplicate.
#'       Vector of length 1.
#' - rename_duplicate_index (int) Which to rename e.g. '2' would rename the 2nd.
#'       Can be a vector of length > 1, but if so the number of items must match
#'       that in rename_duplicate_prefix.
#' - rename_duplicate_expected_freq (int) The expected number of repeats of the
#'       column name in the raw data. Vector of length 1.
#' - rename_duplicate_prefix (str) The prefix that is to be pasted onto the
#'       original name. Can be a vector of length > 1, but if so the number of
#'       items must match that in rename_duplicate_index.
#'
#' @return dat with an additional column 'rename_note' and with the identified
#' duplicate renamed.
#'
#' @examples
#' dat <- data.frame(
#'   "item_detail" = rep(c("A", "A", "Total", "C", "Total", "Total"), 2),
#'   "year" = rep(c("2021-22", "2022-23"), each = 6),
#'   "numeric" = rep(c(2, 3, 5, 0.1, 5.1, 10.1), 2)
#' )
#' cols_to_group_by <- c("year", "item_detail", rep(NA, 3))
#' args_for_rename_duplicate <- list(
#'   rename_duplicate_in_col = "item_detail",
#'   rename_duplicate_regex = "(?i)total",
#'   rename_duplicate_index = c(1, 3),
#'   rename_duplicate_prefix = c("first", "third"),
#'   rename_duplicate_expected_freq = 3
#' )
#'
#' result <- rename_duplicates(dat, cols_to_group_by, args_for_rename_duplicate)
#' print(result)
#'
#' @export
rename_duplicates <- function(dat, cols_to_group_by, args_for_rename_duplicate) {

  args_for_rename_duplicate$rename_duplicate_index <- as.numeric(args_for_rename_duplicate$rename_duplicate_index)
  args_for_rename_duplicate$rename_duplicate_expected_freq <- as.numeric(args_for_rename_duplicate$rename_duplicate_expected_freq)

  if (all(is.na(args_for_rename_duplicate))) {
    # No need to rename duplicates unless args_for_rename_duplicate are specified
    return(dat)
  }

  if (any(is.na(args_for_rename_duplicate))) {
    missing_args <- paste(names(which(is.na(args_for_rename_duplicate))), collapse = "', and '")
    warning(paste0("'", missing_args,
                   "' missing from sheet_structure in the data dict. ",
                   "This may result in unhandled duplicate rows with different ",
                   "values in the data. Please contact a developer."))
    return(dat)
  }

  index_count <- length(args_for_rename_duplicate$rename_duplicate_index)
  prefix_count <- length(args_for_rename_duplicate$rename_duplicate_prefix)

  if (index_count != prefix_count) {
    warning (paste0("rename_duplicate_index in the data_dict is of a different",
                    " length (", index_count, ") to rename_duplicate_prefix (", prefix_count,
                    "). They must be the same. No renaming done."))
    return(dat)
  }

  col <- args_for_rename_duplicate$rename_duplicate_in_col

  if (col %in% names(dat) == FALSE) {
    warning (paste0("rename_duplicate_in_col in the data_dict does not match",
                    "any of the preprocessed column names. No renaming done."))
    return(dat)
  }


  cols_to_group_by <- cols_to_group_by[!is.na(cols_to_group_by)]

  rows_identified <- identify_rows_for_renaming(
    dat,
    args_for_rename_duplicate,
    cols_to_group_by
  )

  rows_renamed <- rename_identified_duplicates(
    rows_identified,
    args_for_rename_duplicate,
    cols_to_group_by
  )

  return(rows_renamed)

}
