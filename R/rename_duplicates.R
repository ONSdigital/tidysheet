#' @title Add a prefix to a duplicated entry.
#'
#' @description If there are values that cannot be uniquely identified by their
#' descriptors, add a prefix to one of character strings in the specified
#' column.
#'
#' Any number of the repeats can be renamed e.g. if the raw data
#' contains two columns called total, the user may choose to rename only one
#' of them (and can choose which using index). Alternatively,
#' they could choose to rename both. See params.
#'
#' Only one set of repeats can be handled: If there are two lots of
#' duplicated names in the data e.g. two columns called 'total' and 3 called
#' 'subtotal', the function currently only allows one of those to be handled -
#' the column name of interest is identified by pattern.
#'
#' If there is more than one row of headers, this function can currently only
#' be used to rename duplicates in the header closest to the data. To get around
#' this you could combine the headers into a single column first.
#'
#' Note: Whilst the entry that is renamed is a column in the raw data it is an
#' entry in a column in dat due to earlier un-pivotting.
#'
#' @param dat dataframe. Must include a column with the name given in column.
#' @param cols_to_group_by vector of character strings giving the names of
#' columns that will have been created during un-pivotting. The entries in these
#' columns would uniquely identify each row if there were no duplicates.
#' @param column character string. The name of the column the duplicate is in in
#' dat. Vector of length 1. In pub sec this variable is specified by
#' rename_duplicate_in_col.
#' @param pattern character string giving a regular expression that identifies
#' the duplicate. Vector of length 1. In pub sec this variable is specified by
#' rename_duplicate_pattern.
#' @param index integer. Which to rename e.g. '2' would rename the 2nd. Can be a
#' vector of length > 1, but if so the number of items must match that in
#' prefix. In pub sec this variable is specified with rename_duplicate_index.
#' @param expected_freq integer. The expected number of repeats of the
#' column name in the raw data. Vector of length 1. In pub sec this variable is
#' specified by rename_duplicate_expected_freq.
#' @param prefix character string. The prefix that is to be pasted onto the
#' original name. Can be a vector of length > 1, but if so the number of
#' items must match that in index. In pub sec this variable is specified by
#' rename_duplicate_prefix.
#'
#' @returns dat dataframe with an additional column 'rename_note' and with the
#' identified duplicate renamed. A warning is raised if expected freq is not
#' equal to the number of repeats found.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   "item_detail" = rep(c("A", "A", "Total", "C", "Total", "Total"), 2),
#'   "year" = rep(c("2021-22", "2022-23"), each = 6),
#'   "numeric" = rep(c(2, 3, 5, 0.1, 5.1, 10.1), 2)
#' )
#' cols_to_group_by <- c("year", "item_detail", rep(NA, 3))
#' column <- "item_detail"
#' pattern <- "(?i)total"
#' index <- c(1, 3)
#' prefix <- c("first", "third")
#' expected_freq <- 3
#'
#' result <- rename_duplicates(
#'     dat, cols_to_group_by, column, pattern, index, prefix, expected_freq)
#' print(result)
#' }
#' @export
rename_duplicates <- function(
    dat, cols_to_group_by,
    column=NA, pattern=NA, index=NA, prefix=NA, expected_freq=NA
    ) {

  user_args <- c(column, pattern, index, prefix, expected_freq)
  arg_names <- c("column", "pattern", "index", "prefix", "expected_freq")
  na_args <- which(is.na(user_args))
  na_count <- sum(is.na(user_args))

  if (na_count == 5) {
    return(dat)
  }

  if (na_count > 0) {
    missing_args <- paste(arg_names[na_args], collapse = "', and '")
    stop(
      "'", missing_args, "' missing from sheet_structure in the data dict. ",
      "This may result in unhandled duplicate rows with different values in ",
      "the data. Please contact a developer."
    )
  }

  if (length(index) != length(prefix)) {
    stop(
      "rename_duplicate_index in the data_dict is of a different ",
      "length (", length(index), ") to rename_duplicate_prefix (",
      length(prefix), "). They must be the same. No renaming done."
    )
  }

  if (column %in% names(dat) == FALSE) {
    stop(
      "rename_duplicate_in_col ('", column, "') does not match any of the ",
      "preprocessed column names. No duplicates renamed."
    )
  }

  message("Adding prefix to duplicated entry")

  cols_to_group_by <- cols_to_group_by[!is.na(cols_to_group_by)]

  rows_identified <- identify_rows_for_renaming(
    dat, cols_to_group_by,
    column, pattern, index, expected_freq
  )

  if (sum(rows_identified$rename_required) == 0) {
    return(dat)
  } else {
    rows_renamed <- rename_identified_duplicates(
      rows_identified, cols_to_group_by,
      column, index, prefix, expected_freq
    )
    return(rows_renamed)
  }

}


#' @title Add specified prefix to entries based on location (1st, 2nd etc)
#'
#' @description Add the specified prefix to the start of the duplicated entries
#' that are at the specified index location.
#'
#' @param dat dataframe. Must include a column with the name given in column.
#' @param cols_to_group_by vector of character strings giving the names of
#' columns that will have been created during un-pivotting. The entries in these
#' columns would uniquely identify each row if there were no duplicates.
#' @param column character string. The name of the column the duplicate is in in
#' dat. Vector of length 1. In pub sec this variable is specified by
#' rename_duplicate_in_col.
#' @param index integer. Which to rename e.g. '2' would rename the 2nd. Can be a
#' vector of length > 1, but if so the number of items must match that in
#' prefix. In pub sec this variable is specified with rename_duplicate_index.
#' @param expected_freq integer. The expected number of repeats of the
#' column name in the raw data. Vector of length 1. In pub sec this variable is
#' specified by rename_duplicate_expected_freq.
#' @param prefix character string. The prefix that is to be pasted onto the
#' original name. Can be a vector of length > 1, but if so the number of
#' items must match that in index. In pub sec this variable is specified by
#' rename_duplicate_prefix.
#' @returns dat with duplicates renamed.
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   item_detail = rep(c("A", "A", "Total", "C", "Total", "Total"), 2),
#'   year = rep(c("2021-22", "2022-23"), each = 6),
#'   numeric = rep(c(2, 3, 5, 0.1, 5.1, 10.1), 2),
#'   location = rep(c(1, 2, 1, 1, 2, 3), 2),
#'   rename_required = rep(c(FALSE, FALSE, TRUE), 4)
#' )
#'
#' cols_to_group_by <- c("year", "item_detail")
#'
#' result <- rename_identified_duplicates(
#'     dat, cols_to_group_by, #
#'     "item_detail", c(1, 3), c("first", "third"), 3 )
#' }
#' @export
rename_identified_duplicates <- function (
    dat, cols_to_group_by, column, index, prefix, expected_freq
    ) {

  name_to_replace <- dat %>%
    filter(rename_required == TRUE) %>%
    distinct(!!sym(column)) %>%
    pull(!!sym(column))

  # so that users can trace the change in wording, add a note to the data
  # stating where the string originally came from in the raw data
  note_added <- dat %>%
    mutate(
      rename_note = ifelse(
        rename_required==TRUE,
        paste0(
          !!column, " was '", name_to_replace, "' in the raw data. It was ",
          "given a prefix because there was more than one instance in the raw ",
          "data with that name."
        ),
        NA)
    )

  prefix_with_index <- data.frame(
    "prefix" = paste0(prefix, " "),
    "location" = as.integer(index)
  )

  all_positions <- data.frame(
    "location" = 1:expected_freq
  )

  prefix_by_location <- all_positions %>%
    left_join(prefix_with_index, by = "location") %>%
    mutate(prefix = ifelse(is.na(prefix), "", prefix),
           !!sym(column) := name_to_replace)

  replacements_done <- note_added %>%
    left_join(prefix_by_location, by = c(column, "location")) %>%
    mutate(prefix = ifelse(is.na(prefix), "", prefix),
           !!sym(column) := paste0(prefix, !!sym(column))) %>%
    select(-c(prefix, location, rename_required))

  return(replacements_done)
}


#' @title Identify which of the repeated rows needs to be renamed
#'
#' @description Flag rows fit the following criteria:
#' - Entry in the specified column matches pattern.
#' - The entry is repeated in at least 2 groups when data are grouped by
#'   cols_to_group_by.
#' - The repeated entry is in the position given by index e.g.
#'   if this is set to 1, only the entries that relate to the first of the
#'   relevant raw data columns are flagged.
#' - The number of times that name is used in the raw data equals
#'   expected_freq
#'
#' @param dat dataframe. Must include a column with the name given in column.
#' @param cols_to_group_by vector of character strings giving the names of
#' columns that will have been created during un-pivotting. The entries in these
#' columns would uniquely identify each row if there were no duplicates.
#' @param column character string. The name of the column the duplicate is in in
#' dat. Vector of length 1. In pub sec this variable is specified by
#' rename_duplicate_in_col.
#' @param pattern character string giving a regular expression that identifies
#' the duplicate. Vector of length 1. In pub sec this variable is specified by
#' rename_duplicate_pattern.
#' @param index integer. Which to rename e.g. '2' would rename the 2nd. Can be a
#' vector of length > 1, but if so the number of items must match that in
#' prefix. In pub sec this variable is specified with rename_duplicate_index.
#' @param expected_freq integer. The expected number of repeats of the
#' column name in the raw data. Vector of length 1. In pub sec this variable is
#' specified by rename_duplicate_expected_freq.
#'
#' @returns The original dataframe with the following columns added:
#' - rename_required: boolean. Identifies the rows containing entries to rename.
#' - location: integer. Identifies whether it is the first second, third of the
#'   instance.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   "year" = rep(c("2022-23", "2023-24"), each = 5),
#'   "item_detail" = rep(c("A", "B", "Total", "C", "Total"), 2),
#'   "numeric" = c(2, 3, 5, 0.1, 5.1,
#'                 1, 2, 3, 0.4, 3.4)
#' )
#' cols_to_group_by <- c("year", "item_detail")
#'
#' result <- identify_rows_for_renaming(
#'     dat, cols_to_group_by, "item_detail", "(?i)total", 1, 2
#'     )
#' }
#' @export
identify_rows_for_renaming <- function(
    dat, cols_to_group_by, column, pattern, index, expected_freq
    ) {

  target_rows <- identify_target_rows(
    dat, cols_to_group_by, column, pattern, index
    )

  # target_rows is essentially the df that gets returned (with location and
  # matches_regex columns dropped), but first we need to run some checks so that
  # incorrect renaming is avoided. First refine to only include matches that are
  # also duplicated.
  target_rows_refined <- refine_target_rows(target_rows, column)

  # If an unexpected number of repeats is present the settings will need to be
  # updated.
  target_rows_checked <- void_wrong_frequency_targets(
    target_rows_refined, column, expected_freq
    ) %>%
    select(-matches_regex)

  return(target_rows_checked)
}


#' @title Identify rows where the entry may require a prefix
#'
#' @description Flag rows that fit the following criteria:
#' - Entry in the specified column matches the given pattern.
#' - The entry is in a position given by index e.g.
#'   if index is set to 1, only the entries that relate to the first of the
#'   relevant raw data columns are flagged.
#' No checks are done at this point on whether the entry is duplicated.
#'
#' @param dat dataframe. Must include a column with the name given in column.
#' @param cols_to_group_by vector of character strings giving the names of
#' columns that will have been created during un-pivotting. The entries in these
#' columns would uniquely identify each row if there were no duplicates.
#' @param column character string. The name of the column the duplicate is in in
#' dat. Vector of length 1. In pub sec this variable is specified by
#' rename_duplicate_in_col.
#' @param pattern character string giving a regular expression that identifies
#' the duplicate. Vector of length 1. In pub sec this variable is specified by
#' rename_duplicate_pattern.
#' @param index integer. Which to rename e.g. '2' would rename the 2nd. Can be a
#' vector of length > 1, but if so the number of items must match that in
#' prefix. In pub sec this variable is specified with rename_duplicate_index.
#'
#' @returns The original dataframe with the following columns added:
#' - rename_required: boolean. Identifies the rows containing entries to rename.
#' - matches_regex: boolean. Identifies entries that match pattern.
#' - location: integer. Identifies whether it is the first second, third of the
#'   instance.
#' A warning is raised if repeated entries have been found but they do not match
#' the pattern.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   "year" = rep(c("2022-23", "2023-24"), each = 5),
#'   "item_detail" = rep(c("A", "B", "Total", "C", "Total"), 2),
#'   "numeric" = c(2, 3, 5, 0.1, 5.1,
#'                 1, 2, 3, 0.4, 3.4)
#' )
#' cols_to_group_by <- c("year", "item_detail")
#' column <- "item_detail"
#' pattern <- "(?i)total"
#' index <- 1
#'
#' result <- identify_target_rows(dat, cols_to_group_by, column, pattern, index)
#' }
#' @export
identify_target_rows <- function(
    dat, cols_to_group_by, column, pattern, index
    ) {

  # find the items in the specified column that match the pattern of the
  # expected duplicates
  regex_matched <- dat %>%
    mutate(matches_regex = stringr::str_detect(!!sym(column), pattern))

  # by grouping across the columns we created during pivoting (beheading), we
  # can label each entry that was a duplicated column in the raw data by it's
  # order i.e. whether it was the first column with the name (location = 1),
  # second (location = 2) etc. We can then use this to flag the rows that need
  # renaming based on which one was specified in the data dict with index.
  target_rows <-  regex_matched %>%
    group_by(across(all_of(cols_to_group_by))) %>%
    mutate(location = row_number()) %>%
    mutate(rename_required = (location %in% index) & (matches_regex == TRUE)) %>%
    ungroup()

  if (any(index > max(target_rows$location)) & any(target_rows$location > 1)) {
    stop(
      "More than 1 row has the same identifiers, but none of them can be ",
      "renamed without updating the settings: ",
      "The repeated entry(s) at index(es): ", paste0(index, collase = ", "),
      "is/are set to be given a prefix, but there are only ",
      max(target_rows$location), " repeats, so at least one index is ",
      "too large. Please contact a developer to update the settings."
    )
  }

  if (all(target_rows$rename_required==FALSE) & any(target_rows$location > 1)) {
    stop(
      "More than 1 row has the same identifiers, but none of them can be ",
      "renamed without updating the settings: No entries ",
      "(usually column names in the raw data) match the pattern '",
      pattern, "' given in rename_duplicate_pattern in sheet_structure. ",
      "Please ask a developer to edit rename_duplicate_pattern."
    )
  }

  return(target_rows)
}


#' @title Refine which of the repeated rows needs to be renamed
#'
#' @description Refine the flag for which rows contain entries that require a
#' prefix to exclude cases that are unique when grouped by cols_to_group_by
#' E.g. if the entries that are usually repeated in the raw data
#' are called 'total' the pattern might be '(?i)total'. In
#' subsequent releases if the data has two entries called 'total A' and one
#' called 'total B', 'total B' may be flagged to be renamed because it matches
#' pattern, however it is clearly not a target for renaming, because it is
#' unique.
#'
#' Warn the user if there is more than one entry that matches the pattern
#' and mark these as not to be renamed.
#' E.g. if the entries that are usually repeated in the raw data
#' are called 'total' the pattern might be '(?i)total'. In
#' subsequent releases if the data has two columns called 'total A' and two
#' called 'total B', we don't want to do the rename, because we don't know which
#' is the right one.
#'
#' Warn the user if there are no duplicated entries that match pattern.
#'
#' @param dat dataframe. Must include a column with the name given in column.
#' @param column character string. The name of the column the duplicate is in in
#' dat. Vector of length 1. In pub sec this variable is specified by
#' rename_duplicate_in_col.
#'
#' @returns dat with 'rename_required' updated
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   "item_detail" = c("Total A", "Total A", "Total B"),
#'   "numeric" = c(2, 3, 5),
#'   "matches_regex" = rep(TRUE, 3),
#'   "location" = c(1, 2, 1),
#'   "rename_required" = c(TRUE, FALSE, TRUE)
#' )
#' column <- "item_detail"
#' result <- refine_target_rows(dat, column)
#' }
#' @export
refine_target_rows <- function(dat, column) {

  matched_repeated_names <- dat %>%
    filter(matches_regex==TRUE & location > 1) %>%
    distinct(!!sym(column)) %>%
    pull(!!sym(column))

  # if there is a match to the pattern that is unique for the group, we don't
  # need to overwrite it, so drop it from target_rows
  dat_refined <- dat %>%
    mutate(rename_required = ifelse(
      rename_required == TRUE &
        (!!sym(column) %in% matched_repeated_names == FALSE),
      FALSE,
      rename_required
    ))

  if (length(matched_repeated_names) > 1) {
    stop(
      "There are ", length(matched_repeated_names), " non-unique entries ",
      "in the '", column, "' column of the data that match the ",
      "rename_duplicate_pattern pattern: '",
      paste0(matched_repeated_names, collapse = "', '"), "'. ",
      "These entries will not be distinguishable from each other in the  ",
      "preprocessed data. Please contact a developer if this is a column you",
      "need to use."
    )
  } else if (length(matched_repeated_names) == 0) {
    warning(
      "rename_duplicate arguments are given but no repeated entries matching ",
      "the rename_duplicate_pattern pattern have been found in the '", column,
      "' column of the data. Please contact a developer."
    )
    no_target_rows <- mutate(dat, rename_required = FALSE)
    return(no_target_rows)

  } else {
    return(dat_refined)
  }

}

#' @title Avoid adding a prefix when there is an unexpected number of repeats.
#'
#' @description Update rename_required to be FALSE if the number of expected
#' repeats does not matche the number of repeats found.
#'
#' Warn the user if there are either too few or too many repeats and flag these
#' as not to be renamed (void them).
#'
#' The user needs to be warned if there are more columns with a repeated name
#' than usual, because the ones that get renamed are specified based on there
#' being the expected number of duplicates E.g. Take a case where there are
#'  usually two columns called 'total', and the user wants to rename the second
#' (index = 2). Imagine that one year there are three columns called total,
#' with a new total column falling before the one we usually rename. In this
#' case, we would otherwise end up renaming the 'new' total column, not the one
#' we intended, which is now the third.
#'
#' @param dat dataframe. Must include a column with the name given in column.
#' @param column character string. The name of the column the duplicate is in in
#' dat. Vector of length 1. In pub sec this variable is specified by
#' rename_duplicate_in_col.
#' @param expected_freq integer. The expected number of repeats of the
#' column name in the raw data. Vector of length 1. In pub sec this variable is
#' specified by rename_duplicate_expected_freq.
#'
#' @returns dat with 'rename_required' updated
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   "item_detail" = c("Total", "Total", "Total", "Other"),
#'   "numeric" = c(2, 3, 5, 10),
#'   "matches_regex" = c(rep(TRUE, 3), FALSE),
#'   "location" = c(1, 2, 3, 1),
#'   "rename_required" = c(TRUE, FALSE, FALSE, FALSE)
#' )
#' void_wrong_frequency_targets(dat, "item_detail", 2)
#' }
#' @export
void_wrong_frequency_targets <- function (dat, column, expected_freq) {

  # we don't want to give further warnings about names that have already been
  # dropped from being renamed, e.g where there is a matched target that is
  # not repeated/duplicated, so limit check to names that are still flagged.
  names_to_rename <- dat %>%
    filter(rename_required) %>%
    distinct(!!sym(column))

  if (nrow(names_to_rename) == 0) {
    message("No renames required.")
    return(dat)
  }

  matches <- names_to_rename %>%
    left_join(dat, by = column) %>%
    distinct(!!sym(column), location, matches_regex) %>%
    filter(matches_regex == TRUE)

  frequencies <- matches %>%
    group_by(!!sym(column)) %>%
    summarise(frequency = max(location))

  too_many <- filter(frequencies, frequency > expected_freq)
  too_few <- filter(frequencies, frequency < expected_freq)

  if (nrow(too_many) > 0) {
    names_of_too_many <- too_many %>%
      pull(!!sym(column)) %>%
      paste0(collapse = ", ")

    freq_of_too_many <- too_many %>%
      pull(frequency) %>%
      paste0(collapse = ", ")

    warning(
      "More than the expected number of duplicate names were found for '",
      names_of_too_many, "'. ", freq_of_too_many,
      " repeats were found where only ", expected_freq, " were expected. ",
      "These columns have not been renamed. Please contact a developer if the ",
      "duplicate column is one you need for further processing."
    )
    no_target_rows <- mutate(dat, rename_required = FALSE)
    return(no_target_rows)

  } else if (nrow(too_few) > 0) {
    names_of_too_few <- too_few %>%
      pull(!!sym(column)) %>%
      paste0(collapse = ", ")

    freq_of_too_few <- too_few %>%
      pull(frequency) %>%
      paste0(collapse = ", ")

    warning(
      "Fewer than the expected number of duplicate names were found for '",
      names_of_too_few, "'. ", freq_of_too_few,
      " repeats were found where ", expected_freq, " were expected. ",
      "None of these columns have been renamed. Please contact a developer if ",
      "the duplicate column is one you need for further processing."
    )
    no_target_rows <- mutate(dat, rename_required = FALSE)
    return(no_target_rows)

  } else {
    return(dat)
  }
}

