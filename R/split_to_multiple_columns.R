#' @title Split the contents of one column over multiple columns
#'
#' @description Sometimes we may want to split a single character column into
#' multiple columns. 
#' 
#' @details The layout of a dataset may change so that it has only
#' one row of headers that contains all the information that was once in
#' multiple headers. Use this function to create the output as it would
#' have been when the input had multiple headers. This may be required as a 
#' quick fix if the output has to remain stable. However, it is recommended to 
#' not be used as a long term fix - the datset with fewer columns is to be 
#' preferred as it easier and less error prone to join columns than to split 
#' them.
#' 
#' Not every split_point has to be present in every string to be split - if
#' a pattern does not exist in a given string, the previous value is used (see
#' example below). In other words, if the split is not consistent across all
#' data, split points are given for the element that requires the most splits.
#'
#' split_patterns are regular expressions used to determine where each split
#' should occur. The split_point pattern is not removed from the result, but
#' any separators (new lines, :, and - ) are.
#'
#' The order split_patterns are given in must match the order the patterns occur
#' in.
#'
#' If no splits patterns are found for a given item, the whole of the from
#' column will be given in every to column.
#'
#' Note: The patterns stated MUST appear in all strings in the order they are
#' given so should be as specific as possible. For example, given the string
#' 'A: B- C' if the split points are given as '-' then ':', the string will be
#'  split only once, at the '-' giving 'A: B' and 'C'.
#'
#' @param dat Dataframe containing a column to split.
#' @param from character string. The name of the column to split.
#' @param to vector of strings. The new names to put the split in.
#' @param split_point_descriptions vector of strings that code for regular
#' expressions. Each one will only be matched once, so any repeats must be
#' explicit.
#'
#' @examples
#' \dontrun{
#' If split_points are '\n1.','\n2.', '\n3.', missing parts are handled thus:
#'
#' | col_to_split | description_1 | description_2 | description_3 | units    |
#' |--------------|---------------|---------------|---------------|----------|
#' | split \n     |               |               |               |          |
#' | 1.this \n    |  split        | this          | one           | thousand |
#' | 2.one \n     |               |               |               |          |
#' | 3.thousand   |               |               |               |          |
#' |--------------|---------------|---------------|---------------|----------|
#' | split \n     |               |               |               |          |
#' | 2.one \n     |  split        | split         | one           | thousand |
#' | 3.thousand   |               |               |               |          |
#' |--------------|---------------|---------------|---------------|----------|
#' | split \n     |               |               |               |          |
#' | 1.this \n    |  split        | this          | this          | thousand |
#' | 3.thousand   |               |               |               |          |
#' |--------------|---------------|---------------|---------------|----------|
#' | split \n     |               |               |               |          |
#' | 1.this \n    |  split        | this          | one           | one      |
#' | 2.one        |               |               |               |          |
#' |--------------|---------------|---------------|---------------|----------|
#' | split \n     |  split        | split         | split         | thousand |
#' | 3.thousand   |               |               |               |          |
#' |--------------|---------------|---------------|---------------|----------|
#' | split \n     |               |               |               |          |
#' | 2.one \n     |  split        | split         | one           | one      |
#' |--------------|---------------|---------------|---------------|----------|
#' | 1. this \n   |  this         | this          | this          | thousand |
#' | 3. thousand  |               |               |               |          |
#' |--------------|---------------|---------------|---------------|----------|
#' |  other       |  other        |  other        |  other        | other    |
#' |--------------|---------------|---------------|---------------|----------|
#' | wrong \n     |  wrong \n     | 1. matches    | 1. matches    |1. matches|
#' | 2. order \n  |  2. order     |               |               |          |
#' | 1. matches   |               |               |               |          |
#' |--------------|---------------|---------------|---------------|----------|
#'
#'
#' from <- "col_to_split"
#' to <- c("description_1", "description_2", "description_3", "units")
#' split_point_descriptions <- c(
#' "newline_whitespace_=1", "newline_whitespace_=2", "newline_whitespace_=3"
#' )
#'
#' dat <- data.frame(
#'   "col_to_split" = c("split:\n 1.this \n 2.one \n 3.thousand",
#'                      "split-\n 2.one \n 3.thousand",
#'                      "split\n 1.this \n 3.thousand",
#'                      "split\n 1.this \n 2.one",
#'                      "split\n 3.thousand",
#'                      "split\n 2.one",
#'                      "1. this \n 3. thousand",
#'                      "other",
#'                      "wrong\n 2. order \n 1. matches"
#'                      ),
#'   "value" = c(1:9)
#' )
#'
#' split_to_multiple_columns(dat,
#'                          from = "col_to_split",
#'                          to = c("description_1", "description_2",
#'                          "description_3", "units"),
#'                          split_point_descriptions = split_point_descriptions)
#' #----------------------------------------------------------------------------
#' # Example 2
#'
#' split_point_descriptions <- c("newline", "newline", "=Note", "=Note")
#' #split_patterns <- c("\n", "\n", "Note", "Note")
#' dat <- data.frame(
#'          col_to_split = c("split \n this \n here",
#'                           "split \n here Note this",
#'                           "to Note this Note that",
#'                           "Note this Note that")
#'          )
#' split_to_multiple_columns(
#' dat, "col_to_split", c("A", "B", "C", "D", "E"), split_point_descriptions
#' )
#' }
#' @md
#' @export
split_to_multiple_columns <- function(dat, from, to, split_point_descriptions) {

  if (all(is.na(from), is.na(to), is.na(split_point_descriptions))) {
    return (dat)
  }

  if (any(is.na(from), is.na(to), is.na(split_point_descriptions))) {
    stop(
      "At least one but not all of `from` (header_to_split), `to` ",
      "(header_split_to) and `split_point_descriptions` (split_points) ",
      "has been specified in settings. Either all or none of these ",
      "must be specified."
    )
  }

  if (length(from) > 1) {
    stop(
      "Only one item can be split into multiple columns. Please correct ",
      "header_to_split in the settings."
      )
  }

  if (length(to)-1 != length(split_point_descriptions)) {
    stop(
      "`split_point_descriptions` (split_points) must contain one fewer ",
      "elements than `to` (header_split_to). Please correct the settings"
      )
  }

  if (length(split_point_descriptions) != (length(to) - 1)) {
    stop("`split_point_descriptions` (split_points) must be 1 element shorter ",
        " than `to` (header_split_to).")
  }

  if (any(to %in% from)) {
    stop(
      "One of the header_split_to strings matches the header_to_split string. ",
      "Please use a different string for header_to_split, and make sure to ",
      "also change it in columns_to_create.")
  }

  contents_of_from_col <- pull(dat, !!sym(from))
  if (all(is.na(contents_of_from_col))) {
    stop(
      "split_patterns were provided in the data_dict, but no information ",
      " has been found in the data for splitting (in '", from,
      "', specified by `from` (header_to_split). Please contact a developer ",
      "for a fix. Developers: Please check for earlier warnings and errors - ",
      "this warning may be raised if the first header row has been ",
      "identified incorrectly."
      )
  }

  split_patterns <- build_regex(split_point_descriptions)

  # ALT_ is required at the beginning of split_point_descriptions so that
  # build_regex works. However it needs to be removed now as
  # the true start of the pattern description is used later on.
  split_point_descriptions <- str_remove(split_point_descriptions, "ALT_")


  split_point_presence <- flag_existing_split_points(dat, split_patterns, from)

  splits_to_use <- get_split_order(split_point_presence)

  # If no splits have been found anywhere send a message that the data may have
  # changed, and just rename the column to split with the first name in
  # `to` (e.g. COR_TAB4 in test files)
  if (all(is.na(splits_to_use$tmp_split_1))) {
    warning(
      "No split pattern has been found in the column to split ('", from,
      "'). Only the first name in `to` (header_split_to) will be used ('",
      to[1], "'). Columns: ", to[2:length(to)], "' will be missing from the ",
      "data. If these are used in SAP mappings, please contact a developer to ",
      "edit the settings."
      )

    no_splits_performed <- splits_to_use %>%
      rename(!!sym(to[1]) := !!sym(from)) %>%
      remove_unwanted_split_columns()

    return(no_splits_performed)

  }

  match_at_start_fixed <- update_start_of_string(
    splits_to_use, split_patterns, from
    )

  consecutives_identified <- flag_consecutive_matching_splits(
    match_at_start_fixed, split_patterns, split_point_descriptions
  )

  splits <- do_the_splits(consecutives_identified, split_patterns, from)

  splits_cleaned <- splits %>%
    trim_separators_from_new_cols() %>%
    remove_unwanted_split_columns() %>%
    rename_split_columns(to)

  return(splits_cleaned)

}


#' @title Add a boolean column stating whether pattern was found
#'
#' @description For each split point add a column e.g. 'use_split_point_1' that
#' states whether that split point (in this case the first) was found in the
#' 'from' column.
#'
#' split_patterns must be given in the order in which they are found - items
#' where the first split pattern occurs after the second split pattern will
#' only be split at the second pattern.
#'
#' @param dat dataframe containing a column with the name given in 'from'
#' @param split_patterns array of regex strings. One for each place the 'from'
#' column should be split. Each pattern is only used once.
#' @param from string. The name of the column containing the strings to be split.
#'
#' @examples
#' \dontrun{
#' split_patterns <- c( "(\n)\\s*1\\.", "(\n)\\s*2\\.", "(\n)\\s*3\\.")
#'
#' dat <- data.frame(
#'   "col_to_split" = c("split\n 1.this \n 2.one \n 3.thousand", "split\n 2.one"),
#'   "value" = c(100, 600)
#' )
#' flag_existing_split_points(dat, split_patterns, "col_to_split")
#' }
flag_existing_split_points <- function(dat, split_patterns, from) {

  dat <- mutate(dat, tmp_remaining = !!sym(from))

  from_index <- which(names(dat) == from)

  if (is.numeric(dat[[from_index]])) {
    stop(
      "Column '", from, "' is expected to contain string data but it contains ",
      "numeric data. This can happen if the number of rows containing headers ",
      "is less than usual. If this is the case a developer will need to create ",
      "a new version of the settings and update the sheet_structure.")
  }

  for(i in 1:length(split_patterns)) {
    new_col <- paste0("tmp_use_split_point_", i)

    dat <- dat %>%
      mutate(tmp_split = str_split_fixed(tmp_remaining, split_patterns[i], 2)) %>%
      mutate(!!sym(new_col) := case_when(
        tmp_split[,2]==""|is.na(tmp_split[,2]) ~ FALSE,
        TRUE ~ TRUE
      )) %>%
      mutate(
        tmp_remaining = case_when(
          !!sym(new_col) == TRUE ~ str_split_fixed(tmp_remaining, split_patterns[i], 2)[, 2],
          !!sym(new_col) == FALSE ~ tmp_remaining
        )
      ) %>%
      select(-c(tmp_split))

  }

  cleaned_dat <- select(dat, -tmp_remaining)

  return (cleaned_dat)

}


#' @title Give the order in which split_point patterns should be used per item
#'
#' @description Not all items will contain all split_patterns patterns so the
#' first split may for example require different patterns to be used for
#' different items. Some (or all) split_point patterns can be skipped but they
#' cannot be done out of order. For example the first split for an item can be
#' the 2nd element of split_patterns (rather than the first), and the third
#' split can be the 4th element of split_patterns. However, if the first split
#' is the 2nd element of split_patterns the second split CANNOT be the 1st
#' because this doesn't exist once the 2nd element has been used).
#'
#' This function uses the boolean columns created by flag_existing_split_points
#' to return an integer column for each split. See split_to_multiple_columns for
#' the desired outcome when an item does not contain all split_point patterns.
#'
#' @param dat Dataframe containing boolean columns prefixed by
#' 'use_split_point_'
#'
#' @returns dat with new columns prefixed by "split_"
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   col_to_split = c("split\n 1.this \n 2.one \n 3.thousand", "split\n 2.one"),
#'   'tmp_use_split_point_1' = c(TRUE, FALSE),
#'   'tmp_use_split_point_2' = c(TRUE, TRUE),
#'   'tmp_use_split_point_3' = c(TRUE, FALSE),
#'   value = c(100, 600)
#' )
#'
#' get_split_order(dat)
#' }
get_split_order <- function(dat) {

  split_count <- ncol(select(dat, starts_with("tmp_use_split_point_")))

  dat <- mutate(dat, tmp_use_split_point_0 = TRUE)

  for (i in 1:split_count) {
    new_col <- paste0("tmp_split_", i)

    dat <- dat %>%
      mutate(!!sym(new_col) := NA)

    for (j in i:split_count) {

      use_split_point <- paste0("tmp_use_split_point_", j)

      dat <- dat %>%
        mutate(!!sym(new_col) :=
                 case_when(
                   is.na(!!sym(new_col)) & !!sym(use_split_point) == TRUE ~ j,
                   TRUE ~ !!sym(new_col)
                 )
        )
    }
  }

  return (dat)
}

#' @title Flag strings where the pattern is found at the start of the string
#'
#' @description If the first split pattern that is found in the 'from' column is
#' right at the start of the string, we need to act like that pattern is not
#' there so that we don't end up with a blank in the first column.
#'
#' This function checks if the first matching pattern is at the start of the
#' 'from' string. If it is at the start, the pattern to be ignored is flagged
#' as such in a new column called 'tmp_pattern_at_start'.  An underscore is also
#' added to the 'from' string where tmp_pattern_at_start is TRUE (if it already
#' starts with an underscore a hyphen is added in case of patterns beginnging
#' 'any number of underscores'). This addition of a hyphen or underscore is to
#' prevent errors when calling update_remaining(): Without there being something
#' to the left of the pattern match in the `from` string, we would get
#' 'empty search patterns are not supported' errors and the 'tmp_string_'
#' columns would remain empty.
#'
#' How this info is used in later functions:
#' Instead of using the pattern that matches the start of the string, the next
#' matching pattern is used for that split. The relevant tmp_split_ (which gives
#' the pattern index) and tmp_use_split_ columns are updated accordingly.
#'
#'
#' N.B. Known issues
#' - This does not cover the potential situation where there
#' is a pattern match at the start of the string, and the next split_patterns
#' pattern is a copy of that pattern. However this is very unlikely to happen
#' and would increase the complexity of the code even more, so we will leave it
#' until it is an issue.
#' - It also only anchors the first pattern if a pattern contains an OR (|), so
#' if one of the patterns after the | is found anywhere in the string an
#' underscore will be added to the start of the string. This is not a major
#' issue though.
#'
#' @param dat dataframe containing the column specifed by 'from' and columns
#' prefixed with tmp_use_split_point and tmp_split.
#' @param split_patterns character vector of regular expressions.
#' @param from character. The name of the column whose entries are to be split.
#' Must be the name of a character column.
#'
#' @returns dat, with an additional boolean column (tmp_pattern_at_start).
#' If tmp_pattern_at_start is true, the string in 'from' is modified to begin
#' with an underscore (or if the split_pattern starts with an underscore, a
#' hyphen).
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'     col_to_split = c("1 A 2 B 3 C 4", "B 3 C 4", "C 4"),
#'     tmp_use_split_point_1 = c(TRUE, FALSE, FALSE),
#'     tmp_use_split_point_2 = c(TRUE, TRUE, FALSE),
#'     tmp_use_split_point_3 = c(TRUE, TRUE, TRUE),
#'     tmp_split_1 = c(1, 2, 3),
#'     tmp_split_2 = c(2, 2, 3),
#'     tmp_split_3 = c(3, 3, 3)
#'     )
#' split_patterns <- c("A", "B", "C")
#'
#' update_start_of_string(dat, split_patterns, "col_to_split")
#' }
update_start_of_string <- function(dat, split_patterns, from) {

  # paste "^" at the start of every split point pattern that doesn't already
  # start that way to anchor the pattern search to the beginning of the string
  # NOTE if a pattern contains an OR (|) this will only anchor the first pattern
  patterns_for_start <- c()
  for (i in 1:length(split_patterns)) {
    if (substr(split_patterns[i], 1, 1) != "^") {
      patterns_for_start <- c(patterns_for_start,
                              paste0("^", split_patterns[i]))
    } else {
      patterns_for_start <- c(patterns_for_start, split_patterns[i])
    }
  }

  match_at_start_flagged <- dat %>%
    mutate(tmp_pattern_at_start = case_when(
      str_detect(!!sym(from), patterns_for_start[tmp_split_1]) ~ TRUE,
      TRUE ~ FALSE
    ))

  if (sum(match_at_start_flagged$tmp_pattern_at_start) > 0) {
    problem_strings <- match_at_start_flagged %>%
      filter(tmp_pattern_at_start == TRUE) %>%
      pull(from)

    warning(
      length(problem_strings), " strings in the `to` (header_split_to) ",
      "column start with the first split_patterns pattern. ",
      "The start of the string to be split will be modified to avoid issues ",
      "with the split. If there are issues with columns containing unexpected ",
      "series, split_patterns may need to be updated by a developer."
    )
  }

  start_adapted <- match_at_start_flagged %>%
    mutate(!!sym(from) := case_when(
      tmp_pattern_at_start == TRUE
      & startsWith(split_patterns[tmp_split_1], "_") ~
        paste0("-", !!sym(from)),
      tmp_pattern_at_start == TRUE
      & startsWith(split_patterns[tmp_split_1], "_") == FALSE ~
        paste0("_", !!sym(from)),
      TRUE ~ !!sym(from)
    ))

  return(start_adapted)
}

#' @title Identify consecutive matching split patterns.
#'
#' @description Adds boolean columns prefixed with 'matches_next_pattern_' and
#' 'whitespace_' for every split_point pattern. 'matches_next_pattern_' states
#' whether a split_patterns pattern is the same as the previous pattern.
#' 'whitespace_' states whether the split_point_description starts with either
#' 'whitespace' or 'newline'.
#'
#' Eventual use (see remove_repeated_pattern_from_remaining):
#' The characters that match the split_point pattern are not themselves removed
#' from the 'tmp_remaining' column in do_the_splits().For example if we take the
#' value 'split _newline_ this _newline_ here' and the split_patterns are
#' c('newline', 'newline') after the first split string_1 would be 'split' and '
#' tmp_remaining' would be
#' '_newline_ this _newline_ here'.
#' If didn't have the output of this function) the second split
#' would result in string_2 being '' and tmp_remaining being
#' '_newline_ this _newline_ here',
#' because it would have split at the first newline.
#'
#' The 'whitespace_' columns are useful later on because if there are
#' consecutive matching split patterns that satrt with whitespace or newline we
#' can just remove that element of the pattern. Otherwise we need to remove the
#' whole pattern which may contain useful info that we would rather keep.
#'
#' IMPORTANT: 'whitespace' and 'newline' are referenced in build_regex. Changes
#' to the whitespace type patterns there should also be reflected in this
#' funciton.
#'
#' @param dat Dataframe output containing columns prefixed with 'split_'.
#' @param split_patterns character array of regex patterns.
#' @param split_point_descriptions character array of regex pattern
#' descriptions passed from Python. N.B. white space type patterns are currently
#' covered by 'whtespace' and 'newline'. If any more are added, they will also
#' need adding to the code in this function
#'
#' @returns dat with 2 new columns for each split_patterns. One prefixed with
#' 'matches_next_pattern_' the other prefixed with 'whitespace_'.
#'
#' @examples
#' \dontrun{
#' patterns <- c( "(\n)\\s*1\\.", "(\n)\\s*2\\.", "(\n)\\s*3\\.")
#' split_point_descriptions <- c(
#'    "newline_whitespace_=1", "newline_whitespace_=2", "newline_whitespace_=3"
#'    )
#'
#' dat <- data.frame(
#'   col_to_split = c("split\n 1.this \n 2.one \n 3.thousand", "split\n 2.one"),
#'   tmp_split_1 = c(1, 2),
#'   tmp_split_2 = c(2, 2),
#'   tmp_split_3 = c(3, NA),
#'   value = c(100, 600)
#' )
#'
#' flag_consecutive_matching_splits(dat, patterns, split_point_descriptions)
#' }
flag_consecutive_matching_splits <- function(
    dat, split_patterns, split_point_descriptions
    ) {

  split_count <- length(split_patterns)

  if (split_count == 1) {

    no_consecutives <- dat %>%
      mutate(tmp_matches_next_pattern_1 = FALSE,
             tmp_whitespace_1 = FALSE)

    return (no_consecutives)
    }

  for (i in 1:(split_count - 1)) {
    split <- paste0("tmp_split_", i)
    next_split <- paste0("tmp_split_", i + 1)
    matches_next_pattern <- paste0("tmp_matches_next_pattern_", i)
    whitespace <- paste0("tmp_whitespace_", i)

    # we need to be able to index the split_patterns using the value in
    # the tmp_split_ column. However, this is sometimes NA which cannot be
    # used as an index - it will fail to evaluate.

    dat <- dat %>%
      mutate(
        !!sym(matches_next_pattern) := case_when(
          split_patterns[!!sym(split)] == split_patterns[!!sym(next_split)]
          & !is.na(!!sym(split)) ~ TRUE,
          TRUE ~ FALSE
        ),
        !!sym(whitespace) :=
          # 'whitespace' and 'newline' are referenced in build_regex. Changes
          # to the whitespace type patterns there should also be reflected here.
          case_when(
            str_starts(
              split_point_descriptions[!!sym(split)], "whitespace|newline"
              )
            & !is.na(!!sym(split)) ~ TRUE,
            TRUE ~ FALSE
            )
      )

    # the following is required for `do_the_splits` to work correctly
    if (i == split_count - 1) {
      matches_next_pattern <- paste0("tmp_matches_next_pattern_", i + 1)
      whitespace <- paste0("tmp_whitespace_", i + 1)
    }

  }

  # the following is required for `do_the_splits` to work correctly
  dat <- dat %>%
    mutate(!!sym(matches_next_pattern) := FALSE,
           !!sym(whitespace) := FALSE)

  return (dat)
}

#' @title split strings in the order provided
#'
#' @description Split strings in the 'from' column and put the resulting strings
#' in new columns with the prefix 'tmp_string_'. No 'to' column will be empty
#' even if not all 'from' strings contain matches to all the split_patterns
#' patterns.
#'
#' @param  dat Dataframe output containing columns prefixed with
#' 'tmp_use_split_point_', 'tmp_split_', 'tmp_matches_next_pattern_' and
#' 'tmp_whitespace_'.
#' @param split_patterns character array of regex patterns.
#' @param from character string giving the name of the column holding strings to
#' be split.
#'
#' @returns dat with columns that hold each element of the from strings after
#' being split. These column names are prefixed with 'tmp_string_'
#'
#' @examples
#' \dontrun{
#' split_patterns <- c( "(\n)\\s*1\\.", "(\n)\\s*2\\.", "(\n)\\s*3\\.")
#' split_point_descriptions <- c(
#'    "newline_whitespace_=1", "newline_whitespace_=2", "newline_whitespace_=3"
#'    )
#'
#' dat <- data.frame(
#'   col_to_split = c("split\n 1.this \n 2.one \n 3.thousand",
#'                      "split\n 2.one"),
#'   tmp_use_split_point_0 = c(TRUE, TRUE),
#'   tmp_use_split_point_1 = c(TRUE, FALSE),
#'   tmp_use_split_point_2 = c(TRUE, TRUE),
#'   tmp_use_split_point_3 = c(TRUE, FALSE),
#'   tmp_split_1 = c(1, 2),
#'   tmp_split_2 = c(2, 2),
#'   tmp_split_3 = c(3, NA),
#'   tmp_matches_next_pattern_1 = c(FALSE, TRUE),
#'   tmp_matches_next_pattern_2 = c(FALSE, FALSE),
#'   tmp_matches_next_pattern_3 = c(FALSE, FALSE),
#'   tmp_whitespace_1 = c(TRUE, TRUE),
#'   tmp_whitespace_2 = c(TRUE, TRUE),
#'   tmp_whitespace_3 = c(TRUE, TRUE),
#'   value = c(100, 600)
#' )
#'
#' do_the_splits(dat, split_patterns, 'col_to_split')
#' }
do_the_splits <- function(dat, split_patterns, from) {

  split_count <- length(split_patterns)

  splits <- dat %>%
    mutate(tmp_remaining = !!sym(from),
           !!sym(paste0("tmp_split_", split_count+1)) := NA)

  for (i in 1:split_count) {

    current_string_col <- paste0("tmp_string_", i)
    use_split_point <- paste0("tmp_use_split_point_", i-1)
    current_split <- paste0("tmp_split_", i)
    matches_next_pattern <- paste0("tmp_matches_next_pattern_", i)
    whitespace <- paste0("tmp_whitespace_", i)

    if (i == 1) {
      prev_string_col <- "tmp_remaining"
    } else {
      prev_string_col <- paste0("tmp_string_", i-1)
    }

    new_strings <- get_new_string(
      splits, split_patterns, current_string_col, prev_string_col,
      use_split_point, current_split
    )

    strings_removed_from_remaining <- update_remaining(
      new_strings, split_count, current_string_col, i
      )

    splits <- remove_repeated_pattern_from_remaining(
      strings_removed_from_remaining, split_patterns[i],
      matches_next_pattern, whitespace
      )

    if (i == split_count) {current_string_col <- paste0("tmp_string_", i + 1)}
  }

  # The final string will be whatever is left
  final_split <- splits %>%
    rename(!!sym(current_string_col) := tmp_remaining) %>%
    relocate(!!sym(current_string_col), .after = last_col())

  return(final_split)
}


#' @title Create a column containing the newly split off string.
#'
#' @description Each newly split off string is put in a column prefixed with
#' 'tmp_string_'. Use current_split to know which split_patterns pattern to use
#' (it is an index).
#' - If there are no split_point pattern matches in an item, all 'tmp_string_'
#' columns will equal the entry in the original column (which has been copied to
#' 'tmp_remaining' by this point).
#' - If current_split is NA, there are no splits left to do so the last string
#' is carried forward.
#' - If use_split_point is FALSE, the regex pattern in split_patterns does not
#' exist, so the last string is also carried forward in this situation.
#'
#' @param dat Dataframe output containing columns prefixed with
#' 'tmp_use_split_point_', 'tmp_split_', 'tmp_matches_next_pattern_' and
#' 'tmp_whitespace_'.
#' @param split_patterns character array of regex patterns.
#' @param current_string_col character. Name of column with prefix 'tmp_string_'
#' This column will be filled with the new (split) string.
#' @param prev_string_col character. Name of the column containing the previous
#' string split. If no splits have yet been performed this will be
#' 'tmp_remaining'
#' @param use_split_point character. Name of the boolean column with prefix
#' 'tmp_use_split_point_'
#' @param current_split character. Name of the integer column with prefix
#' 'tmp_split_' containing the index of the split_patterns pattern to be used.
#'
#' @returns dat with new columns containing the split string, with the prefix
#' 'tmp_string_'.
#'
#' @examples
#' \dontrun{
#' split_patterns <- c( "(\n)\\s*1\\.", "(\n)\\s*2\\.", "(\n)\\s*3\\.")
#' current_string_col <- "tmp_string_1"
#' use_split_point <- "tmp_use_split_point_0"
#' current_split <- "tmp_split_1"
#' matches_next_pattern <- "tmp_matches_next_pattern_1"
#' whitespace <- "tmp_whitespace_1"
#'
#' dat <- data.frame(
#'   tmp_remaining = "split\n 1.this \n 2.one \n 3.thousand",
#'   tmp_use_split_point_0 = TRUE,
#'   tmp_split_1 = 1,
#'   tmp_matches_next_pattern_1 = FALSE,
#'   tmp_whitespace_1 = TRUE,
#'   value = 100
#' )
#'
#' get_new_string(
#'    dat, split_patterns, current_string_col, "tmp_remaining",
#'    use_split_point, current_split
#'    )
#' }
get_new_string <- function(dat, split_patterns,
                             current_string_col, prev_string_col,
                             use_split_point, current_split) {

  if (!prev_string_col %in% names(dat)) {
    stop("'", prev_string_col,
         "' should be a column name, but it was not found.")
  }
  if (!use_split_point %in% names(dat)) {
    stop("'", use_split_point,
         "' should be a column name, but it was not found.")
  }
  if (!current_split %in% names(dat)) {
    stop("'", current_split,
         "' should be a column name, but it was not found.")
  }

  if (current_string_col %in% names(dat)) {
    warning("'", current_string_col,
            "' should not exist yet but it does. It will be overwritten.")
  }

  splits <- dat %>%
    mutate(tmp_action_to_take =
             case_when(
               is.na(!!sym(current_split)) ~ "remaining",
               !!sym(use_split_point) == TRUE
               & !is.na(!!sym(current_split)) ~ "split",
               !!sym(use_split_point) == FALSE ~ "copy forward"
             ),
           !!sym(current_string_col) :=
             case_when(
               tmp_action_to_take == "remaining"  ~ tmp_remaining,
               tmp_action_to_take == "split" ~ str_split_fixed(
                   tmp_remaining, split_patterns[!!sym(current_split)], 2
                   )[, 1],
               tmp_action_to_take == "copy forward" ~ !!sym(prev_string_col)
             )
    )

  return (splits)
}

#' @title Remove information that has already been captured from 'tmp_remaining'
#'
#' @description Once a 'tmp_string_' column has been created, the information it
#' contains is no longer needed for future 'tmp_string_' columns.
#' This function removes the character string in the current tmp_string column
#' from the 'tmp_remaining' column. However, we don't want 'tmp_remaining'
#' to ever be blank because we want all tmp_string_ columns filled even when not
#' all split_point patterns have a match. Therefore if there is no string left
#' for further splitting, string_ is carried forward into tmp_remaining.
#'
#' @param dat Dataframe output containing a column called 'tmp_remaining' that
#' contains the string that remains for further splitting, and
#' a column with the name given by current_string_col.
#' @param split_count integer. The number of split_patterns patterns.
#' @param current_string_col character. Name of column  with prefix 'string_'
#' This column contains at least part of what is contained in 'tmp_remaining'.
#' @param index integer - iterator from the calling funciton.
#'
#' @returns dat with current_string_col info removed from the string in
#' 'tmp_remaining' unless that would leave 'tmp_remaining' blank, in which case
#' it is left as it is.
#'
#' @examples
#' \dontrun{
#' split_patterns <- c( "(\n)\\s*1\\.", "(\n)\\s*2\\.", "(\n)\\s*3\\.")
#'
#' dat <- data.frame(
#'   tmp_remaining = c("split\n 1.this \n 2.one \n 3.thousand", "other"),
#'   tmp_use_split_point_0 = TRUE,
#'   tmp_split_1 = c(1, NA),
#'   tmp_matches_next_pattern_1 = c(FALSE, FALSE),
#'   tmp_whitespace_1 = c(TRUE, FALSE),
#'   value = c(100, 300),
#'   tmp_string_1 = c("split", "other")
#' )
#'
#' update_remaining(dat, 3, "tmp_string_1", 1)
#' }
update_remaining <- function(dat, split_count, current_string_col, index) {


  if (! "tmp_remaining" %in% names(dat)) {
    stop("'tmp_remaining' should be a column name, but it was not found.")
  }

  if (! current_string_col %in% names(dat)) {
    stop("'", current_string_col,
         "' should be a column name, but it was not found.")
  }

  remaining_updated <- dat %>%
    mutate(
      tmp_remaining =
        case_when(
          tmp_remaining == "" ~ NA,
          TRUE ~ tmp_remaining
        ),
      tmp_remaining =
        case_when(
          !is.na(tmp_remaining) & index <= split_count ~
            str_remove(tmp_remaining, fixed(!!sym(current_string_col))),
          TRUE ~ as.character(tmp_remaining)
        ),
      tmp_remaining =
        case_when(
          is.na(tmp_remaining) | tmp_remaining == "" ~
            !!sym(current_string_col),
          TRUE ~ as.character(tmp_remaining)
        )
    )

  return(remaining_updated)
}

#' @title Prevent splits happening twice in the same place
#'
#' @description In some cases (described below), the characters that match the
#' split_point pattern need to be removed from the 'tmp_remaining' column if the
#' next split_point pattern is the same.
#' For example if we take the value 'split _newline_ this _newline_ here'
#' and the split_patterns are c('newline', 'nelwine'), after the first split
#' string_1 would be 'split' and 'tmp_remaining' would be
#' '_newline_ this _newline_ here'. Without the actions of this function the
#' second split would result in string_2 being '' and tmp_remaining being
#' '_newline_ this _newline_ here',
#' because it would have split at the first newline.
#'
#' Use the columns prefixed with 'tmp_whitespace_' (which states whether the
#' split_pattern in question starts with either whitespace or newline) and
#' 'tmp_matches_next_pattern_' to identify what action needs to be taken.
#' If tmp_matches_next_pattern is TRUE, AND tmp_whitespace is FALSE the whole
#' of the split pattern is removed from the start of tmp_remaining. In all
#' other cases, white space is simply trimmed from the start tmp_remaining.
#'
#' N.B. A simpler way to do this would just be to remove one or two characters,
#' from the start of remaining in all cases but we don't do this because doing
#' so could cause the next pattern to match the beginning of tmp_remaining
#' rather than the intended match.
#'
#' @param dat Dataframe output containing columns called 'tmp_remaining'
#' (contains the string that remains for further splitting), and columns with
#' the names given by tmp_matches_next_pattern and tmp_whitespace.
#' @param split_point character string giving the regex pattern that was last
#' used to create a split.
#' @param matches_next_pattern bool. If FALSE, whitespace will be trimmed from
#' the start of tmp_remaining but no other changes to tmp_remaining will be done.
#' @param whitespace bool. If FALSE (and matches_next_pattern is TRUE), the
#' whole split_point pattern will be removed from tmp_remaining.
#'
#' @returns dat with 'tmp_remaining' column updated
#'
#' @examples
#' \dontrun{
#' split_patterns <- c("\n", "\n", "Note", "Note")
#' dat <- data.frame(
#'          col_to_split = c("split \n this \n here",
#'                           "split \n here Note this",
#'                           "Note this Note that"),
#'          tmp_use_split_point_1 = c(TRUE, TRUE, FALSE),
#'          tmp_use_split_point_2 = c(TRUE, FALSE, FALSE),
#'          tmp_use_split_point_3 = c(FALSE, TRUE, TRUE),
#'          tmp_use_split_point_4 = c(FALSE, FALSE, TRUE),
#'          tmp_use_split_point_0 = TRUE,
#'          tmp_split_1 = c(1, 1, 3),
#'          tmp_split_2 = c(2, 3, 3),
#'          tmp_split_3 = c(NA, 3, 3),
#'          tmp_split_4 = c(NA, NA, 4),
#'          tmp_string_1 = c("split", "split", "Note this"),
#'          tmp_remaining = c("\n this \n here", "\n here Note this", "Note that"),
#'          tmp_matches_next_pattern_1 = c(TRUE, FALSE, TRUE),
#'          tmp_matches_next_pattern_3 = c(FALSE, FALSE, TRUE),
#'          tmp_whitespace_1 = c(TRUE, TRUE, FALSE),
#'          tmp_whitespace_3 = c(FALSE, FALSE, FALSE)
#'          )
#'
#' dat2 <- dat %>%
#'     mutate(tmp_remaining = c("\n this \n here", "\n here Note this", "Note that"),
#'            tmp_string_2 = c("this", "here", "to"),
#'            tmp_string_3 = c("here", "here", "to"))
#'
#' # where i is 1
#' remove_repeated_pattern_from_remaining(dat, "\n", "tmp_matches_next_pattern_1",
#'                                        "tmp_whitespace_1")
#' # where i is 3
#' remove_repeated_pattern_from_remaining(
#' dat2, "Note", "tmp_matches_next_pattern_3","tmp_whitespace_3"
#' )
#' }
remove_repeated_pattern_from_remaining <- function(
    dat, split_point, matches_next_pattern, whitespace
) {

  if (!matches_next_pattern %in%  names(dat)) {
    stop("'", matches_next_pattern,
         "' should be a column name, but it was not found.")
  }

  if (!whitespace %in% names(dat)) {
    stop ("'", whitespace,
          "' should be a column name, but it was not found.")
  }

  if (!"tmp_remaining" %in% names(dat)) {
    stop ("'tmp_remaining' should be a column name, but it was not found.")
  }

  repeated_pattern_gone <- dat %>%
    mutate(tmp_remaining = case_when(
      !!sym(matches_next_pattern) & !!sym(whitespace) == FALSE ~
        str_remove(tmp_remaining, split_point),
      TRUE ~ str_trim(tmp_remaining, "left")
    ))

  return (repeated_pattern_gone)
}


#' @title Remove colons and dashes from the 'tmp_string_' columns.
#'
#' @param dat Dataframe containing columns with prefix "tmp_string_"
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(tmp_string_1 = "-Remove dash -",
#'                   tmp_string_2 = ": Remove_colon:",
#'                   tmp_string_3 = "- remove both :",
#'                   other = ":remove none-")
#'
#' trim_separators_from_new_cols(dat)
#' }
trim_separators_from_new_cols <- function(dat) {

  string_col_locs <- which(startsWith(names(dat), "tmp_string_"))
  count <- length(string_col_locs)

  if (count < 1) {
    return(dat)
  }

  for (i in 1:count) {

    current_string <- paste0("tmp_string_", i)

    dat <- dat %>%
      mutate(!!sym(current_string) :=
               trim_separators(!!sym(current_string), c(":", "-", "_")))
  }

  return (dat)

}


#' @title clean the start and end of strings
#'
#' @description Remove hyphens, colons etc, and spaces from the start and end of
#' strings
#'
#' @param string character string.
#' @param separators vector of character strings, each of which is a regular
#' expression.
#'
#' @return vector of strings. Original vector with trailing separators removed
#'
#' @examples
#' \dontrun{
#' example_vector <- c("these - ", "hyphens-", " and colons :", ":now   gone- ")
#' trim_separators(example_vector, "-|:")
#' }
trim_separators <- function(string, separators) {

  for(i in 1:length(separators)) {
    string <- string %>%
      stringr::str_trim() %>%
      stringr::str_remove_all(pattern = paste0("^", separators[i], "+")) %>%
      stringr::str_remove_all(pattern = paste0(separators[i], "+$")) %>%
      stringr::str_trim()
  }

  return(string)
}


#' @title remove all unwanted columns
#'
#' @description remove all unwanted columns generated by
#' split_to_mutliple_columns.
#'
#' @param dat Dataframe containing columns with the prefixes:
#'  "use_split_point_", "split_", "matches_next_pattern_", or "whitespace_"
remove_unwanted_split_columns <- function(dat){

  col_prefixes_to_remove <- c(
    "tmp_use_split_point_", "tmp_split_",
    "tmp_pattern_at_start", "tmp_action_to_take",
    "tmp_matches_next_pattern_", "tmp_whitespace_"
  )

  cols_removed <- select(dat, -starts_with(col_prefixes_to_remove))

  return (cols_removed)

}

#' @title assign new names to columns with the prefix "string_".
#'
#' @param dat Dataframe containing the same number of columns starting 'tmp_string_'
#' as there are elements in `to`
#' @param to character array of new column names. Must be of the same length as
#' the number of columns starting 'string_'. The first element of the vector will
#' be the new name for string_1, the second string_2 etc.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(tmp_string_2 = "10", tmp_string_1 = "B")
#' to <- c("letter", "number")
#' rename_split_columns(dat, to)
#' }
rename_split_columns <- function(dat, to) {

  if (!any(startsWith(names(dat), "tmp_string"))) {
    warning("Expected some column names to start with 'tmp_string' but none found.")
    return (dat)
  }

  for (i in 1:length(to)) {
    name_to_replace <- paste0("tmp_string_", i)
    new_name <- to[i]

    dat <- dat %>%
      rename(!!sym(new_name) := !!sym(name_to_replace))
  }

  return (dat)

}

