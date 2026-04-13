#' @title Replace words and fill blanks in specified columns
#'
#' @description Replace or edit wording of values (not column names) in the
#' columns supplied by the settings.
#'
#' @details The different types of replacement are controlled by different
#' settings (see params). For more details and examples see the documentation
#' for replace_strings, standardise_mentioned_years, and replace_blanks.
#'
#' @param replace_string_col character string giving name of column that
#' contains the text to replace. Must be exact. In pub sec this variable is
#' specified by replace_string_col.
#' @param replace_string_from_col_patterns vector of character strings giving
#' the regular expressions used to identify the locations where replacements
#' should be made. Must be in the same order as `replace_string_to`. The text
#' identified by the first element in 'replace_string_from_col_patterns' will be
#' replaced with the text given in the first element of 'replace_string_to'.
#' @param replace_string_to vector of character strings containing the new text.
#' @param replace_string_keep_original boolean. If TRUE make a copy of column
#' and rename it with '_original' as a postscript.
#' @param descriptors_to_standardise_year_in character string vector. Each
#' element must match the name of a column in dat. These are the columns in
#' which you want the replacement to be made.
#' @param year character string. A single year, either financial or calendar.
#' @param col_pattern_with_blanks_to_replace character vector. Regular
#' expression to match the name of the column in which you want to replace the
#' blanks. Must be the same length as col_pattern_to_replace_blanks_with.
#' @param col_pattern_to_replace_blanks_with character vector. Regular
#' expression to match the name of the column whose values you want to use to
#' replace the blanks in to_column. Must be the same length as
#' col_pattern_with_blanks_to_replace.
#'
#' @returns dataframe with wording cleaned up
edit_wording <- function(
    dat, replace_string_col, replace_string_from_col_patterns,
    replace_string_to, replace_string_keep_original,
    descriptors_to_standardise_year_in, year,
    col_pattern_with_blanks_to_replace, col_pattern_to_replace_blanks_with
    ) {

  strings_replaced <- replace_strings(
    dat, replace_string_col, replace_string_from_col_patterns,
    replace_string_to, replace_string_keep_original
  )
  years_generalised <- standardise_mentioned_years(
    strings_replaced, descriptors_to_standardise_year_in, year
  )
  blanks_replaced <- replace_blanks(
    years_generalised, col_pattern_with_blanks_to_replace,
    col_pattern_to_replace_blanks_with
  )

return (blanks_replaced)

}

#' @title Replace text in a specified column with different text
#'
#' @description In very particular cases we may want to change text in a
#' specified column. In most cases we should do this with user interaction that
#' ensures the correct replacement is taking place (inb pub_sec this is done in
#' Python in the pub_sec standardisation module). However, in columns that are
#' not set to be standardised with user interaction, such as vintage in pub_sec,
#' we can make text replacements using replace_strings.
#'
#' Note that the entire text of an identified cell is replaced, not just the bit
#' identified by the `from_regex`.
#'
#' @param dat DataFrame containing a column with the name given by 'column'
#' @param column character string giving name of column that contains the text
#' to replace. Must be exact. In pub sec this variable is specified by
#' replace_string_col
#' @param from_regex vector of character strings giving the regular expressions
#' used to identify the locations where replacements should be made. Must be
#' in the same order as `to`. The text identified by the first element in
#' 'from_regex' will be replaced with the text given in the first element of
#' 'to'. In pub sec this variable is specified by
#' replace_string_from_col_patterns.
#' @param to vector of character strings containing the new text.
#' In pub sec this variable is specified by replace_string_to.
#' @param keep boolean. If TRUE make a copy of column and rename it with
#' '_original' as a postscript. In pub sec this variable is specified by
#' replace_string_keep_original.
#'
#' @returns dat with replacements made.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame("vintage" = c("Near actuals", "Estimates"),
#'                   "numeric" = c(1, 2))
#' replace_strings(dat, "vintage", c("(?i)actual", "(?i)est"), c("final", "budget"))
#' }
#' @export
replace_strings <- function (dat, column=NA, from_regex=NA, to=NA, keep=FALSE) {

  if (all(is.na(column), is.na(from_regex), is.na(to))) {
    return (dat)
  }

  message("Replacing text in '", column, "' as specified in sheet_structure.")

  if (any(is.na(column), is.na(from_regex), is.na(to))) {
    stop(
      "Some but not all of the variables used to replace strings are ",
      "specified. All of 'replace_string_col', 'replace_string_from_col_patterns', ",
      "and 'replace_string_to' are required to replace specific contents ",
      "of a column with different contents. No replacements will be made."
      )
  }

  if (!column %in% names(dat)) {
    stop(
      "'", column, "' is not an existing column. Replacements will not be made."
    )
  }

  if (is.na(keep)) {

    keep = FALSE

  } else {

    new_col_name <- paste0(column, ("_original"))
    message(
      "The original 'replaced' column will be kept along with its copy, as ",
      "specified by replace_string_keep_original in the sheet structure. It ",
      "will be renamed as '", new_col_name, "'."
    )

    dat <- dat %>%
      mutate(!!sym(new_col_name) := !!sym(column))
  }

  for (i in 1: length(to)) {

    if (from_regex[i] == "NA") {

      message(
        "Replacing NA strings with '", to[i], "' in ", column, "."
      )
      dat <- dat %>%
        mutate(!!sym(column) := ifelse(
          is.na(!!sym(column)), to[i], !!sym(column)
        ))

    } else {

      message(
        "Replacing strings matching '", str_squish(from_regex[i]), "' with '",
        to[i], "' in ", column, "."
      )
      dat <- dat %>%
        mutate(!!sym(column) := ifelse(
          str_detect(!!sym(column), str_squish(from_regex[i])),
          to[i],
          !!sym(column)
        ))

    }
  }

  return(dat)
}


#' @title standardise the wording of years in specified columns
#'
#' @description
#' Where the given year is mentioned in a specified character column, replace
#' it with standardised wording. This wording differs depending on whether the
#' year is on a financial or calendar year basis.
#' If the year itself is found it is replaced replaced with "current year"/
#' "current financial year". If the prior year is found it is replaced with
#' "previous year"/ "previous financial year". If the subsequent year is found
#' it is replaced with "next year"/ "next financial year".
#'
#' @param dat dataframe
#' @param columns character string vector. Each element must match the name of
#' a column in dat. These are the columns in which you want the replacement to
#' be made. In pub sec this variable is specified by
#' descriptors_to_standardise_year_in.
#' @param year character string. A single year, either financial or calendar.
#'
#' @return dataframe dat with replacements made. NOTE: in the unlikely case
#' of an entry with both a calendar year to replace and a financial year that
#' includes that calendar year. e.g. '2022 and 2022-23' will change to 'current
#' year and current year-23'. However '2022 and 2023-24' would be fine. As it
#' is currently written, this function will also only replace financial years
#' that are in the same format as the supplied year i.e. '2024-2025' would not
#' get replaced if year was supplied as '2025-26'.
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'     desc_1 = c("Total 2022-23", "Total 2023-24", "Total 2024-25", "Total"),
#'     desc_2 = c("2022-23", "2023-24", "2024-25", "Total"),
#'     original_1 = c("Total 2022-23", "Total 2023-24", "Total 2024-25", "Total")
#'     )
#' standardise_mentioned_years(dat, c("desc_1", "desc_2"), "2023-24")
#' }
#' @export
standardise_mentioned_years <- function(dat, columns = NA, year) {

  if (all(is.na(columns))) {
    return (dat)
  }

  message(
    "Standardising the wording of years given in the following columns: '",
    paste0(columns, collapse = "', '"), "'."
  )

  if (length(year) > 1) {
    stop(
      "More than one year has been provided. Year text will not be ",
      "standardised in the following columns, despite this action being set ",
      "in settings. Year is currently taken from the file name. If it needs ",
      "to be taken from elsewhere please contact a developer to edit the function."
    )
  }

  if (! all(columns %in% names(dat))) {
    stop(
      "Some columns are not in the data. Please contact a developer"
    )
  }

  # if it is a financial year we should make this clear in the standardised text
  # to avoid confusion, but that is not necessary for calendar years.
  patterns <- make_year_patterns()
  year_type <- get_year_type(year)

  if(is.na(year_type)) {
    stop(
      "year is not a valid calendar or financial year. Year text will not ",
      "be standardised. Please contact a developer if this is an issue."
    )
  } else if (year_type == "financial") {
    current_year_wording <- "current financial year"
    previous_year_wording <- "previous financial year"
    next_year_wording <- "next financial year"
    previous_year <- get_bookend_year(year, -1, "financial")
    next_year <- get_bookend_year(year, 1, "financial")

  } else if (year_type == "calendar") {
    current_year_wording <- "current year"
    previous_year_wording <- "previous year"
    next_year_wording <- "next year"
    previous_year <- get_bookend_year(year, -1, "calendar")
    next_year <- get_bookend_year(year, 1, "calendar")

  } else {
    stop(
      "year type is not recognised - has get_year_type changed?"
    )
  }

  message(
    "If they exist, instances of '", year, "' will be replaced with '",
    current_year_wording, "', '", previous_year, "' with '",
    previous_year_wording, "', and '", next_year, "' with '", next_year_wording,
    "'."
  )

  pattern <- make_year_patterns()[year_type]

  for (column in columns) {

    prep <- dat %>%
      rowwise() %>%
      mutate(
        `_to_edit` = str_extract_all(!!sym(column), pattern),
        `_matches` = length(`_to_edit`)
      ) %>%
      ungroup()

    max_matches <- max(prep["_matches"])

    # If more than one year of the right type is mentioned in `column` we need
    # to repeat the str_replace for each. I expect there is a better way to do
    # this. str_replace_all does not replace all matches for some reason, and
    # the list created by str_extract_all causes difficulties hence using this
    # loop with str_extract and str_replace not str_extract_all and
    # str_replace_all.
    for(i in 1:max_matches) {
      dat <- dat %>%
        mutate(
          `_to_edit` = str_extract(!!sym(column), pattern),
          !!sym(column) := case_when(
            str_detect(`_to_edit`, year) ~
              str_replace(!!sym(column), year, current_year_wording),
            str_detect(`_to_edit`, previous_year) ~
              str_replace(!!sym(column), previous_year, previous_year_wording),
            str_detect(`_to_edit`, next_year) ~
              str_replace(!!sym(column), next_year, next_year_wording),
            TRUE ~ !!sym(column)
          )
        )
    }

  }

  output <- select(dat, -`_to_edit`)
  return (output)

}


#' @title Fill blanks in one column with values from another
#'
#' @description Where there is an NA in a specified column, fill it with the
#' value from another.
#'
#' @param dat dataframe
#' @param to_columns character vector. Regular expression to match the name of
#' the column in which you want to replace the blanks. Must be the same length
#' as from_column. In pub sec this variable is specified with
#' col_pattern_with_blanks_to_replace.
#' @param from_columns character vector. Regular expression to match the name of
#' the column whose values you want to use to replace the blanks in to_column.
#' Must be the same length as to_column. In pub sec this variable is specified
#' with col_pattern_to_replace_blanks_with.
#'
#' @returns dataframe with blanks replaced
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'     Code = c(NA, "E08000001"),
#'     Region_code = c("E92000001", "E12000002"),
#'     LA = c(NA, "Bolton"),
#'     Region = c("England", "North West")
#'     )
#' replace_blanks(
#'     dat, c("(?i)^code$", "(?i)la"), c("(?i)region.code", "(?i)region$")
#'     )
#' }
replace_blanks <- function(dat, to_columns = NA, from_columns = NA) {

  if (all(is.na(to_columns), is.na(from_columns))) {
    return(dat)
  }
  message("Replacing blanks with values from another column.")

  if(length(to_columns) != length(from_columns)) {
    stop(
      "col_pattern_with_blanks_to_replace must have the same number of elements as ",
      "col_pattern_to_replace_blanks_with."
    )
  }

  if(any(is.na(to_columns), is.na(from_columns))) {
    stop(
      "Only one of col_pattern_with_blanks_to_replace or col_pattern_to_replace_blanks_with ",
      "is specified. If one is specified, they both must be."
    )
  }

  for (i in 1:length(to_columns)) {
    to <- get_matching_colnames(dat, to_columns[i])
    from <- get_matching_colnames(dat, from_columns[i])

    if (length(to) == 0) {
      warning(
        "col_pattern_with_blanks_to_replace is specified as '", to_columns[i], "' ",
        "in the settings but no matching column has been found.",
        "If this causes issues please contact a developer."
      )
      next

    } else if (length(to) > 1) {
      warning(
        "Multiple matching column names have been found for ",
        "col_pattern_with_blanks_to_replace in the settings ('", to_columns[i], "'). ",
        "No replacements wil lbe made for this column. If this causes issues ",
        "please contact a developer."
      )
      next
    }
    if (length(from) == 0) {
      warning(
        "col_pattern_to_replace_blanks_with. is specified as '", from_columns[i], "' ",
        "in the settings but no matching column has been found.",
        "If this causes issues please contact a developer."
      )
      next

    } else if (length(from) > 1) {
      warning(
        "Multiple matching column names have been found for ",
        "col_pattern_to_replace_blanks_with. in the settings ('", from_columns[i], "') ",
        "If this causes issues please contact a developer"
      )
      next
    }

    dat <- dat %>%
      mutate(!!sym(to) := case_when(
        is.na(!!sym(to)) | str_squish(!!sym(to)) == "" ~ !!sym(from),
        .default = as.character(!!sym(to))
      ))
  }

  return(dat)
}


#' @title Remove line breaks from character strings.
#'
#' @description Remove line breaks from character strings.
#'
#' @param dat dataframe.
#'
#' @returns dataframe with line breaks from all character strings.
#'
#' @examples
#' \dontrun{
#' dat <- tibble::tibble(
#'     "Service"  = c(
#'        "Education\nServices","Education Services",
#'        "Education\n\n Services",
#'        "\nEducation\n Services\n"
#'        ),
#'    "Transaction" = c(
#'        "Expenditure\r",
#'        "\rExpenditure",
#'        "\nExpenditure\n",
#'        "Expenditure"
#'        ),
#'    "Units" = c("thousands","thousands","hundreds", "millions")
#'    )
#'
#' result <- remove_line_breaks(dat)
#'}
#' @export
remove_line_breaks <- function(dat){

  message("Removing line breaks from all character strings.")

  output <- dat %>%
    mutate(across(where(is.character), ~ gsub("[\n\r]+", " ", .))) %>%
    mutate(across(where(is.character), ~ stringr::str_squish(.)))

  return(output)
}
