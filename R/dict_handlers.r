#' @title Convert instructions into a regex pattern
#'
#' @description It can be problematic to pass regular expressions from Python
#' through subprocess. It is hard to isolate which patterns cause issues, but
#' the error raised in Python is '<x> is not recognized as an internal or
#' external command'.
#' This function uses standard strings that don't cause problems in subprocess
#' to build the regular expression inside R.
#'
#' @param instructions character string vector. Must only contain words given in
#' regex_lookup. Each string in the vector can have multiple instruction words 
#' in it. Each instruction word is converted to a regular expression and these 
#' are pasted together to create a single pattern from each string. Must not
#' contain punctuation.
#'
#' @returns a vector of regular expressions; one for each instruction string
#' Raises an error if there is an instruction that does not appear in
#' regex_lookup.
#'
#' @examples
#' \dontrun{
#'
#' instructions <- c("(?i)a[0-2]b", "^year$")
#' build_regex(instructions)
#'
#' instructions <- c("ALT_colon_whitespace", "ALT_newline_poundsign_newline")
#' build_regex(instructions)
#'}
#' @export
build_regex <- function(instructions){

  if (all(is.na(instructions))) {
    return(instructions)
  }

  if (!all(str_starts(instructions, "ALT_"))) {
    return(instructions)
  } else {
    for (i in 1:length(instructions)) {
      instructions[i] <- str_remove(instructions[i], "ALT_")
    }
  }

  regex_lookup <- data.frame(
    "word" = c("start", "end", "or",
               "anyrepeats", "anycase", "anychar", "fromnorepeatsto", "endrepeats",
               "period", "colon", "hyphen", "poundsign",
               "openroundbrace", "closeroundbrace",
               "whitespace", "dash", "newline", "nocharacter",
               "number", "word", "letter", "underscore"),
    "pattern" = c("^", "$", "|",
                  "*", "(?i)", ".", "{0,", "}",
                  "\\.", "\\:", "-", paste0("\\", enc2utf8("\u00A3")),
                  "\\(", "\\)",
                  "\\s*", "\\s+-\\s+", "(\r\n|\r|\n)", "\\b",
                  "\\d", "\\w", "[a-z, A-Z]", "_")
  )

  if (any(startsWith(instructions, "_"))) {
    stop("instructions must not start with '_'")
  }

  tryCatch(
    {
      instruction_list <- strsplit(instructions, "_")

      pattern <- NULL
      instruction_patterns <- NULL

      for (i in 1:length(instruction_list)) {

        components <- instruction_list[[i]]

        for (j in 1: length(components)) {

          keep_as_is <- grepl("(^\\[).*(\\]$)", components[j])
          keep_no_equals <- grepl("^=", components[j])

          if (keep_as_is) {

            pattern <- paste0(pattern, components[j])

          } else if (keep_no_equals) {

            pattern <- paste0(pattern, substr(components[j], 2, nchar(components[j])))

          } else if (components[j] %in% regex_lookup$word){

            pattern_part <- regex_lookup %>%
              filter(word == components[j]) %>%
              pull(pattern)

            pattern <- paste0(pattern, pattern_part)

          } else {
            stop(
              "'",
              components[j],
              "' passed to R from the data dictionary is not present in the ",
              "regex_lookup. Please ask a developer to edit the dictionary or ",
              "add this to the regex_lookup."
            )
          }
        }
        instruction_patterns <- c(instruction_patterns, pattern)
        pattern <- NULL
      }
    },
    error = function(e) {
      stop(e)
    }
  )

  return(instruction_patterns)
}


#' @title Generate a dictionary-style dataframe from an input string
#'
#' @description The form of a Python dict brought into R as a string is
#' "key: item, key: item", with or without curly braces.
#'
#' To access items using their key we convert this string into a dataframe
#' where the first column contains keys, and the second column contains item
#' lists.
#'
#' This is only written for use with simple dictionaries, not nested dicts.
#'
#' @param input_string A character string containing key-value pairs separated
#' by commas. See example.
#'
#' @returns A data frame representing a dictionary with keys corresponding
#' to a list of items. If the input string is empty, an empty data frame is 
#' eturned.If the input string contains any empty keys, they are replaced with 
#' NA values.
#'
#' @examples
#' \dontrun{
#' # simple
#' input_string <- "key1: value1, key2: value2, key3: value3"
#' result_dict <- string_to_dict(input_string)
#'
#' # nested
#' input_string <- "key1: [val1, val1b], key2: [val2, val2b], key3: val3"
#' result_dict <- string_to_dict(input_string)
#' }
#' @export
string_to_dict <- function(input_string) {

  if (input_string == "{}") {
    warning("Empty input string provided. Returning an empty dictionary.")
    return(data.frame(key = character(0), item = character(0)))
  }

  # We no longer need the curly braces - they are just an artifact of bringing
  # a dict into r as a string
  no_braces <- stringr::str_remove_all(input_string, "\\{|\\}")
  squished <- stringr::str_squish(no_braces)
  no_quotes <- stringr::str_replace_all(squished, '"', '')
  no_colon_spaces <- stringr::str_replace_all(no_quotes, " :", ":")

  # find the location of the keys so that we can split the string into keys and
  # items look for any word (including underscores) immediately followed by a
  # colon
  key_locs <- stringr::str_locate_all(no_colon_spaces, "\\w+:")[[1]]

  # so that the split occurs just after the colon,
  # we need to add 1 to the end loc
  split_locs <- c((key_locs[, "start"] - 1),
                  (key_locs[, "end"] + 1))

  # The first split location will always be 0 so we need to remove this one.
  # We need to order the locations as we will split the string in the order
  # given in this vector
  split_points <- sort(split_locs[2:length(split_locs)])

  key_item_vector <- substring(no_colon_spaces,
                               c(1, split_points),
                               c(split_points, nchar(no_colon_spaces)))

  keys <- tibble(key_item_vector) %>%
    filter(row_number() %% 2 != 0 ) %>%
    rename(key = key_item_vector) %>%
    mutate(key = stringr::str_remove(key, ": "),
           key = stringr::str_squish(key))

  items <- tibble(key_item_vector) %>%
    filter(row_number() %% 2 == 0 )  %>%
    rename(item = key_item_vector) %>%
    mutate(item = trimws(item, whitespace = "[, ]")) %>%
    # because of how it is passed from Python, the first and last character of
    # each item at this point will be a square brace. We need to remove these.
    # Doesn't need case_when if it always came from Python, but during dev
    # we pass arguments from the pre-processing_setup.r file, which could cause
    # issues if we forgot to put the square braces in.
    mutate(item =
             ifelse(
               substr(item, 1, 1) == "[" &
                 substr(item, nchar(item), nchar(item))=="]",
               substr(item, 2, nchar(item)-1),
               as.character(item)
             )
    )

  dict <- bind_cols(keys, items) %>%
    mutate(item = stringr::str_split(item, ","))

  return(dict)
}


#' @title Remove invalid variables from the settings.
#'
#' @description In pub sec all possible R variable names are passed to R from
#' Python as a list (r_vars). If there is a variable in the dict that is not in
#' r_vars it cannot be created as an R variable. Remove it from the dict with a
#' warning.
#'
#' @param dict dataframe with columns 'key' and 'item'. 'key' gives the name of
#' the variable to be, 'item' the value of that variable.
#' @param variables character string vector. The name of the variables that are
#' to be created in R.
#'
#' @returns dataframe. dict with invalid rows removed. A warning is given for
#' each row that is removed.
#'
#' @examples
#' \dontrun{
#' dict <- data.frame(key = c("do_thing", "sing"), item = c("jump", "A"))
#' variables <- c("do_thing", "ignore_thing")
#' remove_invalid_args(dict, variables)
#' }
#' @export
remove_invalid_args <- function(dict, variables) {

  keep <- NULL

  for (i in 1:nrow(dict)) {
    if (dict$key[i] %in% variables) {
      keep <- c(keep, i)
    } else {
      warning(
        "Invalid variable found in settings: '", dict$key[i], "'. Variable ",
        "will need to be added to the r_vars list if it is required as part ",
        "of preprocessing."
        )
    }
  }

  filtered <- dict[keep, ]
  return(filtered)
}


#' @title Get value for a key in a string_to_dict style dataframe
#'
#' @description When a dict is passed from python and converted to a dataframe
#' by string_to_dict() it has a key column and a value column. Get the value
#' on the row of the given key.
#'
#' @param key character string to look up the value for.
#' @param dict dataframe with key and item columns.
#'
#' @return The item from dict for the key in question. If the key does not
#' exist NA is returned.
#'
#' @examples
#' \dontrun{
#' layout_dict <- data.frame("key" = c("header", "group"),
#'                            "item" = c("NA", "mice"))
#' group <- get_dict_item("header", layout_dict)
#' print(group)
#' }
#' @export
get_dict_item <- function(key, dict) {

  if (key %in% dict$key) {
    item <- stringr::str_squish(dict$item[dict$key == key][[1]])
    if (all(item == "NA") | all(is.na(item))) {
      item <- NA
    } else if (all(item == "true" | item == "false")) {
      # If all elements of a variable are true/false/NA they will already have
      # been converted to TRUE/FALSE, but if the separator pattern is included
      # they will not be because all values would have been read as strings.
      item <- as.logical(item)
    }

  } else {
    item <- NA
  }
  return(item)

}


#' @title Split dictionary style dataframe of arguments by a specified string
#'
#' @description The dictionary of arguments has a key column that gives the
#' variable name and an item column that gives the values for each variable.
#' Some variables can contain values for multiple tables. In such cases, the
#' arguments for one table will be separated from the arguments for the next
#' table by a separator string. e.g. if the separator string is 'next_table',
#' and an arg for table 1 is TRUE, but for table 2 it is FALSE, that variable
#' will have the item value c(TRUE, 'next_table', FALSE). This function splits
#' the args for each table into separate columns in the dictionary, based on
#' the separator string. Where the same value is required for all tables the
#' separator need not be included - the same value will be given for each table.
#'
#' @details
#' A 'dictionary' dataframe that looks like this:
#' | __key__ | __item__ |
#' |----|----|
#' |multitable_arg_separator | next |
#' |header_identifier | c('p1', 'next', 'p2') |
#' |year_column_name  | ^year$ |
#' 
#' has the arguments split out by the separator string into new columns like so:
#' 
#' |  __key__ | __item__ | __item_1__ | __item_2__ |
#' |----|----|----|----|
#' | multitable_arg_separator | next | next | next |
#' | header_identifier | c('p1', 'next', 'p2') | 'p1' | 'p2' |
#' | year_column_name | ^year$ | ^year$ | ^year$ |
#'
#' @param dict dataframe with columns 'key' and 'item'. 'key' gives the name of
#' the variable to be, 'item' the value of that variable.
#' @param separator character string giving the string that separates args.
#'
#' @returns dataframe. dict with arguments split by the separator into new
#' columns prefixed with 'item_'. The number of 'item_' columns is equal to the
#' maximum number of identified separators for a single row + 1 (i.e. the number
#' of tables). If the separator is not present in a row, the same value is
#' returned in every item_ column.
#'
#' @examples
#' \dontrun{
#' dict <- tibble(
#'     key = c("var1", "var2", "var 3"),
#'     item = list(c("a1", "a2", "next", "b", "next", "c"),
#'                 c("d", "next", "e", "next", "f"), ("1"))
#'     )
#'
#' result <- split_args_by_separator(dict, separator = "next")
#' }
#' @export
split_args_by_separator <- function(dict, separator) {

  if (is.na(separator)) {
    message("No split required as separator is not specified.")
    return(dict)
  }

  message("Splitting arguments by '", paste0(separator), "'.")

  if (length(names(dict)) != 2) {
    stop("dict must contain only 'key' and 'item' columns.")
  }

  if (sum(c("key", "item") %in% names(dict)) < 2) {
    stop("dict must contain the columns 'key' and 'item'.")
  }

  if (! separator %in% unlist(dict$item)) {
    stop(
      "separator is specified but is not present for any variable. If you are ",
      "processing multiple tables, '", separator, "' should be used to show ",
      "where the args for one table ends and the args for the next table ",
      "start. If only one table is to be processed, do not specify separator."
      )
  }

  table_counts <- dict %>%
    rowwise() %>%
    mutate(
      item_string = paste(item, collapse = ", "),
      separator_count = str_count(item_string, separator),
      table_count = case_when(
        item_string == separator ~ 1,
        item_string != separator ~ sum(separator_count) + 1
      )
    ) %>%
    ungroup()

  max_tables <- max(table_counts$table_count)
  wrong_length <- table_counts %>%
    filter(!table_count %in% c(1, max_tables)) %>%
    distinct(table_count)

  if (nrow(wrong_length) > 0) {
    stop(
      "Not all settings that include the separator string are of the same ",
      "length. All variables that contain the separator string as one of the ",
      "elements must contain it the same number of times."
      )
  }

  # remove columns we no longer need and add a column to give the locations of
  # the separators - we could do this in the loop but it is quicker to do here
  new_dict <- table_counts %>%
    select(-c(item_string, separator_count)) %>%
    rowwise() %>%
    mutate(
      var_ends = case_when(
        table_count > 1 ~ list(
          c(which(str_detect(item, separator)) - 1, length(item))),
        .default = list(1)
      ),
      var_starts = case_when(
        table_count > 1 ~list(c(1, which(str_detect(item, separator)) + 1)),
        .default = list(1)
      )
    ) %>%
    ungroup()

  for (i in 1:max_tables) {

    colname <- paste0("item_", i)
    new_dict <- new_dict %>%
      rowwise() %>%
      mutate(
        !!sym(colname) := ifelse(
          table_count > 1, list(item[var_starts[i]:var_ends[i]]), list(item))
        )
    }

  cleaned <- new_dict %>%
    select(-c(table_count, var_starts, var_ends))

  return(cleaned)
}


#' @title Where args have been split by separator, retain the relevant column
#'
#' @description When args are split by separator, they are separated into
#' columns. This function retains only the relevant column for the table in
#' question.
#' 
#' @details See split_args_by_separator for more information.
#'
#' @param dict dataframe with a column for key and one or more item columns.
#' @param part integer. The number of the item column to keep.
#'
#' @returns dict with a column for key and a single column for item.
#'
#' @examples
#' \dontrun{
#' dict <- tibble(
#'     key = c("var1", "var2"),
#'     item = list(NA),
#'     item_1 = c(list(c("a", "b")), list("1")),
#'     item_2 = c(list("c"), list(NA))
#'     )
#' result <- select_relevant_args(dict, 1)
#' result <- select_relevant_args(dict, 2)
#' }
#' @export
select_relevant_args <- function(dict, part) {

  if (length(names(dict)) == 2) {
    if (all(names(dict) == c("key", "item"))) {
      return(dict)
    } else {
      stop("dict must contain 'key' and 'item' columns.")
    }
  }

  if (! "key" %in% names(dict)) {
    stop("dict must contain the column 'key'.")
  }

  message("Selecting args for part ", part, ".")

  col_to_keep <- paste0("item_", part)

  if (! col_to_keep %in% names(dict)) {
    stop("'", col_to_keep, "' not found.")
  }

  current_args <- dict %>%
    select(-item) %>%
    rename(item = !!sym(col_to_keep)) %>%
    select(key, item)

  return(current_args)

}
