#' @title reformat_multi_table_data
#' @description  Reformat data with multiple tables in one sheet
#' @author Emma Wood \email{emma.wood@ons.gov.uk}
#' @author Mark London \email{mark.london@ext.ons.gov.uk}
#' @details
#' Reformat data with multiple tables in one sheet
#'
#' Used for cases where data have multiple tables in one sheet, with tables
#' stacked vertically. Each table only has one header row. Each table is split
#' by row headers (e.g. sub-services are listed under a row containing the
#' service name in the sub-service column, and no data in the other columns)
#'
#' The data column names from the original table are carried over to the output.
#'
#' This is used for e.g. 2022 Scottish Gov POBE data.
#'
#'
#' @param dat tidyxl::xlsx_cells table. Must include info above the first header row
#' @param table_heading string. Taken from the data dictionary.
#' Number of elements must equal the number of parts labelled 'Part ...' in  source data
#' @param table_split_points vector of regular expressions. The first pattern
#' will be used to make the initial split, the second, the second split etc
#' @param group_row string. New column name for the grouping variables.
#' Number of elements must equal the number of parts labelled 'Part ...' in  source data
#' @param nested_row_1 string. New column name for the nested variable.
#' Number of elements must equal the number of parts labelled 'Part ...' in  source data
#' @param group_col string. Taken from the data dictionary
#'
#' @return tibble. Tidy dataframe containing data from all tables.
#'
#' @examples
#' EW To do
#'
#' @export
reformat_multi_table_data <- function(dat,
                                      table_split_points,
                                      table_heading, subtable_heading,
                                      group_row, nested_row_1, group_col) {

  #--> separate the data out into individual tables -
  # each table has its own set of headings in the raw data
  split_pattern <- paste0(table_split_points, collapse="|")
  partitioned_data <- split_table(dat, split_pattern)

  tables_count <- length(partitioned_data[["cells"]])
  parts_count <- length(grep("^Part", partitioned_data$character))

  #--> check the info from the data dict fits the layout
  if(length(table_heading) != parts_count |
     length(nested_row_1) != parts_count |
     length(group_row) != parts_count) {

    stop("Multi-table data has not been split into the expected number of tables. In the data_dict.toml, the number of items for table_heading, group_row, and nested_row_1 should all equal the number of parts labelled 'Part ...' in source data. ",
         parts_count, " 'parts' were found in the data. The data_dict may need updating. However if tables can no longer be identified using '^Part', reformat_multi_table_data() will need to be edited by a developer")
  }

  #--> initialise variables
  part_name <- NA

  # We behead each sub-table separately, but we want to return a single table that
  # contains all the subtables, which we will call joined_tables.
  joined_tables <- NULL

  for (i in 1:tables_count) {
    message(paste0("preprocessing table ", i))

    sub_table <- partitioned_data[["cells"]][[i]]

    # there is an extra cell in the title of the table that we need to remove
    # otherwise the initial behead to get the title of the table ('up-left')
    # will give the wording in the extra cell rather than the name of the table
    for_beheading <- sub_table %>%
      remove_cells(pattern = "thousand|million") %>%
      remove_cells(pattern = "be completed") %>%
      remove_empty_lines("row")

    # The first iteration (i=1) will be part 1 i.e. the first major table.
    # However, the second iteration (i=2) might be either part 2, or a sub-table
    # of part 1. If the latter, 'part' and the iteration number 'i' will differ.
    # - 'part' is used to step through the items brought in from the nesting_dict
    #    (e.g. header_identifier).
    # - 'i' is used to step through the individual tables, whether they are
    #    sub-tables or not.
    if (i == 1) {
      part <- i
    } else if(str_detect(for_beheading$character[1], "^Part") == TRUE) {
      part <- part + 1
    }

    message(paste0("table ", i, " is in 'part ", part, "' of the source data"))

    # some 'parts' do not contain any data, for example if it only contains
    # other tables, it will not contain data (as the numbers will all be in it's sub_tables)
    # However, if a table does contain numbers in any rows other than the first row
    # it needs to be pre-processed.
    # (there may be random admin numbers in the heading row as in POBE 2024-25 budget year)
    rows_with_data <- for_beheading %>%
      mutate(heading_row = min(row)) %>%
      filter(row > heading_row & data_type == "numeric") %>%
      nrow()

    table_contains_data <- rows_with_data > 0
    if (table_contains_data) {

      if (subtable_heading[part] == "NA" | is.na(subtable_heading[part])){
        heading <- table_heading[part]
      } else {
        heading <- subtable_heading[part]
      }

      beheaded <- behead_multi_table(dat = for_beheading,
                                     heading = heading,
                                     nested = nested_row_1[part],
                                     columns = group_col)

      # in pobe capital 2021 there are some rows that are just notes but
      # they mess up how the group_row is assigned and thus how some items
      # are distinguished from other items.
      pre_clean_fix <- beheaded %>%
        mutate(problematic_entry = ifelse(
          grepl(
            "explanation required in column",
            tolower(!!sym(nested_row_1[part]))
          )
          & (is.na(numeric) | numeric == 0),
          TRUE, FALSE)) %>%
        filter(problematic_entry != TRUE) %>%
        select(-problematic_entry)

      # remove the number from the start of table titles,
      # and put row_headers and notes in their own columns
      beheaded_cleaned <- clean_multi_table(dat = pre_clean_fix,
                                            heading = heading,
                                            nested = nested_row_1[part],
                                            columns = group_row[part])

      part_needs_adding <- (heading == subtable_heading[part])

      if (part_needs_adding) {
        # paste the subtable heading to the table heading
        beheaded_cleaned <- beheaded_cleaned %>%
          mutate(!!sym(heading) :=
                   ifelse(
                     !is.na(part),
                     paste(part_name, "-", !!sym(heading)),
                     !!sym(heading)
                   ))
      }


      problematic_repeats <- get_repeats_with_different_values(
        dat = beheaded_cleaned, columns = group_col,
        repeats_column = nested_row_1[part]
      )

      repeats_differentiated <- differentiate_repeats(
        dat=problematic_repeats, repeats_column=nested_row_1[part],
        differentiator=group_row[part]
      )

      joined_tables <- bind_rows(joined_tables, repeats_differentiated)

    } else if (table_contains_data == FALSE){
      # because there is no data to process in the first split of a table that
      # contains other tables, we store the part name for use in it's sub-tables
      part_name <- for_beheading$character[1]
    }

  }

  #---> make final changes to the whole dataset
  if(group_col == "year_and_vintage") {
    year_and_vintage_added <- split_year_and_vintage(
      dat = joined_tables,
      from_col = group_col,
      columns = group_col)
  }

  # if notes is one of the entries for table_heading, group_row, or
  # nested_row_1, we need to append these notes to the temporary_notes column
  # so that all notes are in the same place. If not, we just need to change the
  # name of the extra notes column
  if ("notes" %in% names(year_and_vintage_added)) {
    joined_tables_with_notes <- concatenate_columns(year_and_vintage_added,
                                                    "notes", "temporary_notes")

  } else {

    joined_tables_with_notes <- year_and_vintage_added %>%
      mutate(notes = temporary_notes) %>%
      select(-temporary_notes)
  }

  # 'part A', 'part B' etc are not easy to standardise so remove the part letter
  part_columns <- unique(table_heading)
  for (column in part_columns) {
    part_letters_removed <- joined_tables_with_notes %>%
      mutate(!!sym(column) := str_remove(!!sym(column), "^Part\\s[\\w\\d]"))

  }

  return(part_letters_removed)

}
