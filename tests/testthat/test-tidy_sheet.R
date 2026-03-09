filepath <- file.path(test_path("testdata"), "examples.xlsx")


test_that("Example 1 is processed as expected", {

  settings <- "{
  header_identifier: (?i)measure,
  header_identifier_instance: 2,
  columns_to_create: full_description,
  column_to_right_of_data_name_pattern: code,
  dropdown_pattern: (?i)excluding,
  dropdown_name: geography,
  single_year_of_data: true,
  single_vintage: final,
  header_to_split: full_description,
  header_split_to: description_1, description_2, units,
  split_points: ALT_colon_newline, ALT_newline_poundsign
  }"

  arg_values <- c("--args", filepath, "example 1", NA, settings, "1")

  expected <- tibble (
    sheet = "example 1",
    code = c(rep("ga1", 4), rep("ga2", 4), rep("ga3", 4), rep("ga4", 4),
             rep("gatot", 4), NA, NA, NA),
    address = c("B11", "C11", "D11", "E11", "B12", "C12", "D12", "E12",
                "B13", "C13", "D13", "E13", "B14", "C14", "D14", "E14",
                "B15", "C15", "D15", "E15", "B16", "C16", "D16"),
    value = c(10, 1, 11, 30, 20, 2, 22, 40, 30, 3, 33, 50, 40, 4, 44, 60,
              100, 10, 110, 180, NA, NA, NA),
    non_numeric_value = c(rep(NA, 20), "fixed", "fixed", "fixed"),
    group = c(rep(c("A1", "A2", "A3", "A4", "Total"), each = 4), NA, NA, NA),
    full_description = c(
      rep(c("Measure 1: part a £ thousand", "Measure 1: part b £ thousand",
            "Measure 1 £ thousand", "Measure 2 £ thousand"), times = 5),
      "Measure 1: part a £ thousand", "Measure 1: part b £ thousand",
      "Measure 1 £ thousand"
    ),
    description_1 = c(rep(c(rep("Measure 1", 3),  "Measure 2"), 5),
                      rep("Measure 1", 3)),
    description_2 = c(rep(c("part a", "part b", "Measure 1", "Measure 2"), 5),
                      "part a", "part b", "Measure 1"),
    units = "£ thousand",
    geography = "England (grossed, excluding double counting)",
    title = "Example 1 2023-24",
    vintage = "final",
    # supplier = "testthat",
    source = "testdata",
    dataset = as.character(NA),
    year = "2023-24",
    year_type = "financial",
    year_notes = NA,
    fy_start = "2023"
  )

  result <- suppressWarnings(
    suppressMessages(tidy_sheet(arg_values, to_csv = FALSE))
  ) %>%
    select(-supplier)

  expect_equal(result, expected)
})


test_that("Example 2 is processed as expected", {

  settings <- "{
  header_identifier: (?i)source,
  table_split_dirs: col,
  tables_to_split: 1,
  table_split_patterns: (?i)source,
  table_split_pattern_instances: 2,
  tables_to_process: 2, 3,
  multitable_arg_separator: -next-,
  columns_to_create: year, -next-, day_month_year,
  extend_row_pattern: ALT_anycase_=of_whitespace_=which,
  extend_row_order: reverse,
  extend_row_with: above,
  POSIX_column: NA, -next-, day_month_year,
  columns_to_rename_patterns: source_1, Source,
  columns_to_rename_names: description_1, description_1,
  single_vintage: final
  }"

  arg_values <- c("--args", filepath, "example 2", NA, settings, "1")

  expected <- tibble (
    sheet = "example 2",
    address = c("B8", "C8", "B9", "C9", "B10", "C10", "B11", "C11",
                "B12", "C12", "B13", "C13",
                "F8", "G8", "F9", "G9", "F10", "G10", "F11", "G11",
                "F12", "G12", "F13", "G13"),
    value = c(10, 11, 5, 5, 20, 32, 10, 20, 10, 12, 56, 56, 10, 11, 5, 5,
              20, 32, 10, 20, 10, 12, 56, 56),
    non_numeric_value = as.character(NA),
    description_1 =
      rep(rep(
        c("A", "A _of which x", "B", "B _of which x", "B _of which y", "C"),
        each = 2), times = 2),
    year = c(rep(c("2019-20", "2020-21"), times = 6), rep("2025", times = 12)),
    title = "Example 2: years as column names (one table financial, one calendar), and where some row names that refer to the row name above",
    units = as.character(NA),
    vintage = "final",
    # supplier = "testthat",
    source = "testdata",
    dataset = as.character(NA),
    year_type = c(rep("financial", 12), rep("calendar", 12)),
    year_notes = NA,
    fy_start = c(rep(c("2019", "2020"), times = 6), rep(NA, times = 12)),
    day_month_year = c(
      rep(NA, 12), rep(c("2025-01-01", "2025-03-01"), times = 6)
    ),
    day = c(rep(NA, 12), rep(1, 12)),
    month = c(rep(NA, 12), rep(c("Jan", "Mar"), times = 6)),
  )

  result <- suppressWarnings(
    suppressMessages(tidy_sheet(arg_values, to_csv = FALSE))
  ) %>%
    select(-supplier)

  expect_equal(result, expected)
})


test_that("Example 3 is processed as expected", {

  settings <- "{
  header_identifier: (?i)group,
  header_row_offset: 1,
  table_split_dirs: col, row,
  tables_to_split: 1, 2,
  table_split_patterns: (?i)table\\s*3, (?i)table\\s*2,
  tables_to_process: 3, 4, 5,
  multitable_arg_separator: -next-,
  table_header_identifier: (?i)group, -next-, (?i)year, -next-, (?i)group,
  columns_to_create: year, -next-, description_1, -next-, year,
  table_title_column: table,
  tidy_data: true,
  tidy_notes_name: vintage, -next-, NA, -next-, NA,
  single_vintage: NA, -next-, final, -next-, final,
  replace_string_col: vintage, -next-, NA, -next-, NA,
  replace_string_from_col_patterns: (?i)f, NA, -next-, NA, -next-, NA,
  replace_string_to: budget, final, -next-, NA, -next-, NA,
  columns_to_combine_patterns: NA, -next-, NA, -next-, (?i)^group, (?i)sub,
  columns_to_combine_combined_names: NA, -next-, NA, -next-, description_1,
  columns_to_rename_patterns: (?i)^group, -next-, NA, -next-, NA,
  columns_to_rename_names: description_1, -next-, NA, -next-, NA,
  }"

  arg_values <- c("--args", filepath, "example 3", NA, settings, "1")

  expected <- tibble (
    sheet = "example 3",
    vintage = c("final", "budget", rep("final", 5)),
    value = c(200, 150, NA, 100, 200, 200, 150),
    description_1 = c("A", "B", "*f: forecast", "A", "A", "A - a", "A - b"),
    year = c(rep("2021-22", 3),"2020-21", rep("2021-22", 3)),
    table = c(rep("Table 3 - with notes, and year as a column heading", 3),
              rep("Table 1 - simple", 2),
              rep("Table 2 - with 2 descriptor columns", 2)),
    title = "Tidy data example: England, 2019-20 to 2021-22 (£ millions)",
    units = "million",
    # supplier = "testthat",
    source = "testdata",
    dataset = as.character(NA),
    year_type = "financial",
    year_notes = NA,
    fy_start = c(rep("2021", 3),"2020", rep("2021", 3))
  )

  result <- suppressWarnings(
    suppressMessages(tidy_sheet(arg_values, to_csv = FALSE))
  ) %>%
    select(-supplier)

  expect_equal(result, expected)
})


test_that("Example 4 is processed as expected", {

  settings <- "{
  header_identifier: (?i)group,
  metadata_cells_to_remove_patterns: (?i)see front page,
  populated_rows_to_check_for_metadata_to_remove: 15,
  columns_to_create: description_1, description_2,
  hidden_character_strings_to_remove: true,
  columns_to_rename_patterns: (?i)^group,
  columns_to_rename_names: description_3,
  name_for_total_column: description_4,
  col_with_totals_pattern: (?i)group,
  total_column_fill_dir: up,
  single_year_of_data: true
  }"

  arg_values <- c("--args", filepath, "example 4", NA, settings, "1")

  expected <- tibble (
    sheet = "example 4",
    address = c("B11", "C11", "D11", "F11", "B12", "C12", "D12", "F12",
                "B13", "C13", "D13", "F13", "B14", "C14", "D14", "F14",
                "B15", "C15", "D15", "F15", "B16", "C16", "D16", "F16", "P18"),
    value = c(10, 0, 10, 40, 20, NA, 20, 50, 30, 1, 31, 60, 60, 1, 61, 150, 40,
              2, 42, 10, 40, 2, 42, 10, NA),
    non_numeric_value = c(rep(NA, 5), "s", rep(NA, 18), "ivhifs"),
    description_3 = c(rep(
      c("set1", "set2", "set3", "Total group A", "set 1", "Total group B"),
      each = 4), NA),
    description_1 = c(rep(c(rep("Measure 1", 3), "Measure 2"), 6), "Measure 2"),
    description_2 = c(rep(c("part a", "part b", "total", NA), 6), NA),
    description_4 = c(rep("group A", 16), rep("group B", 8), NA),
    title = "Example 1: metadata in title (2023-24, £ thousand, provisional data), multiple header rows; hidden text; totals information needs to be associated with descriptors above them",
    units = "thousand",
    vintage = "provisional",
    # supplier = "testthat",
    source = "testdata",
    dataset = as.character(NA),
    year = "2023-24",
    year_type = "financial",
    year_notes = NA,
    fy_start = "2023"
  )

  result <- suppressWarnings(
    suppressMessages(tidy_sheet(arg_values, to_csv = FALSE))
  ) %>%
    select(-supplier)

  expect_equal(result, expected)
})


test_that("Example 5 is processed as expected", {

  # could also use vintage_and_year for col_patterns_with_values_to_drop, this just shows
  # something a bit different that requires exclude_from_reserved_word_check to
  # also b set. Note that the cell style needs to have been set to percentage
  # (not just cell format).
  settings <- "{
  header_identifier: (?i)group,
  columns_to_create: vintage_and_year,
  col_patterns_with_values_to_drop: style_format,
  value_patterns_to_drop: (?i)per.*cent,
  exclude_from_reserved_word_check: col_patterns_with_values_to_drop,
  left_cols_to_remove_patterns: (?i)comparable,
  vintage_with_year_column: vintage_and_year,
  year_col_pattern: ^year$,
  columns_to_rename_patterns: (?i)^group,
  columns_to_rename_names: description_1
  }"

  arg_values <- c("--args", filepath, "example 5", NA, settings, "1")

  expected <- tibble (
    sheet = "example 5",
    address = c("C6", "D6", "C7", "D7", "C8", "D8", "C9", "D9", "C10", "D10"),
    value = c(8662.589, 8621.354, 2766.08, 3044.197, 2752.084, 2922.173,
              4.793, 6.203, 14185.546, 14593.927),
    non_numeric_value = as.character(NA),
    description_1 = rep(c("A", "B", "C", "D", "Total"), each = 2),
    vintage_and_year = rep(
      c("2022-23 Provisional outturn", "2023-24 Budget Estimate"),
      times = 5),
    year = rep(c("2022-23", "2023-24"), times = 5),
    vintage = rep(c("provisional", "budget"), times = 5),
    title = "Example 5: Unwanted proportions; vintage in the headers; and an unwanted left block column",
    units = as.character(NA),
    # supplier = "testthat",
    source = "testdata",
    dataset = as.character(NA),
    year_type = "financial",
    year_notes = NA,
    fy_start = rep(c("2022", "2023"), times = 5)
  )

  result <- suppressWarnings(
    suppressMessages(tidy_sheet(arg_values, to_csv = FALSE))
  ) %>%
    select(-supplier)

  expect_equal(result, expected)
})


test_that("Example 6 is processed as expected", {

  settings <- "{
  header_identifier: (?i)count,
  table_split_dirs: row,
  tables_to_split: 1,
  table_split_patterns: (?i)table\\s2
  tables_to_process: 2,
  table_header_identifier: (?i)group,
  table_title_column: table_title,
  subtable_title_column: taxa,
  subtable_title_patterns: (?i)group$,
  subtitle_offset: -1,
  subtitle_horizontal_index: 1,
  columns_to_create: year,
  col_with_row_headers_pattern: (?i)group\\s*$,
  name_for_group_row_column: description_1,
  name_for_nested_row_column: description_2,
  row_header_fill_dir: down,
  group_row_na_identifier: all,
  col_patterns_to_drop_NA_rows: ^value$,
  }"

  arg_values <- c("--args", filepath, "example 6", NA, settings, "1")

  expected <- tibble (
    sheet = "example 6",
    address = c("B9", "C9", "B11", "C11", "B15", "C15", "B16", "C16"),
    value = c(100, 110, 20, 22, 50, 54, 100, 104),
    non_numeric_value = as.character(NA),
    description_2 = c(rep("a", 6), "b", "b"),
    year = rep(c("2022-23", "2023-24"), times = 4),
    taxa = rep(c("butterflies", "birds"), each = 4),
    description_1 = c(rep(c("group 1", "group 2"), each = 2), rep("group 1", 4)),
    title = "Example 6: Using subtables",
    units = "count",
    vintage = as.character(NA),
    # supplier = "testthat",
    source = "testdata",
    dataset = as.character(NA),
    year_type = "financial",
    year_notes = NA,
    fy_start = rep(c("2022", "2023"), times = 4)
  )

  result <- suppressWarnings(
    suppressMessages(tidy_sheet(arg_values, to_csv = FALSE))
  ) %>%
    select(-supplier)

  expect_equal(result, expected)
})


test_that("Example 7 is processed as expected", {

  settings <- "{
  header_identifier: (?i)group,
  header_identifier_instance: 2,
  columns_to_create: description_1, description_2, code,
  left_headers: year_and_quarter, month,
  combine_start_row_identifier: (?i)desc,
  combine_end_row_identifier: item\\s1,
  fill_columns_column_names: year_and_quarter,
  fill_columns_fill_dirs: down,
  quarter_from_col_pattern: year_and_quarter,
  quarter_col_name: quarter,
  q1_is_jan_to_mar: false,
  calendar_year_to_fy_start: true,
  year_from_pattern: year_and_quarter,
  month_col_pattern: month,
  quarter_col_pattern: ^quarter$,
  col_patterns_to_drop_NA_rows: ^numeric$,
  }"

  arg_values <- c("--args", filepath, "example 7", NA, settings, "1")

  expected <- tibble (
    sheet = "example 7",
    address = c("C15", "D15", "F15", "C16", "D16", "F16", "C17", "D17", "F17",
                "C18", "D18", "F18", "C19", "D19", "F19", "C20", "D20", "F20",
                "C21", "D21", "F21", "C22", "D22", "F22", "C23", "D23", "F23",
                "C24", "D24", "F24", "C25", "D25", "F25", "C26", "D26", "F26",
                "C29", "D29", "F29", "C30", "D30", "F30", "C31", "D31", "F31",
                "C32", "D32", "F32"),
    value = c(1, 2, 0, 2, 2, 0, 3, 1, 0, 4, 2, 0, 5, 2, 0, 6, 1, 0, 7, 2, 0, 8,
              2, 0, 9, 1, 0, 10, 2, 100, 11, 2, 0, 12, 1, 0, 6, 5, 0, 15, 5, 0,
              24, 5, 0, 33, 5, 100),
    non_numeric_value = as.character(NA),
    year_and_quarter =
      c(rep("2000", 27), rep("2001", 9),
        rep(c("2000q2", "2000q3", "2001q4", "2001q1"), each = 3)),
    month = c(rep(c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov",
                  "Dec", "Jan", "Feb", "Mar"), each = 3), rep(NA, 12)),
    description_1 = "GROUP A",
    description_2 =
      rep(c("Desc of first item (item 1)", "Another desc (item 2)", "Item 3"),
          times = 16),
    code = rep(c("ABCD", "EFGH", "CDID"), 16),
    title = "Example 7: Artificially split column names and all sorts of dates",
    units =  as.character(NA),
    vintage = as.character(NA),
    # supplier = "testthat",
    source = "testdata",
    dataset = as.character(NA),
    quarter = c(rep(NA, 36), rep(c("2", "3", "4", "1"), each = 3)),
    year = c(rep("2000", 27), rep("2001", 9), rep("2000", 6), rep("2001", 6)),
    fy_start = c(rep("2000", 45), rep("2001", 3)),
    year_type = "calendar",
    year_notes = NA,
  )

  result <- suppressWarnings(
    suppressMessages(tidy_sheet(arg_values, to_csv = FALSE))
  ) %>%
    select(-supplier)

  expect_equal(result, expected)
})


test_that("Example 8 is processed as expected", {

  settings <- "{
  header_identifier: (?i)measure,
  columns_to_create: description_1,
  left_headers: vintage_and_year,
  tolerance: 0.3,
  vintage_with_year_column: vintage_and_year,
  year_col_pattern: ^year$,
  replace_string_col: vintage,
  replace_string_from_col_patterns: actual,
  replace_string_to: final
  }"

  arg_values <- c("--args", filepath, "example 8", NA, settings, "1")

  expected <- tibble (
    sheet = "example 8",
    address = c("B7", "C7", "D7", "B8", "C8", "D8", "B9", "C9", "D9"),
    value = c(rep(NA, 4), 3, 4, 1, 4, 5),
    non_numeric_value = c(rep("s", 4), rep(NA, 5)),
    vintage_and_year = rep(
      c("forecast data 2024-25", "provisional data 2023-24", "actual 2022-23"),
      each = 3),
    description_1 = rep(c("Measure 1", "Measure 2", "Measure 3"), times = 3),
    year = rep(c("2024-25", "2023-24", "2022-23"), each = 3),
    vintage = rep(c("forecast", "provisional", "final"), each = 3),
    title = "Example 8: Lots of suppressed (non numeric) values; vintage in the same column as year",
    units = as.character(NA),
    # supplier = "testthat",
    source = "testdata",
    dataset = as.character(NA),
    year_type = "financial",
    year_notes = NA,
    fy_start =  rep(c("2024", "2023", "2022"), each = 3)
  )

  result <- suppressWarnings(
    suppressMessages(tidy_sheet(arg_values, to_csv = FALSE))
  ) %>%
    select(-supplier)

  expect_equal(result, expected)
})


test_that("Example 9 is processed as expected", {

  settings <- "{
  header_identifier: (?i)description,
  header_identifier_instance: 2,
  columns_to_create: description_1a, description_1b,
  left_headers: country, region,
  descriptors_to_standardise_year_in: description_1b,
  columns_to_combine_patterns: description_1a, description_1b,
  columns_to_combine_combined_names: description_1,
  rename_duplicate_in_col: description_1,
  rename_duplicate_pattern: A,
  rename_duplicate_index: 1, 2,
  rename_duplicate_prefix: first, second,
  rename_duplicate_expected_freq: 2,
  col_pattern_with_blanks_to_replace: (?i)region,
  col_pattern_to_replace_blanks_with: (?i)country,
  single_year_of_data: true
  }"

  arg_values <- c("--args", filepath, "example 9", NA, settings, "1")

  expected_description_note <- "description_1 was 'description A - measure 1 previous financial year' in the raw data. It was given a prefix because there was more than one instance in the raw data with that name."
  expected <- tibble (
    sheet = "example 9",
    address = c("C8", "D8", "E8", "C9", "D9", "E9"),
    value = c(1, 3, 5, 2, 4, 6),
    non_numeric_value = as.character(NA),
    country = "England",
    region = rep(c("SW", "England"), each = 3),
    description_1 = rep(
      c("first description A - measure 1 previous financial year",
        "description A - measure 1 current financial year",
        "second description A - measure 1 previous financial year"),
      times = 2),
    title = "Example 9: year is mentioned in headers, where current year is 2023-24; filling blanks in column with values from another",
    units = as.character(NA),
    vintage = as.character(NA),
    # supplier = "testthat",
    source = "testdata",
    dataset = as.character(NA),
    year = "2023-24",
    year_type = "financial",
    year_notes = NA,
    fy_start =  "2023",
    rename_note = rep(
      c(expected_description_note, NA, expected_description_note),
      times = 2)
  )

  result <- suppressWarnings(
    suppressMessages(tidy_sheet(arg_values, to_csv = FALSE))
  ) %>%
    select(-supplier)

  expect_equal(result, expected)
})

# settings <- "{header_identifier: ^A$, columns_to_create: description_1,
# columns_to_remove_patterns: code, single_year_of_data: true"
# arg_values <- c("--args", filepath, "Sheet1", NA, settings, "1")
# result <- tidy_sheet(arg_values, to_csv = FALSE)

