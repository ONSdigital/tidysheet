context("process_year_column")

test_that("process_year_column function works as expected", {
  # Sample data for testing---------------
  year_pattern <- make_year_patterns()$financial
  year_for_layout <- "2023"
  
  one_year_col <- data.frame(
    year = c("2021", "2022-23", "2022-23 to 2024-25", "2025/26", "2026 to 27"),
    another_col = c("abc", "dfe", "ghf", "ijk", "lmn")
  )
  capitalisation <- data.frame(
    Year = c("2022-23"),
    another_col = c("abc")
  )
  one_non_year <- data.frame(
    one_year_olds = c("Bill", "Bob", "2023-24"),
    another_col = c("abc", "dfe", "ghi")
  )  
  one_possible_year <- data.frame(
    this_has_year_vals = c("2022-23", "2022-23"),
    another_col = c("abc", "dfe")
  )  
  two_possible_years <- data.frame(
    previous_year = c("2021-22", "2021-22"),
    year_vals = c("2022-23", "2022-23"),
    another_col = c("abc", "dfe")
  )  
  year_plus_possible <- data.frame(
    year = c("2021", "2022-23", "not a year"),
    another_year_col = c("2023", "2024", "2025"),
    another_col = c("abc", "dfe", "ghf")
  )  
  no_year_cols <- data.frame(
    another_col = c("abc", "dfe", "ghf")
  )  
  
  # Expected results --------------------
  expected_one_year_col <- data.frame(
    year = c("2021", "2022-23", "2022-23 to 2024-25", "2025-26", "2026-27"),
    another_col = c("abc", "dfe", "ghf", "ijk", "lmn")
  )
  expected_capitalisation <- data.frame(
    year = c("2022-23"),
    another_col = c("abc")
  )
  expected_one_possible_year <- data.frame(
    year = c("2022-23", "2022-23"),
    another_col = c("abc", "dfe")
  )
  expected_one_non_year <- one_non_year %>% 
    mutate(year = year_for_layout)
  expected_two_possible_years <- two_possible_years %>% 
    mutate(year = year_for_layout)
  expected_no_year_cols <- no_year_cols %>% 
    mutate(year = year_for_layout)
  
  # Actual results and tests --------------------
  
  # Single 'year' column, function keeps the columns but changes the formatting
  output_one_year_col <- suppressWarnings(
    process_year_column(one_year_col, year_pattern, year_for_layout)
  )
  
  # Single column with year in column name which gets renamed to 'year'
  output_one_possible_year <- suppressWarnings(
    process_year_column(one_possible_year, year_pattern, year_for_layout)
  )
  expect_equal(output_one_possible_year, expected_one_possible_year)
  expect_warning(
    process_year_column(one_possible_year, year_pattern, year_for_layout),
    "Year values found in 'this_has_year_vals' so it has been renamed 'year'")
  
  # Single column with year in the name but not in the values
  output_one_non_year <- suppressWarnings(
    process_year_column(one_non_year, year_pattern, year_for_layout)
  )
  expect_equal(output_one_non_year, expected_one_non_year)
  expect_warning(
    process_year_column(one_non_year, year_pattern, year_for_layout),
    "No 'year' column found in data. Using Year Method specified in earlier warning message. Please check that the year column contains the correct values in the pre-processed data")
  
  # Two columns with year in column name - no renaming: 'year' taken from elsewhere
  output_two_possible_years <- suppressWarnings(
    process_year_column(two_possible_years, year_pattern, year_for_layout)
  )
  expect_equal(output_two_possible_years, expected_two_possible_years)
  expect_warning(
    process_year_column(two_possible_years, year_pattern, year_for_layout),
    "Multiple columns with 'year' in the name have been found, but none are called just 'year': None have been renamed 'year' as it is not known which to use. Using the Year Method specified in earlier warning message. Please check that the year column contains the correct values in the pre-processed data")
  
  # 'year' column, plus a column with year in the name - nothing changes
  output_year_plus_possible <- process_year_column(
    year_plus_possible, year_pattern, year_for_layout
  )
  expect_equal(output_year_plus_possible, output_year_plus_possible)
  
  # No columns with year in it or 'year' column
  output_no_year_cols <- suppressWarnings(
    process_year_column(no_year_cols, year_pattern, year_for_layout)
  )
  expect_equal(output_no_year_cols, expected_no_year_cols)
  expect_warning(
    process_year_column(no_year_cols, year_pattern, year_for_layout),
    "No 'year' column found in data. Using Year Method specified in earlier warning message. Please check that the year column contains the correct values in the pre-processed data")
  
})