sheet_dat <- data.frame(
  character = c(
    "All data for 2022-23 to 2026-27",
    "£ million unless otherwise stated",
    "England"
  ),
  address = c("A1", "A2", "A4"),
  row = c(1, 2, 4),
  col = 1
)

table_dat <- data.frame(
  character = c("Data for 2024-25", "£ thousand"),
  address = "A40",
  row = 40,
  col = 1
)


test_that("get_metadata returns a named list", {

  expected <- list(
    "units" = "million", "dropdown" = NA,
    "title" = "All data for 2022-23 to 2026-27",
    "year" = c("2022-23", "2026-27"), "vintage" = NA
  )
  result <- suppressMessages(get_metadata(sheet_dat))

  expect_equal(result, expected)
})


test_that("get_metdata raises an error if dat does not have the required columns", {

  row_missing <- select(sheet_dat, -row)
  col_missing <- select(sheet_dat, -col)
  char_missing <- select(sheet_dat, -character)
  all_missing <- select(sheet_dat, -c(row, col, character))

  message <- "Data must be imported using xlsx_cells and contain"

  expect_error(suppressMessages(get_metadata(row_missing)), message)
  expect_error(suppressMessages(get_metadata(col_missing)), message)
  expect_error(suppressMessages(get_metadata(char_missing)), message)
  expect_error(suppressMessages(get_metadata(all_missing)), message)
})


test_that("get_metdata works as expected for both sheet and table metadata", {

  expected_sheet <- list(
    "units" = "million", "dropdown" = "England",
    "title" = "All data for 2022-23 to 2026-27",
    "year" = c("2022-23", "2026-27"), "vintage" = NA
  )
  expected_table <- list(
    "units" = "thousand", "dropdown" = as.character(),
    "title" = "Data for 2024-25",
    "year" = "2024-25", "vintage" = "final"
  )

  sheet_result <- suppressMessages(
    get_metadata(
      sheet_dat, dropdown_pattern = "(?i)Eng", single_vintage = "final"
    )
  )
  table_result <- suppressMessages(
    get_metadata(
      sheet_dat, "(?i)Eng", "final", table_dat = table_dat,
      sheet_title = sheet_result[['title']],
      sheet_units = sheet_result[['units']]
    )
  )

  expect_equal(sheet_result, expected_sheet)
  expect_equal(table_result, expected_table)

})


test_that("If year is in the title get_metdata returns that year not years from
info below that", {

  dat <- data.frame(
    character = c(
      "All data for 2022-23 to 2026-27",
      "2021 standards"
    ),
    address = c("A1", "A2"),
    row = c(1, 2),
    col = 1
  )

  result <- suppressMessages(get_metadata(dat))

  expect_equal(result[["year"]], c("2022-23", "2026-27"))

})


test_that("get_metdata only looks at year below the title if it is not in the title", {

  dat <- data.frame(
    character = c(
      "All data",
      "2022-23 to 2026-27"
    ),
    address = c("A1", "A2"),
    row = c(1, 2),
    col = 1
  )

  result <- suppressMessages(get_metadata(dat))

  expect_equal(result[["year"]], c("2022-23", "2026-27"))

})


test_that("get_metdata only returns vintage if table_dat is supplied", {

  result <- suppressMessages(get_metadata(sheet_dat))
  expect_equal(result[["year"]], c("2022-23", "2026-27"))

})


test_that("get_metdata returns the sheet units if the table units do not exist", {

  no_units_in_table <- mutate(table_dat, character = c("Data for 2024-25", NA))

  result <- suppressMessages(
    get_metadata(
      sheet_dat, "(?i)Eng", single_vintage = "final",
      table_dat = no_units_in_table,
      sheet_title = NA, sheet_units = "million"
      ))

  expect_equal(result[["units"]], "million")

})


test_that("get_metdata returns the table units if they exist", {

  result <- suppressMessages(
    get_metadata(
      sheet_dat, "(?i)Eng", single_vintage = "final",
      table_dat = table_dat,
      sheet_title = NA, sheet_units = "million"
    ))

  expect_equal(result[["units"]], "thousand")

})
