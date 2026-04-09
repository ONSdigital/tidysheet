all <- data.frame(
  address = c("A1", "A2", "A3", "A4"),
  row = 1:4,
  col = 1,
  character = c("Some data", NA, "value", NA),
  numeric = c(NA, NA, NA, 10)
)

test_that("split_data_from_metadata returns a list of dataframes", {

  dat <- filter(all, row == 3:4)
  meta <- filter(all, row == 1)
  expected <- list("data" = dat, "metadata" = meta)

  result <- suppressMessages(split_data_from_metadata(all, "(?i)value", NA, NA))

  expect_equal(result, expected)
})


test_that("split_data_from_metadata raises an error if data are not from xlsx_cells", {

  row_missing <- select(all, -row)
  col_missing <- select(all, -col)
  addr_missing <- select(all, -address)
  all_missing <- select(all, -c(row, col, address))

  error <- "Data must be imported using xlsx_cells and contain"

  expect_error(
    suppressMessages(split_data_from_metadata(row_missing, "(?i)value")), error
    )
  expect_error(
    suppressMessages(split_data_from_metadata(col_missing, "(?i)value")), error
    )
  expect_error(
    suppressMessages(split_data_from_metadata(addr_missing, "(?i)value")), error
    )
  expect_error(
    suppressMessages(split_data_from_metadata(all_missing, "(?i)value")), error
    )
})
