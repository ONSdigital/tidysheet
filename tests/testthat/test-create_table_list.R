context("create_table_list")
test_that("create_table_list correctly partitions data", {
  header_rows_combined <- data.frame(
    "row" = c(1, 1, 1, 2, 2, 2),
    "col" = c(1, 2, 3, 1, 2, 3),
    "address" = c("A1", "B1", "C1", "A2", "B2", "C2"),
    "character"= c(NA,NA,NA,"Education Services",NA,NA))
  table_start_cols <- 1
  table_count <- 1

  result <- create_table_list(header_rows_combined, table_start_cols, table_count)

  result <- create_table_list(header_rows_combined, table_start_cols, table_count)

  expect_type(result, "list")
  expect_length(result, table_count)
  expect_true(all(c("row", "col", "address", "character") %in% names(result[[1]])))
  expect_true(all(result[[1]]$col >= table_start_cols[1]))
})
