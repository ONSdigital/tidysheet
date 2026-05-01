test_that("join_left_and_right_unpivotted_data acts as expected", {
  left <- data.frame(
    row = c(1, 2),
    col = c(1, 2),
    character = c("Date", NA),
    numeric = c(NA, 100),
    Date = c(NA, "2020 Q1")
  )
  right <- data.frame(
    row = 2,
    col = 2,
    character = NA,
    numeric = 100,
    description_1 = "Education"
  )
  column_names <- c("row", "col", "character", "numeric")
  expected <- left %>%
    filter(row == 2) %>%
    mutate(description_1 = "Education")

  result <- suppressMessages(
    join_left_and_right_unpivotted_data(left, right, column_names, 1)
  )
  expect_equal(result, expected)
})
