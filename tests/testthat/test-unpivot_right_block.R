test_that("unpivot_right_block with a single row of headers", {

  dat <- data.frame(
    address = c("B1", "C1", "B2", "C2"),
    row = c(1, 1, 2, 2),
    col = c(2, 3, 2, 3),
    data_type = c(rep("character", 2), rep("numeric", 2)),
    numeric = c(rep(NA, 2), 100, 22),
    character = c("primary", "secondary", NA, NA))

  expected <- dat %>%
    filter(row != 1) %>%
    mutate(description_1 = c("primary", "secondary"))

  result <- suppressMessages(unpivot_right_block(dat, "description_1"))

  expect_equal(result, tibble(expected))
})


test_that("unpivot_right_block with multiple rows of headers", {

  dat <- data.frame(
    address = c("B1", "C1", "B2", "C2", "B3", "C3", "B4", "C4"),
    row = rep(1:4, each = 2),
    col = rep(2:3, times = 4),
    data_type = c(rep(c("character", "blank"), times = 2),
                  rep(c("character", "numeric"), each = 2)),
    numeric = c(rep(NA, 2), 100, 22),
    character = c("Expenditure", NA, "Education", NA, "primary", "secondary", NA, NA))

  expected <- dat %>%
    filter(row == 4) %>%
    mutate(col_1 = "Expenditure",
           col_2 = "Education",
           col_3 = c("primary", "secondary"))

  result <- suppressMessages(
    unpivot_right_block(dat, c("col_1", "col_2", "col_3"))
  )

  expect_equal(result, tibble(expected))

})
