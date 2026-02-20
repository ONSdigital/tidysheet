test_that("deduplicate_data works as expected when there are no replicates", {

  dat <- tibble(
    row = c(1, 2, 3, 4),
    col = c("A", "B", "A", "B"),
    value = c(10, 20, 30, 40)
  )
  result <- suppressMessages(
    deduplicate_data(dat, c("row", "col"), c("row", "col"))
  )
  expect_equal(result, dat)

})


test_that("deduplicate_data works as expected when there are replicates and ignored columns", {

  #   A   B
  # 1 ed  1
  # 2 ed  1
  # 3 tr  2
  dat <- tibble(
     address = c("A1", "A2", "A3", "B1", "B2", "B3"),
     row = rep(c(1:3), times = 2),
     col = rep(c(1, 2), each = 3),
     description = c(NA, NA, NA, "ed", "ed", "tr"),
     value = c(NA, NA, NA, 1, 1, 2)
     )

  expected <- dat %>%
    filter(address %in% c("A1", "B1", "B3"))

  expect_warning(
    suppressMessages(
      result <- deduplicate_data(
        dat, c("address", "row", "col"), c("row", "col")
      )
    ),
    "3 row.*2, 3"
  )
  expect_equal(result, expected)

})


test_that("deduplicate_data works as expected when there are replicates and no ignored columns", {

  dat <- tibble(value = 1, id = c("A", "A", "B"))
  expected <- tibble(value = 1, id = c("A", "B"))

  expect_warning(
    suppressMessages(
      result <- deduplicate_data(dat, arrange_by = "id")
    ),
    "1.*found and removed"
  )
  expect_equal(result, expected)

})


test_that("deduplicate_data works as expected with an empty dataframe", {

  dat <- data.frame()
  expect_error(
    result <- deduplicate_data(dat),
    "Input data frame is empty"
  )

})

test_that("deduplicate_data works as expected with a single row in the dataframe", {

  dat <- tibble(
    row = 1,
    col = "A",
    value = 10
  )
  result <- suppressMessages(deduplicate_data(dat))
  expect_equal(result, dat)

})


test_that("deduplicate_data works as expected with all rows being replicates, and multiple different replicates", {

  dat <- tibble(
    id = c(1, 1, 2, 2),
    value = 10
  )

  expected <- tibble(
    id = c(1, 2),
    value = 10
  )

  expect_warning(
    suppressMessages(
      result <- deduplicate_data(dat, arrange_by = "id")
    ),
    "2.*found and removed"
  )
  expect_equal(result, expected)

})

