test_that("remove_remnants_from_behead removes rows above the first data row", {

  dat <- data.frame(character = c("header_1", "a"),
                    row = 1:2,
                    description_1 = c(NA, "a"))
  result <- remove_remnants_from_behead(dat, 2)
  expected <- filter(dat, row != 1)
  expect_equal(result, expected)

})

test_that("remove_remnants_from_behead errors if there are no data beyond first_data_row", {

  dat <- data.frame(character = c("header_1", "a"),
                    row = 1:2,
                    description_1 = c(NA, "a"))
  expect_error(remove_remnants_from_behead(dat, 3))

})
