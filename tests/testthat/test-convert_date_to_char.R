test_that("convert_date_to_char gives expected result when there are no NAs in date", {

  dat <- data.frame(
    data_type = "date",
    date = c("01/04/1999", "01/04/2013",  "01/04/2015"),
    character = NA
  )
  expected <-  data.frame(
    data_type = "character",
    date = NA,
    character = c("01/04/1999", "01/04/2013",  "01/04/2015")
  )

  expect_equal(convert_date_to_char(dat), expected)

})


test_that("convert_date_to_char gives expected result when there are only NAs in date", {

  dat <- data.frame(
    data_type = "character",
    date = NA,
    character = c("2013", "2018", "2025")
  )

  expect_equal(convert_date_to_char(dat), dat)
})


test_that("convert_date_to_char gives expected result when there is a mix of NAs and non NAs in date", {

  dat <- data.frame(
    data_type = c("character", rep("date", 3), "character"),
    date = c(NA, "01/04/2015", NA, "29/04/2035", NA),
    character = c("2021", NA, NA, NA, "2022")
  )

  expected <- data.frame(
    data_type = "character",
    date = NA,
    character = c("2021", "01/04/2015", NA, "29/04/2035", "2022")
  )

  expect_equal(convert_date_to_char(dat), expected)

})
