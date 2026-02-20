test_that("extract_all_years accepts numeric input for calendar years", {
  expect_equal(extract_all_years(2020), "2020")
  expect_equal(extract_all_years(202), NA)
})


test_that("extract_all_years accepts character input for calendar and financial years", {
  expect_equal(extract_all_years("2020"), "2020")
  expect_equal(extract_all_years("2020-21"), "2020-21")
  expect_equal(extract_all_years("2020-2021"), "2020-2021")
})


test_that("extract_all_years accepts df, list, vector, and matrix input", {
  expect_equal(extract_all_years(data.frame("year" = "2020")), "2020")
  expect_equal(extract_all_years(as.list("2020")), "2020")
  expect_equal(extract_all_years(c("2020", "no")), "2020")
  expect_equal(extract_all_years(as.matrix("2020")), "2020")

  expect_equal(extract_all_years(data.frame("year" = "2020-21")), "2020-21")
  expect_equal(extract_all_years(as.list("2020-21")), "2020-21")
  expect_equal(extract_all_years(c("2020-21", "no")), "2020-21")
  expect_equal(extract_all_years(as.matrix("2020-21")), "2020-21")
})


test_that("extract_all_years deals with yyyy - yyyy", {
  expect_equal(extract_all_years("2098 - 2099"), "2098 - 2099")
  expect_equal(extract_all_years("Year: 2020 - 2021"), "2020 - 2021")
  expect_equal(extract_all_years("2000 - 2001 is the year"), "2000 - 2001")
})


test_that("extract_all_years doesnt return a year followed by a hyphen unless it is YYYY-YY or YYYY-YYYY", {
  expect_equal(extract_all_years("yes: 2098-99"), c("2098-99"))
  expect_equal(extract_all_years("yes: 2012-2013"), c("2012-2013"))
  expect_equal(extract_all_years("no: 2098-2099-4567"), NA)
})


test_that("extract_all_years doesn't return years if non-year number entered", {
  expect_equal(extract_all_years("19001"), NA)
  expect_equal(extract_all_years("Year: 19501"), NA)
  expect_equal(extract_all_years("19501 is the year"), NA)
})


test_that("extract_all_years returns years between 1900 and 2099", {
  expect_equal(extract_all_years("1900"), "1900")
  expect_equal(extract_all_years("2099"), "2099")
  expect_equal(extract_all_years("Year: 1950"), "1950")
  expect_equal(extract_all_years("1950 is the year"), "1950")

  expect_equal(extract_all_years("1900-01"), "1900-01")
  expect_equal(extract_all_years("2099-00"), "2099-00")
  expect_equal(extract_all_years("Year: 1950-51"), "1950-51")
  expect_equal(extract_all_years("1950-51 is the year"), "1950-51")
})


test_that("extract_all_years returns NA when no 19th or 20th C years given", {
  expect_equal(extract_all_years("1899"), NA)
  expect_equal(extract_all_years("2100"), NA)

  expect_equal(extract_all_years("1899-00"), NA)
  expect_equal(extract_all_years("2100-01"), NA)

  expect_equal(extract_all_years("0000"), NA)
  expect_equal(extract_all_years("year: 200"), NA)

  expect_equal(extract_all_years("this-that"), NA)
  expect_equal(extract_all_years("123-45"), NA)
  expect_equal(extract_all_years("0000-00"), NA)
})


test_that("extract_all_years returns NA when year is preceded by letter", {
  expect_equal(extract_all_years("A1990"), NA)
  expect_equal(extract_all_years("a1990"), NA)
  expect_equal(extract_all_years("q1990"), NA)
})


test_that("extract_all_years returns NA when year is followed by letter other than q or Q", {
  expect_equal(extract_all_years("1990A"), NA)
  expect_equal(extract_all_years("1990a"), NA)
  expect_equal(extract_all_years("1990p"), NA)
  expect_equal(extract_all_years("1990r"), NA)
  expect_equal(extract_all_years("1990q"), "1990")
  expect_equal(extract_all_years("1990Q"), "1990")
})


test_that("extract_all_years returns multiple years when multiple given", {
  expect_equal(extract_all_years("1950 and 2050"), c("1950", "2050"))
  expect_equal(extract_all_years(c("1999", "2020")), c("1999", "2020"))

  expect_equal(extract_all_years("1950-51 and 2050-51"), c("1950-51", "2050-51"))
  expect_equal(extract_all_years(c("Year: 1950-51","Year: 2050-51")), c("1950-51", "2050-51"))
  expect_equal(extract_all_years(c("1950-51", "2050-51")), c("1950-51", "2050-51"))
})


test_that("extract_all_years returns unique values", {
  expect_equal(extract_all_years(c("1999 and 2000", "1999")), c("1999", "2000"))
  expect_equal(extract_all_years(c("2020", "2020", "2020")), "2020")

  expect_equal(extract_all_years(c("1999-00 and 2000-01", "1999-00")), c("1999-00", "2000-01"))
  expect_equal(extract_all_years(c("2020-21", "2020-21", "2020-21")), "2020-21")
})


test_that("extract_all_years returns a vector", {
  expect_equal(is.vector(extract_all_years("1950")), TRUE)
  expect_equal(is.vector(extract_all_years(1950)), TRUE)
  expect_equal(is.vector(extract_all_years(c("1950", "2020"))), TRUE)
  expect_equal(is.vector(extract_all_years(c("1950 and 1951", "2020"))), TRUE)

  expect_equal(is.vector(extract_all_years("1950-51")), TRUE)
  expect_equal(is.vector(extract_all_years(c("1950-51", "2020-21"))), TRUE)
  expect_equal(is.vector(extract_all_years(c("1950-51 and 1951-52", "2020-21"))), TRUE)
})

test_that("extract_all_years returns expected datatype", {
  expect_equal(typeof(extract_all_years("1950")), "character")
  expect_equal(typeof(extract_all_years(1950)), "character")
  expect_equal(typeof(extract_all_years(c("1950", "2020"))), "character")

  expect_equal(typeof(extract_all_years("1950-51")), "character")
  expect_equal(typeof(extract_all_years(c("1950-51", "2020-21"))), "character")
})

