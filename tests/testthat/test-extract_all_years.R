test_that("extract_all_years accepts numeric input for yyyy", {
  expect_equal(extract_all_years(2020), "2020")
  expect_equal(extract_all_years(202), NA)
  # haven't tested for numeric and financial because 2021-22 wouldn't be seen as 
  # a number by R but as a string. This function wouldn't be useful if you were
  # just typing year in e.g if you were to use extract_all_years(2020-21, "yyyy-yy")
  # it would return "1999" - the result of the sum. 
})

test_that("extract_all_years accepts character input", {
  expect_equal(extract_all_years("2020"), "2020")
  expect_equal(extract_all_years("2020-21"), "2020-21")
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

# this is a potential area of improvement - could add yyyy-yyyy as a pattern
test_that("extract_all_years returns list of strings that look like years if more than one year is present", {
  expect_equal(extract_all_years("2098-2099"), c("2098", "2099"))
  expect_equal(extract_all_years("Year: 1950-1951"), c("1950", "1951"))
  expect_equal(extract_all_years("1950-1951 is the year"), c("1950", "1951"))
})

test_that("extract_all_years doesn't returns years if non-year number entered", {
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

test_that("extract_all_years returns NA when year is preceded or followed by letter", {
  expect_equal(extract_all_years("1990A"), NA)
  expect_equal(extract_all_years("1990a"), NA)
  expect_equal(extract_all_years("A1990"), NA)
  expect_equal(extract_all_years("a1990"), NA)
})

test_that("extract_all_years returns multiple years when multiple given", {
  expect_equal(extract_all_years("1950 and 2050"), c("1950", "2050"))
  expect_equal(extract_all_years("Year: 1950-1951"), c("1950", "1951"))
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

