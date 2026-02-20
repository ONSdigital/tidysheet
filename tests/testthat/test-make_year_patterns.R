test_that("make_year_patterns()['financial'] matches long form and short form financial years", {
  expect_equal(
    str_extract_all("2023-24", make_year_patterns()['financial']),
    list("2023-24")
  )
  expect_equal(
    str_extract_all("2023-2024", make_year_patterns()['financial']),
    list("2023-2024")
  )
})


test_that("make_year_patterns()['long_form_financial'] doesn't pick up e.g. 2019/20 - special case as 1920 is an allaowable year", {
  expected <- list(as.character(c()))

  expect_equal(
    str_extract_all("2019/20", make_year_patterns()['long_form_financial']),
    expected
  )

  expect_equal(
    str_extract_all("2018/19", make_year_patterns()['long_form_financial']),
    expected
  )
})


test_that("make_year_patterns()['long_form_financial'] does pick up e.g. 2019-2020", {
  expected <- list(as.character(c()))

  expect_equal(
    str_extract_all("2019-2020", make_year_patterns()['long_form_financial']),
    list("2019-2020")
  )
})


test_that("make_year_patterns()['financial'] matches 20th and 21st centuries", {
  expect_equal(
    str_extract_all("1999-2000", make_year_patterns()['financial']),
    list("1999-2000")
  )
  expect_equal(
    str_extract_all("1999-00", make_year_patterns()['financial']),
    list("1999-00")
  )
  expect_equal(
    str_extract_all("2099-00", make_year_patterns()['financial']),
    list("2099-00")
  )
  expect_equal(
    str_extract_all("2098-2099", make_year_patterns()['financial']),
    list("2098-2099")
  )
  expect_equal(
    str_extract_all("1900-01", make_year_patterns()['financial']),
    list("1900-01")
  )
  expect_equal(
    str_extract_all("202324", make_year_patterns()['financial']),
    list("202324")
  )
  expect_equal(
    str_extract_all("20232024", make_year_patterns()['financial']),
    list("20232024")
  )
})


test_that("make_year_patterns()['financial'] matches financial years separated with a dash", {

  expected <- list("2023-24")

  expect_equal(
    str_extract_all("2023-24", make_year_patterns()['financial']), expected
    )
  expect_equal(
    str_extract_all("date:2023-24", make_year_patterns()['financial']), expected
    )
  expect_equal(
    str_extract_all("-2023-24", make_year_patterns()['financial']), expected
  )
  expect_equal(
    str_extract_all("_2023-24", make_year_patterns()['financial']), expected
  )
  expect_equal(
    str_extract_all("2023-24_", make_year_patterns()['financial']), expected
  )
  expect_equal(
    str_extract_all("2023-24.", make_year_patterns()['financial']), expected
  )

})


test_that("make_year_patterns()['financial'] matches financial years separated with an underscore", {

  expected <- list("2023_24")

  expect_equal(
    str_extract_all("2023_24", make_year_patterns()['financial']), expected
  )
  expect_equal(
    str_extract_all("date:2023_24", make_year_patterns()['financial']), expected
  )
  expect_equal(
    str_extract_all("-2023_24", make_year_patterns()['financial']), expected
  )
  expect_equal(
    str_extract_all("_2023_24", make_year_patterns()['financial']), expected
  )
  expect_equal(
    str_extract_all("2023_24_", make_year_patterns()['financial']), expected
  )
  expect_equal(
    str_extract_all("2023_24.", make_year_patterns()['financial']), expected
  )

  # with spaces
  expect_equal(
    str_extract_all("2023 _ 24", make_year_patterns()['financial']),
    list("2023 _ 24")
  )
  expect_equal(
    str_extract_all("2023   _ 24", make_year_patterns()['financial']),
    list("2023   _ 24")
  )

})


test_that("make_year_patterns()['financial'] matches financial years separated with 'to'", {

  expected <- list("2023to24")

  expect_equal(
    str_extract_all("2023to24", make_year_patterns()['financial']), expected
  )
  expect_equal(
    str_extract_all("date:2023to24", make_year_patterns()['financial']), expected
  )
  expect_equal(
    str_extract_all("-2023to24", make_year_patterns()['financial']), expected
  )
  expect_equal(
    str_extract_all("_2023to24", make_year_patterns()['financial']), expected
  )
  expect_equal(
    str_extract_all("2023to24_", make_year_patterns()['financial']), expected
  )
  expect_equal(
    str_extract_all("2023to24.", make_year_patterns()['financial']), expected
  )

  # with spaces
  expect_equal(
    str_extract_all("2023_to_24", make_year_patterns()['financial']),
    list("2023_to_24")
  )
  expect_equal(
    str_extract_all("2023 to 24", make_year_patterns()['financial']),
    list("2023 to 24")
  )

})


test_that("make_year_patterns()['financial'] does not match strings that do not contain financial years", {
  expected <- list(as.character(c()))

  expect_equal(
    str_extract_all("2023", make_year_patterns()['financial']), expected
  )
  expect_equal(
    str_extract_all("2023-24-25", make_year_patterns()['financial']), expected
  )
  expect_equal(
    str_extract_all("2000 and 2001", make_year_patterns()['financial']), expected
  )

})


test_that("make_year_patterns()['financial'] does not match anything outside of 20th and 21st centuries", {
  expected <- list(as.character(c()))
  expect_equal(
    str_extract_all("1899-1900", make_year_patterns()['financial']),
    expected
  )
  expect_equal(
    str_extract_all("2100-2111", make_year_patterns()['financial']),
    expected
  )

  expect_equal(
    str_extract_all("20231818", make_year_patterns()['financial']), expected
  )
  expect_equal(
    str_extract_all("181819", make_year_patterns()['financial']), expected
  )
})


test_that("make_year_patterns()['calendar'] matches 20th and 21st centuries", {
  expect_equal(
    str_extract_all("1900", make_year_patterns()['calendar']), list("1900")
  )
  expect_equal(
    str_extract_all("2099", make_year_patterns()['calendar']), list("2099")
  )
})


test_that("make_year_patterns()['calendar'] does not match centuries other than 20th and 21st", {
  expected <- list(as.character())
  expect_equal(
    str_extract_all("1899", make_year_patterns()['calendar']), expected
  )
  expect_equal(
    str_extract_all("2100", make_year_patterns()['calendar']), expected
  )
})


test_that("make_year_patterns()['calendar'] does not match numbers with anything other than 4 digits", {
  expected <- list(as.character())
  expect_equal(
    str_extract_all("18991", make_year_patterns()['calendar']), expected
  )
  expect_equal(
    str_extract_all("210", make_year_patterns()['calendar']), expected
  )
})


test_that("make_year_patterns()['calendar'] matches years preceded by a dash", {
  expect_equal(
    str_extract_all("-2020", make_year_patterns()['calendar']), list("2020")
  )
})


test_that("make_year_patterns()['calendar'] does not match matches years preceded by numbers then a dash", {
  expected <- list(as.character())

  expect_equal(
    str_extract_all("2019-2020", make_year_patterns()['calendar']), expected
  )
  expect_equal(
    str_extract_all("2019 - 2020", make_year_patterns()['calendar']), expected
  )
})


test_that("make_year_patterns()['calendar'] does not match years followed by a dash then 2-4 numbers", {
  expected <- list(as.character())

  expect_equal(
    str_extract_all("2020-21", make_year_patterns()['calendar']), expected
  )
  expect_equal(
    str_extract_all("2020 - 21", make_year_patterns()['calendar']), expected
  )
  expect_equal(
    str_extract_all("2019/20 or", make_year_patterns()['calendar']), expected
  )
  expect_equal(
    str_extract_all("2019-2020", make_year_patterns()['calendar']), expected
  )
  expect_equal(
    str_extract_all("2019/20", make_year_patterns()['calendar']), expected
  )

})


test_that("make_year_patterns()['calendar'] matches years followed by a dash then 1, or more than 4 numbers", {
  expected <- list(as.character())

  expect_equal(
    str_extract_all("2098-1", make_year_patterns()['calendar']), list("2098")
  )
  expect_equal(
    str_extract_all("2019 - 1", make_year_patterns()['calendar']), list("2019")
  )
  expect_equal(
    str_extract_all("2019-24578", make_year_patterns()['calendar']), list("2019")
  )
})


test_that("make_year_patterns()['calendar'] matches years followed by a period", {
  expect_equal(
    str_extract_all("2020.", make_year_patterns()['calendar']), list("2020")
  )
})


test_that("make_year_patterns()['calendar'] does not match financial years", {
  expected <- list(as.character())
  expect_equal(
    str_extract_all("2020-21", make_year_patterns()['calendar']), expected
  )
  expect_equal(
    str_extract_all("2020/21", make_year_patterns()['calendar']), expected
  )
  expect_equal(
    str_extract_all("2020_2021", make_year_patterns()['calendar']), expected
  )
  expect_equal(
    str_extract_all("2020to2021", make_year_patterns()['calendar']), expected
  )
  expect_equal(
    str_extract_all("2020 to 2021", make_year_patterns()['calendar']), expected
  )
  expect_equal(
    str_extract_all("2020-to-2021", make_year_patterns()['calendar']), expected
  )
})


test_that("make_year_patterns()['calendar'] matches a year followed by a quarter", {
  expect_equal(
    str_extract_all("2020q1", make_year_patterns()['calendar']), list("2020")
  )
  expect_equal(
    str_extract_all("2020Q2", make_year_patterns()['calendar']), list("2020")
  )
  expect_equal(
    str_extract_all("2020_Q1", make_year_patterns()['calendar']), list("2020")
  )
  expect_equal(
    str_extract_all("2020-Q1", make_year_patterns()['calendar']), list("2020")
  )
})


test_that("make_year_patterns()['calendar'] does not match a year follwed by a letter other than q", {
  expected <- list(as.character())
  expect_equal(
    str_extract_all("2020a", make_year_patterns()['calendar']), expected
  )
  expect_equal(
    str_extract_all("2020p", make_year_patterns()['calendar']), expected
  )
  expect_equal(
    str_extract_all("2020r", make_year_patterns()['calendar']), expected
  )
  expect_equal(
    str_extract_all("2020P", make_year_patterns()['calendar']), expected
  )
  expect_equal(
    str_extract_all("2020R", make_year_patterns()['calendar']), expected
  )
})

