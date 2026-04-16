test_that("get_units correctly extracts units when there are multiple matches", {
  dat <- data.frame(
    character = c("Revenue (millions)", "Expenses in thousand", "Title of data")
    )
  expect_equal(suppressMessages(get_units(dat)), "million, thousand")
})


test_that("get_units correctly extracts units when there is one match", {
  dat <- data.frame(character = c("Sales: £ Millions", "title of data"))
  expect_equal(suppressMessages(get_units(dat)), "Million")
})


test_that("get_units returns NA when no units are found", {
  dat <- data.frame(character = c("General information", "No unit mentioned"))
  expect_equal(suppressMessages(get_units(dat)), NA)
})


test_that("get_units handles mixed cases and duplicates", {
  dat <- data.frame(
    character = c("million dollar revenue",
                  "million dollar profit",
                  "thousand sales")
    )
  expect_equal(suppressMessages(get_units(dat)), "million, thousand")
})
