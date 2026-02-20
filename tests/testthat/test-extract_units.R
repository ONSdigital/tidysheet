test_that("extract_units correctly extracts units when there are multiple matches", {
  units_patterns <- "thousand|million|000s|£ Millions"
  dat <- data.frame(
    character = c("Revenue (millions)", "Expenses in thousand", "Title of data")
    )
  expect_equal(
    extract_units(dat, units_patterns),
    "million, thousand"
    )
})


test_that("extract_units correctly extracts units when there is one match", {
  units_patterns <- "thousand|million|000s|£ Millions"
  dat <- data.frame(character = c("Sales: £ Millions", "title of data"))
  expect_equal(extract_units(dat, units_patterns), "£ Millions")
})


test_that("extract_units returns empty string when no units are found", {
  units_patterns <- "thousand|million|000s|£ Millions"
  dat <- data.frame(character = c("General information", "No unit mentioned"))
  expect_equal(extract_units(dat, units_patterns), "")
})


test_that("extract_units handles mixed cases and duplicates", {
  units_patterns <- "thousand|million"
  dat <- data.frame(
    character = c("million dollar revenue",
                  "million dollar profit",
                  "thousand sales")
    )
  expect_equal(extract_units(dat, units_patterns), "million, thousand")
})
