test_that("add_metadata_columns gives expected output for single items", {

  dat <- tibble(
    Year = c(2023, 2024, 2025),
    Value = c(1:3)
  )

  expected <- dat %>%
    mutate(units = "count",
           title = "Counts by year")

  units <- "count"
  title <- "Counts by year"

  # 2 new cols
  expect_equal(
    suppressMessages(add_metadata_columns(dat, units, title)), expected
    )
  # 1 new col
  expect_equal(
    suppressMessages(add_metadata_columns(dat, title)), expected[, c(1, 2, 4)]
    )

})


test_that("add_metadata_columns returns original data and a warning when there are no valid variables", {

  dat <- tibble(
    Year = c(2023, 2024, 2025),
    Value = c(1:3)
  )

  units <-  c("count", "thousands")

  expect_warning(
    suppressMessages(
      result <- add_metadata_columns(dat, units)
    ),
    "Variables must be of length 1"
  )

  expect_equal(result, dat)

})


test_that("add_metadata_columns does not overwrite existing info", {

  dat <- tibble(
    vintage = c("final", "budget")
  )

  expected <- mutate(dat, units = "thousands")

  units <-  "thousands"
  vintage <-  "final"

  expect_warning(
    suppressMessages(
      result <- add_metadata_columns(dat, units, vintage)
    ),
    "vintage is already a column in the data "
  )

  expect_equal(result, expected)

})


test_that("add_metadata_columns raises expected warnings", {

  dat <- tibble(
    Year = c(2023, 2024, 2025),
    Value = c(1:3)
  )

  expected <- dat %>%
    mutate(title = "Counts by year")

  species <- c("chaffich", "hawfinch")
  title <- "Counts by year"
  units <- c("count", "weight")

  expect_warning(
    suppressMessages(
      result <- add_metadata_columns(dat, species, title, units)
    ),
    "not been added: 'species', 'units', as"
  )

  expect_equal(result, expected)
})

