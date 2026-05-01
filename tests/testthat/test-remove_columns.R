dat <- data.frame(
  "row" = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  "col" = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
  "address" = c("A1", "B1", "C1",
                "A2", "B2", "C2",
                "A3", "B3", "C3"),
  "numeric" = c(NA, NA, NA,
                NA, 1, NA,
                NA, 2, NA),
  "character" = c("name", "id", "ID",
                  "1st row", NA, "a",
                  "2nd row", NA, "b"),
  "data_type" = c(
    rep("character", 4), "numeric", rep("character", 2), "numeric", "character"
  )
)

test_that("remove_columns gives expected results with one to one matches", {

  expect_warning(suppressMessages(
    result_one_col <- remove_columns(dat, "id", NA, 1, 1)),
    "have been removed.*B"
  )
  expect_equal(result_one_col, filter(dat, col != 2))


  expect_warning(suppressMessages(
    result_two_cols <- remove_columns(dat, c("name", "id"), NA, 1, 1)),
    "have been removed.*A, B"
  )

  expect_equal(result_two_cols, filter(dat, !col %in% c(1, 2)))

})


test_that("identify_columns_to_remove does not remove a column if there is more than one match", {

  expect_error(
    suppressMessages(remove_columns(dat, "(?i)id", NA, 1, 1)),
    "More than one.*(?i)id"
  )

  expect_error(
    suppressMessages(remove_columns(dat, c("(?i)id", "name"), NA, 1, 1)),
    "More than one.*(?i)id"
    )

})


test_that("remove_columns returns expected result and warnings when no columns are identified", {

  expect_warning(
    result <- suppressMessages(remove_columns(dat, "no match", NA, 1, 1)),
    "No columns matched the pattern 'no match'"
  )
  expect_equal(result, dat)

  result_mixed <- suppressWarnings(suppressMessages(
    remove_columns(dat, c("no match", "id"), NA, 1, 1)
    ))
  expect_equal(result_mixed, filter(dat, col != 2))

})


test_that("remove_columns works when there are muliple header rows", {
  dat <- data.frame(
    "row" = rep(1:4, each = 4),
    "col" = rep(1:4, times = 4),
    "address" = c("A1", "B1", "C1", "D1",
                  "A2", "B2", "C2", "D2",
                  "A3", "B3", "C3", "D3",
                  "A4", "B4", "C4", "D4"),
    "numeric" = c(NA, NA, NA, NA,
                  NA, NA, NA, NA,
                  NA, 1, 1, 3,
                  NA, 2, 2, 4),
    "character" = c(NA, NA, "group A", "group B",
                    "name", "id", "a", "b",
                    "1st row", NA, NA, NA,
                    "2nd row", NA, NA, NA),
    "data_type" = c("blank", "blank", rep("character", 6),
                    rep(c("character", rep("numeric", 3)), 2)
    )
  )

  expect_warning(suppressMessages(
    result <- remove_columns(dat, "id", NA, 1, 2)
    ), "have been removed:\\s+B."
  )

})

