test_that("remove_metadata_cells removes expected cells with default n_row", {

  dat <- data.frame(
    character = c("Title", "header_info_1",
                  "Units", "header_info_2",
                  "header_info_3"),
    address = c("A1", "B1",
                "A2", "B2",
                "B3"),
    row = c(1, 1, 2, 2, 3),
    col = c(1, 2, 1, 2, 2)
  )

  expect_warning(result_one_pattern <- suppressMessages(
      remove_metadata_cells(dat, "(?i)units")
    ),
    "The following cells have been removed.*A2"
  )
  expect_equal(result_one_pattern, filter(dat, address != "A2"))


  expect_warning(
    result_both_patterns <- suppressMessages(
      remove_metadata_cells(dat, c("(?i)units", "(?i)title"))
    ),
    "The following cells have been removed.*A1, A2"
  )
  expect_equal(
    result_both_patterns, filter(dat, !address %in% c("A1","A2"))
  )

})


test_that("remove_metadata_cells only removes cells within specified number of rows", {

  dat <- data.frame(
    character = c("Title", "header_info_1",
                  "Units", "header_info_2"),
    address = c("A1", "B1",
                "A2", "B2"),
    row = c(1, 1, 2, 2),
    col = c(1, 2, 1, 2)
  )

  # units is in the second row of data so we don't expect it to be dropped.
  expect_warning(result <- suppressMessages(
    remove_metadata_cells(dat, c("(?i)uni", "(?i)tit"), 1)
    ),
    "The following cells have been removed.*A1"
  )

  expect_equal(result, filter(dat, address != "A1"))

})


test_that("remove_metadata_cells n_row argument acts on existing rows not row number", {
  dat <- data.frame(
    character = c(NA, "header_info_1",
                  "Title", "header_info_2",
                  "Units", NA),
    address = c("A1", "B1",
                "A3", "B3",
                "A4", "B4"),
    row = c(1, 1, 3, 3, 4, 4),
    col = c(1, 2, 1, 2, 1, 2)
  )

  # 'title' is on row 3 but this is within the first 2 existing rows, so we
  # expect it to be removed. 'uni' is on the 3rd row so we expect it to remain.
  expect_warning(
    result <- suppressMessages(
      remove_metadata_cells(dat, c("(?i)unit", "(?i)title"), 2)
      ),
    "The following cells have been removed.*A3"
  )
  expect_equal(result, filter(dat, address != "A3"))

})


test_that("remove_metadata_cells only removes the first match for each pattern", {

  # mess up the order as a secondary test (because of the row and col columns,
  # 'Title' is the first match for "(?i)tit" not "Title 2".)
  dat <- data.frame(
    character = c("Title 2", "header_info_2",
                  "units2", "Units",
                  "header_info_1", "Title"),
    address = c("A2", "B2",
                "B3", "A3",
                "B1", "A1"),
    row = c(2, 2, 3, 3, 1, 1),
    col = c(1, 2, 2, 1, 2, 1)
  )

  expect_warning(
    result_diff_matches_for_each_pattern <- suppressMessages(
      remove_metadata_cells(dat, c("(?i)uni", "(?i)tit"))
    ),
    "The following cells have been removed.*A1, A3"
  )

  expect_warning(
    result_same_match_for_each_pattern <- suppressMessages(
      remove_metadata_cells(dat, c("(?i)it", "(?i)title"))
    ),
    "The following cells have been removed.*A1"
  )
})


test_that("remove_metadata_cells returns original data when no matches are found", {
  dat <- data.frame(
    character = "Title", address = "A1", row = 1, col = 1
  )

  expect_warning(
    result <- suppressMessages(remove_metadata_cells(dat, "unit")),
    "No cells were found to match the metadata_cells_to_remove_patterns"
  )

})
