test_that("split_tables returns dat as a list if pattern is NA", {
  dat <- data.frame(
    "row" = c(1, 1, 1, 2, 2, 2),
    "col" = c(1, 2, 3, 1, 2, 3),
    "address" = c("A1", "B1", "C1", "A2", "B2", "C2"),
    "character"= c(NA, NA, NA, "table 1", "NA", "table 2")
  )

  expect_equal(
    list(dat),
    split_tables(list(dat), NA, NA, NA, NA, NA)
  )

})


test_that("split_tables raises an error if pattern is NA but other args are not", {
  dat <- data.frame(
    "row" = c(1, 1, 1, 2, 2, 2),
    "col" = c(1, 2, 3, 1, 2, 3),
    "address" = c("A1", "B1", "C1", "A2", "B2", "C2"),
    "character"= c(NA, NA, NA, "table 1", "NA", "table 2")
  )

  expect_error(
    split_tables(list(dat), NA, 1, 0, "col", 1),
    "pattern is required"
  )

})


test_that("split_tables converts instance and offest_by to integers when supplied as character", {
  dat <- data.frame(
    "row" = c(1, 1, 1, 2, 2, 2),
    "col" = c(1, 2, 3, 1, 2, 3),
    "address" = c("A1", "B1", "C1", "A2", "B2", "C2"),
    "character"= c(NA, NA, NA, "table 1", "NA", "table 2")
  )

  expect_no_error(
    suppressMessages(split_tables(list(dat), "table", "1", "1", "col", 1))
  )

})


test_that("split_tables splits by col, retaining all info from the whole table", {
  dat <- data.frame(
    "row" = c(1, 1, 1, 2, 2, 2),
    "col" = c(1, 2, 3, 1, 2, 3),
    "address" = c("A1", "B1", "C1", "A2", "B2", "C2"),
    "character"= c(NA, NA, NA, "table 1", "NA", "table 2")
  )
  left_table <- filter(dat, col < 3)
  right_table <- filter(dat, col >= 3)
  expected <- list(
    dat,
    left_table,
    right_table
  )

  result <- suppressMessages(
    split_tables(list(dat), "table 2", 1, 0, "col", 1)
    )

  expect_equal(result, expected)
})


test_that("split_tables splits horizontally for row", {
  dat <- data.frame(
    "row" = c(1, 1, 2, 2, 3, 3),
    "col" = c(1, 2, 1, 2, 1, 2),
    "address" = c("A1", "B1", "A2", "B2", "A3", "B3"),
    "character"= c("title", NA, "table 1", "NA", "table 2", "NA")
  )
  top_table <- filter(dat, row < 3)
  bottom_table <- filter(dat, row >= 3)
  expected <- list(
    dat,
    top_table,
    bottom_table
  )
  result <- suppressMessages(
    split_tables(list(dat), "table 2", 1, 0, "row", 1)
    )

  expect_equal(result, expected)
})


test_that("split_tables throws error if pattern is not found", {
  dat <- data.frame(
    "row" = c(1, 1, 2, 2, 3, 3),
    "col" = c(1, 2, 1, 2, 1, 2),
    "address" = c("A1", "B1", "A2", "B2", "A3", "B3"),
    "character"= c("title", NA, "table 1", "NA", "table 2", "NA")
  )

  expect_error(
    suppressMessages(
      suppressWarnings(
        split_tables(list(dat), "table 3", 1, 0, "row", 1)
        )
      ),
    "Tables cannot be split."
  )
})


test_that("split_tables uses the first instance of the pattern", {
  dat <- data.frame(
    "row" = c(1, 1, 2, 2, 3, 3),
    "col" = c(1, 2, 1, 2, 1, 2),
    "address" = c("A1", "B1", "A2", "B2", "A3", "B3"),
    "character"= c("title", NA, "table 1", "NA", "table 2", "NA")
  )
  top_table <- filter(dat, row < 2)
  bottom_table <- filter(dat, row >= 2)
  expected <- list(
    dat,
    top_table,
    bottom_table
  )

  result <- suppressMessages(
    split_tables(list(dat), "table", 1, 0, "row", 1)
    )

  expect_equal(result, expected)
})


test_that("split_tables throws an error if dat is not a list", {
  dat <- data.frame(
    "row" = c(1, 1, 2, 2, 3, 3),
    "col" = c(1, 2, 1, 2, 1, 2),
    "address" = c("A1", "B1", "A2", "B2", "A3", "B3"),
    "character"= c("title", NA, "table 1", "NA", "table 2", "NA")
  )

  vector <- 1:3

  expect_error(
    suppressMessages(split_tables(dat, "table 3", 1, 0, "row", 1)),
    "dat must be a list"
  )
  expect_error(
    suppressMessages(split_tables(vector, "table 3", 1, 0, "row", 1)),
    "dat must be a list"
  )
})


test_that("split_tables splits the correct table", {
  table_1 <- data.frame(
    "row" = c(1, 1, 2, 2, 3, 3, 4),
    "col" = c(1, 2, 1, 2, 1, 2, 1),
    "address" = c("A1", "B1", "A2", "B2", "A3", "B3", "A4"),
    "character"= c("title", NA, "table 1", "NA", "table 2", "NA", "table 3")
  )
  top_1 <- filter(table_1, row < 3)
  bottom_1 <- filter(table_1, row >= 3)

  table_list <- list(table_1, top_1, bottom_1)

  top_table <- filter(bottom_1, row < 4)
  bottom_table <- filter(bottom_1, row >= 4)

  expected <- list(
    table_1,
    top_1,
    bottom_1,
    top_table,
    bottom_table
  )
  result <- suppressMessages(
    split_tables(table_list, "table 3", 1, 0, "row", 3)
    )

  expect_equal(result, expected)


})

