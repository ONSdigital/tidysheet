test_that("get_header_row works correctly for simple cases", {

  dat <- data.frame(
    row = rep(1:3, each = 4),
    col = rep(1:4, times = 3),
    data_type = c(
      rep("character", 4),
      rep(rep(c("character", "numeric"), each = 2), times = 2)
    ),
    numeric = c(rep(NA, 6), 1, 2, NA, NA, 5, 6),
    character = c(
      "LA", "Region", "primary", "secondary",
      "cornwall", "SW", NA, NA, "Sussex", "SE", NA, NA)
  )

  # first row is row 1
  header_identifier_pattern <- c('LA')

  # Test case: Expecting a specific message
  expect_message(
    result <- get_header_row(
      dat, header_identifier_pattern
      ),
    "The first header row has been identified as row 1"
  )
  expect_equal(result, 1)


  # first row is row 2
  header_identifier_pattern <- c("(?i)Cornwall")

  expect_message(
    result <- get_header_row(
      dat, header_identifier_pattern
      ),
    "The first header row has been identified as row 2"
  )
  expect_equal(result, 2)
})


test_that("get_header_row works correctly when header pattern is matched on the row after the first header row", {

  # same data as simple test above
  dat <- data.frame(
    row = rep(1:3, each = 4),
    col = rep(1:4, times = 3),
    data_type = c(
      rep("character", 4),
      rep(rep(c("character", "numeric"), each = 2), times = 2)
    ),
    numeric = c(rep(NA, 6), 1, 2, NA, NA, 5, 6),
    character = c(
      "LA", "Region", "primary", "secondary",
      "cornwall", "SW", NA, NA, "Sussex", "SE", NA, NA)
  )

  header_identifier_pattern <- c("(?i)Cornwall")
  header_row_offset <- 1

  expect_message(
    result <- get_header_row(
      dat, header_identifier_pattern,
      offset_by = header_row_offset
      ),
    "The first header row has been identified as row 1"
  )
  expect_equal(result, 1)
})


test_that("get_header_row errors when header is not found", {
  # same data as simple test above
  dat <- data.frame(
    row = rep(1:3, each = 4),
    col = rep(1:4, times = 3),
    data_type = c(
      rep("character", 4),
      rep(rep(c("character", "numeric"), each = 2), times = 2)
    ),
    numeric = c(rep(NA, 6), 1, 2, NA, NA, 5, 6),
    character = c(
      "LA", "Region", "primary", "secondary",
      "cornwall", "SW", NA, NA, "Sussex", "SE", NA, NA)
  )

  header_identifier_pattern <- c("NOTFOUND")

  expect_error(
    result <- suppressWarnings(
      get_header_row(dat, header_identifier_pattern)
    )
  )

})

test_that("get_header_row errors works correctly when header_identifier_instance is required", {

  # two tables the same stached one over the other - assume we only want info
  # from the second, so we want to identify the second instance of a match
  dat <- data.frame(
    row = rep(1:6, each = 4),
    col = rep(1:4, times = 6),
    data_type = rep(
      c(rep("character", 4),
        rep(rep(c("character", "numeric"), each = 2), times = 2)),
      times = 2),
    numeric = rep(c(rep(NA, 6), 1, 2, NA, NA, 5, 6), times = 2),
    character = rep(
      c("LA", "Region", "primary", "secondary",
        "cornwall", "SW", NA, NA, "Sussex", "SE", NA, NA),
      times = 2)
    )

  # Valid header_identifier_instance returns correct row
  header_identifier_pattern <- c("LA")
  expect_message(
    result <- get_header_row(
      dat, header_identifier_pattern, instance = 2
    ),
    "The first header row has been identified as row 4"
  )
  expect_equal(result, 4) # Second instance of "header" is at row 4

  # header_identifier_instance exceeds found instances
  expect_warning(
    result <- suppressMessages(
      get_header_row(dat, header_identifier_pattern, instance = 3)
    ),
    "The specified instance"
  )
  expect_equal(result, 1) # Return first instance when more than one is found

  # Single instance of header_identifier
  expect_message(
    result <- get_header_row(
      dat, header_identifier_pattern, instance = 1
      ),
    "The first header row has been identified as row 1"
  )
  expect_equal(result, 1) # Only one instance, at row 1

  # No instances of header_identifier found
  expect_error(suppressWarnings(
    get_header_row(dat, "header")),
    "No header identifier found in the data"
  )
})

