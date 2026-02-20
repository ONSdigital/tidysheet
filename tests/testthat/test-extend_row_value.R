test_that("extend_row_value returns dat if relevant variables are all NA", {

  dat <- data.frame(address = "A9")

  result <- extend_row_value(dat, NA, NA, NA)

  expect_equal(result, dat)
})


test_that("extend_row_value raises an error if one but not all relevant variables are NA", {

  dat <- data.frame(address = "A9")

  expect_error(
    extend_row_value(dat, "start_whitespace_period", NA, NA),
    "If one is specified all must be"
  )

  expect_error(
    extend_row_value(dat, NA, "forward", NA),
    "If one is specified all must be"
  )

  expect_error(
    extend_row_value(dat, NA, NA, "above"),
    "If one is specified all must be"
  )


})


test_that("extend_row_value correctly adds information to row from the 'above'
row and place the information 'forward'", {

  dat <- data.frame(
    address = c(
      "A9", "A10", "A11", "A12", "A13", "A14"
      ),
    row = 9:14,
    col = 1,
    character = c(
      "single pattern match line",
      "...of which HRA",
      "multiple pattern match lines",
      "...of which grants",
      "...of which HRA",
      "no pattern match lines"
      )
  )

  expected <- dat %>%
    mutate(character = c(
      "single pattern match line",
      "...of which HRA single pattern match line",
      "multiple pattern match lines",
      "...of which grants multiple pattern match lines",
      "...of which HRA multiple pattern match lines",
      "no pattern match lines"
    ))

  result <- extend_row_value(
    dat = dat,
    pattern = "^\\s*\\.\\.\\.\\s*of\\s*which",
    placement = "forward", direction = "above"
    )

  expect_equal(result, expected)
})


test_that("extend_row_value correctly adds information when there are multiple
columns with pattern matches", {

  # e.g.
  #
  # Fruit           | random column | value |
  #=================|===============|=======|
  #Apple            | some info     |  3    |
  #...of which pips |...of which x  |  1    |
  #...of which skin |               |  2    |
  #==========================================
  dat <- data.frame(
    address = c("A2", "A3", "A4", "B2", "B3", "B4"),
    row = rep(c(2, 3, 4), 2),
    col = rep(1:2, each = 3),
    character = c(
      "apple",
      "...of which pips",
      "...of which skin",
      "some info",
      "...of which x",
      NA
    )
  )

  expected <- dat %>%
    mutate(character = c(
    "apple",
    "...of which pips apple",
    "...of which skin apple",
    "some info",
    "...of which x",
    NA
    ))

  expect_warning(
    result <- suppressMessages(
      extend_row_value(
        dat = dat,
        pattern = "^\\s*\\.\\.\\.\\s*of\\s*which",
        direction = "above",
        placement = "forward")
    ),
    "Text matching the extend_row_pattern in column A will be combined to "
  )

  expect_equal(result, expected)

})


test_that("extend_row_value correctly adds information to row from the 'below'
row and place it in 'reverse' to original value", {

  dat <- data.frame(
    address = c("A9", "A10", "A11", "A12", "A13", "A14"),
    character = c(
      "...of which HRA",
      "single pattern match line",
      "...of which grants",
      "...of which HRA",
      "multiple pattern match lines",
      "no pattern match lines"
    ),
    row = 9:14,
    col = 1
  )

  expected <- dat %>%
    mutate(character = c(
      "single pattern match line ...of which HRA",
      "single pattern match line",
      "multiple pattern match lines ...of which grants",
      "multiple pattern match lines ...of which HRA",
      "multiple pattern match lines",
      "no pattern match lines"
    ))

  result <- extend_row_value(
    dat = dat,
    pattern = "^\\s*\\.\\.\\.\\s*of\\s*which",
    direction = "below",
    placement = "reverse"
    )

  expect_equal(result, expected)
})


test_that("extend_row_value throws errors when extend_row_order and
extend_row_pattern are not in the allowed options", {
  dat <- data.frame(
    address = c("A9", "A10"),
    character = c("...of which HRA",
                  "Acquisition of land & existing buildings"),
    row = 9:10,
    col = 1
  )

  expect_error(
    extend_row_value(
      dat = dat,
      pattern = "^\\s*\\.\\.\\.\\s*of\\s*which",
      direction = "sideways",
      placement = "forward"
      ),
    "must be either 'above' or 'below'"
  )

  expect_error(
    extend_row_value(
      dat = dat,
      pattern = "^\\s*\\.\\.\\.\\s*of\\s*which",
      direction = "below",
      placement = "upward"
    ),
    "must be either 'forward' or 'reverse'"
  )

})


test_that("extend_row_value throws an error when there are no matches to the pattern provided", {

  dat <- data.frame(
    address = c("A9", "A10"),
    row = 9:10,
    col = 1,
    character = c(
      "single pattern match line",
      "...of which HRA"
    )
  )

  expect_error(
    extend_row_value(
      dat = dat,
      pattern = "=no",
      placement = "forward", direction = "above"
    ),
    "no text was found that matched extend_row_pattern"
  )
})

