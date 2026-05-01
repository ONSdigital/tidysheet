test_that("get_subtitles returns NA if no variables are supplied", {

  dat <- data.frame(
    row = 1, col = 1
  )

  expect_equal(get_subtitles(dat, NA, NA, NA, NA), NA)

})


test_that("get_subtitles raises an error if one required variable is specified but not both", {

  dat <- data.frame(
    row = 1, col = 1
  )

  expect_error(
    get_subtitles(dat, "subtitle", NA, NA, NA),
    "If one is supplied they both must be"
    )
  expect_error(
    get_subtitles(dat, NA, "table", NA, NA),
    "If one is supplied they both must be"
    )

})


test_that("get_subtitles raises an error if no subtable titles are found", {

  dat <- data.frame(
    row = rep(c(1:3), each = 2),
    col = rep(c(1:2), times = 3),
    character =
      c(NA, "budget",
        "A", "item 1",
        "item 3", NA
      ),
    numeric = c(rep(NA, 5), 1),
    is_blank = c(TRUE, rep(FALSE, 5))
  )

  expect_error(
    suppressMessages(get_subtitles(dat, "subtitle", "table A", 1, NA)),
    "No rows in the data contain all the patterns"
  )

})


test_that("get_subtitles works with single pattern, and no index or offset", {

  dat <- data.frame(
    row = rep(c(1:5), each = 2),
    col = rep(c(1:2), times = 5),
    character =
      c("table A", NA,
        NA, "item 1",
        "item 3", NA,
        "table B", NA,
        "item 4", NA
      ),
    numeric = c(rep(NA, 5), 1, rep(NA, 3), 2),
    is_blank = c(FALSE, TRUE, TRUE, rep(FALSE, 4), TRUE, FALSE, FALSE)
  )

  expected <- tibble(row = c(1, 4), subtitle = c("table A", "table B"))

  result <- suppressMessages(
    get_subtitles(dat, "subtitle", "(?i)table", NA, NA)
  )

  expect_equal(result, expected)

})


test_that("get_subtitles works with positive offset", {

  dat <- data.frame(
    row = rep(c(1:3), each = 2),
    col = rep(c(1:2), times = 3),
    character =
      c(NA, "budget",
        "A", "item 1",
        "item 3", NA
      ),
    numeric = c(rep(NA, 5), 1),
    is_blank = c(TRUE, rep(FALSE, 5))
  )

  expected <- tibble(row = 2, subtitle = "A")

  result <- suppressMessages(
    get_subtitles(dat, "subtitle", "budget", 1, NA)
  )

  expect_equal(result, expected)

})


test_that("get_subtitles works with negative offset", {

  dat <- data.frame(
    row = rep(c(1:3), each = 2),
    col = rep(c(1:2), times = 3),
    character =
      c(NA, "budget",
        "A", "item 1",
        "item 3", NA
      ),
    numeric = c(rep(NA, 5), 1),
    is_blank = c(TRUE, rep(FALSE, 5))
  )

  expected <- tibble(row = 1, subtitle = "budget")

  result <- suppressMessages(
    get_subtitles(dat, "subtitle", "A", -1, NA)
  )

  expect_equal(result, expected)

})


test_that("get_subtitles works with negative horizontal index", {

  dat <- data.frame(
    row = rep(c(1:3), each = 2),
    col = rep(c(1:2), times = 3),
    character =
      c("budget", "table A",
        NA, "item 1",
        "item 3", NA
      ),
    numeric = c(rep(NA, 5), 1),
    is_blank = c(FALSE, FALSE, TRUE, rep(FALSE, 3))
  )

  expected <- tibble(row = 1, subtitle = "table A")

  result <- suppressMessages(
    get_subtitles(dat, "subtitle", "budget", NA, -1)
  )

  expect_equal(result, expected)

})


test_that("get_subtitles works with positive horizontal index", {

  dat <- data.frame(
    row = rep(c(1:3), each = 2),
    col = rep(c(1:2), times = 3),
    character =
      c("budget", "table A",
        NA, "item 1",
        "item 3", NA
      ),
    numeric = c(rep(NA, 5), 1),
    is_blank = c(FALSE, FALSE, TRUE, rep(FALSE, 3))
  )

  expected <- tibble(row = 1, subtitle = "budget")

  result <- suppressMessages(
    get_subtitles(dat, "subtitle", "budget", NA, 1)
  )

  expect_equal(result, expected)

})


test_that("get_subtitles works with multiple patterns, negative offset and negative
index", {

  dat <- data.frame(
      row = rep(c(1:5), each = 3),
      col = rep(c(1:3), times = 5),
      character =
          c("million", NA, "table A",
            NA, "col A", "col B",
            "c", NA, NA,
            NA, NA, "table B",
            NA, "col A", "col B"
            ),
      numeric = c(rep(NA, 7), 1, 2, rep(NA, 6)),
      is_blank = c(FALSE, TRUE, FALSE, TRUE, rep(FALSE, 5),
                   rep(c(TRUE, TRUE, FALSE), 2))
      )

  expected <- tibble(row = c(1, 4), subtitle = c("table A", "table B"))

  result <- suppressMessages(
    get_subtitles(dat, "subtitle", c("(?i)a", "(?i)col.*b"), -1, -1)
  )

  expect_equal(result, expected)

})


test_that("get_subtitles only finds rows that match ALL provided patterns", {

  dat <- data.frame(
    row = rep(c(1:4), each = 2),
    col = rep(c(1:2), times = 4),
    character =
      c("table A", "budget",
        NA, "item 1",
        "item 3", NA,
        "item table", NA
      ),
    numeric = c(rep(NA, 5), 1, NA, 2),
    is_blank = c(FALSE, FALSE, TRUE, rep(FALSE, 5))
  )

  expected <- tibble(row = 1, subtitle = "table A")

  result <- suppressMessages(
    get_subtitles(dat, "subtitle", c("budget", "(?i)table"), NA, NA)
  )

  expect_equal(result, expected)

})
