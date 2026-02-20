test_that("get_vintage only returns single_vintage if it is valid", {

  result_final <- suppressMessages(get_vintage("final", NA, NA, NA, NULL, NULL))
  expect_equal(result_final, "final")

  result_budget <- suppressMessages(
    get_vintage("budget", NA, NA, NA, NULL, NULL)
    )
  expect_equal(result_budget, "budget")

  result_provisional <- suppressMessages(
    get_vintage("provisional", NA, NA, NA, NULL, NULL)
    )
  expect_equal(result_provisional, "provisional")

  expect_error(
    suppressMessages(get_vintage("outturn", NA, NA, NA, NULL, NULL)),
    "single_vintage setting must be one of"
    )

})


test_that("get_vintage returns single_vintage if it exists and is valid", {

  result_just_single <- suppressMessages(
    get_vintage("final", NA, NA, NA, NULL, NULL)
    )
  expect_equal(result_just_single, "final")

  result_with_release <- suppressMessages(suppressWarnings(
    get_vintage("final", 1, NA, NA, NULL, NULL)
  ))
  expect_equal(result_with_release, "final")

  result_with_title <- suppressMessages(suppressWarnings(
    get_vintage("final", NA, "budget", NA, NULL, NULL)
  ))

  result_with_dat <- suppressMessages(suppressWarnings(
    get_vintage(
      "final", NA, NA, NA, data.frame(row = 1, character = "budget"), NULL
      )
  ))
  expect_equal(result_with_dat, "final")


  expect_equal(result_with_title, "final")

})


test_that("get_vintage returns release number vintage if it exists and is valid", {

  result_just_release <- suppressMessages(
    get_vintage(NA, 2, NA, NA, NULL, NULL)
    )
  expect_equal(result_just_release, "final")

  result_with_title <- suppressMessages(suppressWarnings(
    get_vintage(NA, 2, "budget", NA, NULL, NULL)
  ))
  expect_equal(result_with_title, "final")

  result_with_dat <- suppressMessages(suppressWarnings(
    get_vintage(
      NA, 2, NA, NA, data.frame(row = 1, character = "budget"), NULL
      )
  ))
  expect_equal(result_with_dat, "final")

})


test_that("get_vintage returns title vintage if it exists and is valid", {

  result_just_title <- suppressMessages(
    get_vintage(NA, NA, "final", NA, NULL, NULL)
    )
  expect_equal(result_just_title, "final")

  result_with_dat <- suppressMessages(suppressWarnings(
    get_vintage(
      NA, NA, "final", NA, data.frame(row = 1, character = "budget"), NULL
      )
  ))
  expect_equal(result_with_dat, "final")

})


test_that("get_vintage returns above table vintage if only it exists and is valid", {

  result <- suppressMessages(
    get_vintage(NA, NA, NA, NA, data.frame(row = 1, character = "final"), NULL)
  )
  expect_equal(result, "final")

})


test_that("get_vintage returns table title vintage if it exists and is valid", {
  result <- suppressMessages(
    get_vintage(NA, NA, NA, "budget data", data.frame(), NULL)
  )
  expect_equal(result, "budget")

})


test_that("get_vintage returns top sheet vintage if only it exists and is valid", {

  result <- suppressMessages(
    get_vintage(NA, NA, NA, NA, data.frame(),
                data.frame(row = 1, character = "provisional"))
  )
  expect_equal(result, "provisional")

})


test_that("get_vintage returns NA and a message if no vintages are found", {

  expect_message(
    result <- get_vintage(NA, NA, NA, NA, data.frame(), data.frame()),
    "vintage not found in dataset metadata or in settings"
  )
  expect_equal(result, NA)

})


test_that("get_vintage returns a warning if more than one unique vintage is found", {

  expect_warning(suppressMessages(
    get_vintage("final", 1, NA, NA, NULL, NULL)
    ),
    "Multiple vintages found.*'final', 'provisional'.$")

  expect_warning(suppressMessages(
    get_vintage(
      NA, NA, "final", NA, data.frame(row = 1, character = "budget"), NULL
      )
    ),
    "Multiple vintages found.*'final', 'budget'.$")
})
