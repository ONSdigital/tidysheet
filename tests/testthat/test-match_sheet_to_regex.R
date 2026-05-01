test_that("match_sheet_to_regex returns NULL if pattern is not provided",{
  expect_equal(
    match_sheet_to_regex(list(c("Sheet1", "Sheet2")), NA),
    NULL
  )
})

test_that("match_sheet_to_regex gives type errors for sheet_name",{
  expect_error(
    match_sheet_to_regex(list(c("Sheet1", "Sheet2")), "2"),
    "Sheet names should be a vector of character strings",
    fixed = TRUE
    )
  expect_error(
    match_sheet_to_regex(list("Sheet1", "Sheet2"), "2"),
    "Sheet names should be a vector of character strings",
    fixed = TRUE)
  expect_error(
    match_sheet_to_regex(data.frame("Sheets" = c("Sheet1", "Sheet2")), "2"),
    "Sheet names should be a vector of character strings",
    fixed = TRUE
    )
  expect_error(
    match_sheet_to_regex(c(1, 2), "2"),
    "Sheet names should be a vector of character strings",
    fixed = TRUE
    )
})

test_that("match_sheet_to_regex gives type errors for regex",{
  expect_error(
    match_sheet_to_regex(c("Sheet1", "Sheet2"), list(c("Sheet", "2"))),
    "Pattern should be a character string", fixed = TRUE
    )
  expect_error(
    match_sheet_to_regex(
      c("Sheet1", "Sheet2"), data.frame("pattern" = c("Sheet", "2"))
      ),
    "Pattern should be a character string", fixed = TRUE
    )
  expect_error(
    match_sheet_to_regex(c("Sheet1", "Sheet2"), 2),
    "Pattern should be a character string", fixed = TRUE
    )
})

test_that("match_sheet_to_regex returns matching sheet when there is one match", {
  expect_equal(
    match_sheet_to_regex("RA LA Data", "LA"),"RA LA Data"
    )
  expect_equal(
    match_sheet_to_regex("RA LA Data 2020", ".*(RA.*LA.*Data).*"),
    "RA LA Data 2020"
    )
  expect_equal(
    match_sheet_to_regex(
      c("not a match", "RA_LA_Data_2020"), ".*(RA.*LA.*Data).*"),
    "RA_LA_Data_2020"
    )
})

test_that("match_sheet_to_regex returns first sheet only when there are multiple matches", {

  expect_equal(
    suppressWarnings(
      match_sheet_to_regex(c("not", "match 1", "match 2", "not 2"), "match")
    ),
    "match 1"
  )

  expect_warning(
    match_sheet_to_regex(c("not", "match 1", "match 2", "not 2"), "match")
    )

})

test_that("match_sheet_to_regex with no matching sheets returns NULL", {
  expect_equal(
    suppressWarnings(match_sheet_to_regex("String", "pattern")),
    NULL
  )

  expect_warning(match_sheet_to_regex(c("RA LA Data", "Not RA LA Data"), "RA"))

})

