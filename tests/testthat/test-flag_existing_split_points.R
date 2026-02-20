test_that("flag_existing_split_points correctly adds a boolean column for each split_point, stating whether pattern was found", {

  split_patterns <- c( "(\n)\\s*1\\.", "(\n)\\s*2\\.", "(\n)\\s*3\\.")

  dat_all <- data.frame(
    "col_to_split" = c(
      "split\n 1.this \n 2.one \n 3.thousand",
      "split\n 2.one",
      "no splits"),
    "value" = c(100, 600, 200)
  )

  expected_all <- dat_all %>%
    mutate(tmp_use_split_point_1 = c(TRUE, FALSE, FALSE),
           tmp_use_split_point_2 = c(TRUE, TRUE, FALSE),
           tmp_use_split_point_3 = c(TRUE, FALSE, FALSE)
    )

  result_all <- flag_existing_split_points(dat_all, split_patterns, "col_to_split")

  expect_equal(expected_all, result_all)

  #-------
  # test cases individually as well to make it easier to assess (testing them
  # all together tests that diff rows can have diff values)
  all_patterns_matched <- dat_all[1, ]
  all_patterns_matched_expected <- expected_all[1, ]

  all_patterns_matched_result <- flag_existing_split_points(
    all_patterns_matched, split_patterns, "col_to_split"
  )
  expect_equal(all_patterns_matched_expected, all_patterns_matched_result)

  #-------
  some_patterns_matched <- dat_all[2, ]
  some_patterns_matched_expected <- expected_all[2, ]

  some_patterns_matched_result <- flag_existing_split_points(
    some_patterns_matched, split_patterns, "col_to_split"
  )
  expect_equal(some_patterns_matched_expected, some_patterns_matched_result)

  #----
  no_patterns_matched <- dat_all[3, ]
  no_patterns_matched_expected <- expected_all[3, ]

  no_patterns_matched_result <- flag_existing_split_points(
    no_patterns_matched, split_patterns, "col_to_split"
  )
  expect_equal(no_patterns_matched_expected, no_patterns_matched_result)


})


test_that("flag_existing_split_points raises an error if from column is numeric", {
  split_patterns <- c( "(\n)\\s*1\\.")

  dat_all <- data.frame(
    "col_to_split" = 1,
    "value" = 100
  )

  expect_error(
    flag_existing_split_points(dat_all, split_patterns, "col_to_split"),
    "Column 'col_to_split' is expected to contain string data but it contains numeric data"
  )
})
