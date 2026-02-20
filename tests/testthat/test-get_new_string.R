test_that("get_new_string creates a column containing the newly split off string", {

  # - If there are no split_point pattern matches in an item, all 'tmp_string_'
  # columns will equal the entry in the original column (which has been copied to
  # 'tmp_remaining' by this point). This is tested in the second and third rows of
  # dat/expected_1/result_1.

  # - If current_split is NA, there are no splits left to do so the last string is
  # carried forward. This is tested in the second row of dat/expected_1/result_1.

  dat <- data.frame(
    tmp_remaining = c(
      "all \n 1.split points \n 2.present",
      "no splits",
      "only the \n 2.second split",
      "only the \n 1.first split"),
    tmp_use_split_point_0 = TRUE,
    tmp_use_split_point_1 = c(TRUE, FALSE, FALSE, TRUE),
    tmp_use_split_point_2 = c(TRUE, FALSE, TRUE, FALSE),
    tmp_split_1 = c(1, NA, 2, 1),
    tmp_split_2 = c(2, NA, 2, NA),
    tmp_split_3 = NA,
    value = c(1:4)
  )

  expected_1 <- dat %>%
    mutate(tmp_action_to_take = c("split", "remaining", "split", "split"),
           tmp_string_1 = c("all ", "no splits", "only the ", "only the "))

  result_1 <- get_new_string(
    dat,
    split_patterns = c("(\n)\\s*1\\.", "(\n)\\s*2\\."),
    current_string_col = "tmp_string_1",
    prev_string_col = "tmp_remaining",
    use_split_point = "tmp_use_split_point_0",
    current_split = "tmp_split_1"
  )

  expect_equal(expected_1, result_1)

  #-----------------
  # 2nd iteration call to this function

  # - If current_split is NA, there are no splits left to do so the last string is
  # carried forward. This is tested in the second and fourth rows of
  # dat_2/expected_2/result_2.

  # - If use_split_point is FALSE, the regex pattern in split_patterns does not exist,
  # so the last string is also carried forward in this situation.  This is tested in
  # the second and third rows of dat_2/expected_2/result_2.

  dat_2 <- result_1 %>%
    mutate(
      tmp_remaining = c(
        "\n 1.split points \n 2.present",
        "no splits",
        "\n 2.second split",
        "\n 1.first split"
      ))

  expected_2 <- dat_2 %>%
    mutate(tmp_action_to_take = c("split", "remaining", "copy forward", "remaining"),
           tmp_string_2 = c("\n 1.split points ", "no splits", "only the ", "\n 1.first split"))

  result_2 <- get_new_string(
    dat_2,
    split_patterns = c("(\n)\\s*1\\.", "(\n)\\s*2\\."),
    current_string_col = "tmp_string_2",
    prev_string_col = "tmp_string_1",
    use_split_point = "tmp_use_split_point_1",
    current_split = "tmp_split_2"
  )

  expect_equal(expected_2, result_2)

  #-----------------
  # 3rd (last) iteration call to this function

  # - If current_split is NA, there are no splits left to do so the last string is
  # carried forward. This is tested in the all rows of dat_3/expected_3/result_3,
  # as should always be the case in the final iteration.

  # - If use_split_point is FALSE, the regex pattern in split_patterns does not exist,
  # so the last string is also carried forward in this situation. This is tested in
  # the second and fourth rows of dat_3/expected_3/result_3.

  dat_3 <- result_2 %>%
    mutate(
      tmp_remaining = c(
        "2.present",
        "no splits",
        "2.second split",
        "1.first split"
      )
    )

  expected_3 <- dat_3 %>%
    mutate(tmp_action_to_take = "remaining",
           tmp_string_3 = c("2.present", "no splits", "2.second split", "1.first split"))

  result_3 <- get_new_string(
    dat_3,
    split_patterns = c("(\n)\\s*1\\.", "(\n)\\s*2\\."),
    current_string_col = "tmp_string_3",
    prev_string_col = "tmp_string_2",
    use_split_point = "tmp_use_split_point_2",
    current_split = "tmp_split_3"
  )

  expect_equal(expected_3, result_3)
})

test_that("get_new_string raises expected errors and warnings", {
  dat <- data.frame(
    tmp_remaining = "all \n 1.split points \n 2.present",
    tmp_use_split_point_0 = TRUE,
    tmp_split_1 = 1,
    value = 100
  )

  #----
  current_string_col_already_exists <- mutate(dat, tmp_string_1 = "this should not exist yet")
  expect_warning(
    get_new_string(
      current_string_col_already_exists,
      split_patterns = c("(\n)\\s*1\\.", "(\n)\\s*2\\."),
      current_string_col = "tmp_string_1",
      prev_string_col = "tmp_remaining",
      use_split_point = "tmp_use_split_point_0",
      current_split = "tmp_split_1"
    ),
    "'tmp_string_1' should not exist yet but it does. It will be overwritten"
  )
  #----
  prev_string_col_missing <- select(dat, -tmp_remaining)
  expect_error(
    get_new_string(
      prev_string_col_missing,
      split_patterns = c("(\n)\\s*1\\.", "(\n)\\s*2\\."),
      current_string_col = "tmp_string_1",
      prev_string_col = "tmp_remaining",
      use_split_point = "tmp_use_split_point_0",
      current_split = "tmp_split_1"
    ),
    "'tmp_remaining' should be a column name, but it was not found"
  )
  #----
  use_split_point_missing <- select(dat, -tmp_use_split_point_0)
  expect_error(
    get_new_string(
      use_split_point_missing,
      split_patterns = c("(\n)\\s*1\\.", "(\n)\\s*2\\."),
      current_string_col = "tmp_string_1",
      prev_string_col = "tmp_remaining",
      use_split_point = "tmp_use_split_point_0",
      current_split = "tmp_split_1"
    ),
    "'tmp_use_split_point_0' should be a column name, but it was not found"
  )

  #----
  current_split_missing <- select(dat, -tmp_split_1)
  expect_error(
    get_new_string(
      current_split_missing,
      split_patterns = c("(\n)\\s*1\\.", "(\n)\\s*2\\."),
      current_string_col = "tmp_string_1",
      prev_string_col = "tmp_remaining",
      use_split_point = "tmp_use_split_point_0",
      current_split = "tmp_split_1"
    ),
    "'tmp_split_1' should be a column name, but it was not found"
  )

}
)
