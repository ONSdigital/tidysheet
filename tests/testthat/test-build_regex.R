test_that("build_regex uses equals to use the word immediately after", {

  pattern <- build_regex("ALT_=this")
  expect_equal(pattern, "this")

  usage <- c(grepl(pattern, "this is true"), grepl(pattern, "false"))
  expect_equal(usage, c(TRUE, FALSE))

})


test_that("build_regex uses equals to use the character immediately after", {

  pattern <- build_regex("ALT_= ")
  expect_equal(pattern, " ")

  usage <- c(grepl(pattern, "this is true"), grepl(pattern, "false"))
  expect_equal(usage, c(TRUE, FALSE))

})


test_that("build_regex handles start pattern", {

  pattern <- build_regex("ALT_start_=this")
  expect_equal(pattern, "^this")

  usage <- c(grepl(pattern, "this is true"), grepl(pattern, "false this is"))
  expect_equal(usage, c(TRUE, FALSE))

})


test_that("build_regex handles end pattern", {

  pattern <- build_regex("ALT_=this_end")
  expect_equal(pattern, "this$")

  usage <- c(grepl(pattern, "this is false"), grepl(pattern, "true is this"))
  expect_equal(usage, c(FALSE, TRUE))

})


test_that("build_regex handles or pattern", {

  pattern <- build_regex("ALT_=this_or_=that")
  expect_equal(pattern, "this|that")

  usage <- c(
    grepl(pattern, "this is true"),
    grepl(pattern, "true is that"),
    grepl(pattern, "now false")
    )

  expect_equal(usage, c(TRUE, TRUE, FALSE))

})


test_that("build_regex handles anyrepeats pattern", {

  pattern <- build_regex("ALT_=this_period_anyrepeats_=true")
  expect_equal(pattern, "this\\.*true")

  usage <- c(
    grepl(pattern, "this.....true"),
    grepl(pattern, "thistrue"),
    grepl(pattern, "false")
    )
  expect_equal(usage, c(TRUE, TRUE, FALSE))

})


test_that("build_regex handles anycase pattern", {

  pattern <- build_regex("ALT_anycase_=this")
  expect_equal(pattern, "(?i)this")

  usage <- c(
    grepl(pattern, "this is true"),
    grepl(pattern, "THIS is true"),
    grepl(pattern, "tHiS is true"),
    grepl(pattern, "that is false"))
  expect_equal(usage, c(TRUE, TRUE, TRUE, FALSE))

})


test_that("build_regex handles anychar pattern", {

  pattern <- build_regex("ALT_=this_anychar_=that")
  expect_equal(pattern, "this.that")

  usage <- c(
    grepl(pattern, "this that"),
    grepl(pattern, "this&that"),
    grepl(pattern, "this or that")
    )
  expect_equal(usage, c(TRUE, TRUE, FALSE))

})


test_that("build_regex handles fromnorepeatsto and endrepeats patterns", {

  pattern <- build_regex("ALT_=is_whitespace_=a_fromnorepeatsto_=2_endrepeats_=b")
  expect_equal(pattern, "is\\s*a{0,2}b")

  usage <- c(
    grepl(pattern, "this is b true"),
    grepl(pattern, "this is ab true"),
    grepl(pattern, "this is aab true"),
    grepl(pattern, "this is aaab false")
    )
  expect_equal(usage, c(TRUE, TRUE, TRUE, FALSE))

})


test_that("build_regex handles period pattern", {

  pattern <- build_regex("ALT_period")
  expect_equal(pattern, "\\.")

  usage <- c(grepl(pattern, ". true"), grepl(pattern, ", false"))
  expect_equal(usage, c(TRUE, FALSE))

})


test_that("build_regex handles colon pattern", {

  pattern <- build_regex("ALT_colon")
  expect_equal(pattern, "\\:")

  usage <- c(grepl(pattern, ": true"), grepl(pattern, "; false"))
  expect_equal(usage, c(TRUE, FALSE))

})


test_that("build_regex handles hyphen pattern", {

  pattern <- build_regex("ALT_hyphen")
  expect_equal(pattern, "-")

  usage <- c(grepl(pattern, "- true"), grepl(pattern, "_ false"))
  expect_equal(usage, c(TRUE, FALSE))

})


test_that("build_regex handles poundsign pattern", {

  pattern <- build_regex("ALT_poundsign")
  expect_equal(pattern, paste0("\\",enc2utf8("\u00A3")))

  usage <- c(grepl(pattern, "£ true"), grepl(pattern, "false"))
  expect_equal(usage, c(TRUE, FALSE))

})


test_that("build_regex handles openroundbrace pattern", {

  pattern <- build_regex("ALT_openroundbrace")
  expect_equal(pattern, "\\(")

  usage <- c(grepl(pattern, "(true"), grepl(pattern, "false"))
  expect_equal(usage, c(TRUE, FALSE))

})


test_that("build_regex handles closeroundbrace pattern", {

  pattern <- build_regex("ALT_closeroundbrace")
  expect_equal(pattern, "\\)")

  usage <- c(grepl(pattern, "true)"), grepl(pattern, "false"))
  expect_equal(usage, c(TRUE, FALSE))

})


test_that("build_regex handles whitespace pattern", {

  pattern <- build_regex("ALT_=this_whitespace_=true")
  expect_equal(pattern, "this\\s*true")

  usage <- c(
    grepl(pattern, "this\rtrue)"),
    grepl(pattern, "this\ntrue)"),
    grepl(pattern, "this true"),
    grepl(pattern, "thistrue"),
    grepl(pattern, "this is not true")
  )
  expect_equal(usage, c(TRUE, TRUE, TRUE, TRUE, FALSE))

})


test_that("build_regex handles dash pattern", {

  pattern <- build_regex("ALT_=this_dash_=that")
  expect_equal(pattern, "this\\s+-\\s+that")

  usage <- c(
    grepl(pattern, "this - that true"),
    grepl(pattern, "this-that false"),
    grepl(pattern, "this -that false"),
    grepl(pattern, "this- that false")
    )
  expect_equal(usage, c(TRUE, FALSE, FALSE, FALSE))


})


test_that("build_regex handles newline pattern", {

  pattern <- build_regex("ALT_=this_newline_=true")
  expect_equal(pattern, "this(\r\n|\r|\n)true")

  usage <- c(
    grepl(pattern, "this\rtrue)"),
    grepl(pattern, "this\ntrue)"),
    grepl(pattern, "this\r\rtrue not)"),
    grepl(pattern, "this\n\ntrue)"),
    grepl(pattern, "this true not")
  )
  expect_equal(usage, c(TRUE, TRUE, FALSE, FALSE, FALSE))

})


test_that("build_regex handles nocharacter pattern", {

  pattern <- build_regex("ALT_nocharacter_=a")
  expect_equal(pattern, "\\ba")

  usage <- c(
    grepl(pattern, "a true)"),
    grepl(pattern, " a true"),
    grepl(pattern, ".a true"),
    grepl(pattern, "na false"),
    grepl(pattern, "1a false")
    )
  expect_equal(usage, c(TRUE, TRUE, TRUE, FALSE, FALSE))

})


test_that("build_regex handles number pattern", {

  pattern <- build_regex("ALT_number")
  expect_equal(pattern, "\\d")

  usage <- c(
    grepl(pattern, "1 true"),
    grepl(pattern, "tru3"),
    grepl(pattern, "false")
    )
  expect_equal(usage, c(TRUE, TRUE, FALSE))

})


test_that("build_regex handles word pattern", {

  pattern <- build_regex("ALT_=this_whitespace_word_whitespace_=true")
  expect_equal(pattern, "this\\s*\\w\\s*true")

  usage <- c(
    grepl(pattern, "this i true"),
    grepl(pattern, "this 1 true"),
    grepl(pattern, "this $ true not"),
    grepl(pattern, "this isnt true"),
    grepl(pattern, "this true not")
  )
  expect_equal(usage, c(TRUE, TRUE, FALSE, FALSE, FALSE))

})


test_that("build_regex handles letter pattern", {
  pattern <- build_regex("ALT_=this_whitespace_letter_whitespace_=true")
  expect_equal(pattern, "this\\s*[a-z, A-Z]\\s*true")

  usage <- c(
    grepl(pattern, "this i true"),
    grepl(pattern, "this 1 true not"),
    grepl(pattern, "this $ true not"),
    grepl(pattern, "this isnt true"),
    grepl(pattern, "this true")
  )
  expect_equal(usage, c(TRUE, FALSE, FALSE, FALSE, TRUE))
})


test_that("build_regex handles underscore pattern", {

  pattern <- build_regex("ALT_=this_underscore")
  expect_equal(pattern, "this_")

  usage <- c(grepl(pattern, "this_true"), grepl(pattern, "this false"))
  expect_equal(usage, c(TRUE, FALSE))

})


test_that("build_regex throws an error if any instructions start with an underscore", {
  expect_error(
    build_regex(c("ALT_=word", "ALT__whitespace")),
    "instructions must not start with '_'"
    )
})


test_that("build_regex returns NA if instructions are NA", {
  expect_equal(build_regex(NA), NA)
})


test_that("build_regex raises an error for unrecognised patterns", {
  expect_error(
    build_regex(c("ALT_space")),
    "'space' passed to R from the data dictionary is not present in the regex_lookup"
  )
})
