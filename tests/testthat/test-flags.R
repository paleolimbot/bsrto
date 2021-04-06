
test_that("internal flag functions work", {
  expect_identical(
    bs_flag(c("not assessed", "probably good data")),
    c(0L, 1L)
  )

  expect_error(bs_flag("not a flag"), "Unknown flag label")

  expect_identical(
    bs_flag_label(c(0L, 1L, NA_integer_, 100)),
    c("not assessed", "probably good data", NA_character_, NA_character_)
  )

  expect_identical(names(bs_flag_scheme()), c("flag_label", "flag"))
})
