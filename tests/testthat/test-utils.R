
test_that("resample functions work", {

  df <- tibble::tibble(
    x = 1:10,
    y = 11:20
  )

  expect_equal(resample_exact(df$x, df$y, 10:1), 20:11)
  expect_equal(resample_exact(df$x, df$y, 10:1 + 0.1), rep(NA_real_, 10))

  expect_equal(resample_last(df$x, df$y, 10:1 + 0.1), 20:11)
  expect_equal(resample_last(df$x, df$y, 10:1 - 0.1), c(19:11, NA))

  expect_equal(resample_linear(df$x, df$y, 10:1), 20:11)
  expect_equal(resample_linear(df$x, df$y, 10:1 + 0.1), c(NA, 19:11 + 0.1))
  expect_equal(resample_linear(df$x, df$y, 10:1 - 0.1), c(20:12 - 0.1, NA))

  expect_equal(resample_nearest(df$x, df$y, 10:1), 20:11)
  expect_equal(resample_nearest(df$x, df$y, 10:1 + 0.1), 20:11)
  expect_equal(resample_nearest(df$x, df$y, 10:1 - 0.1), 20:11)
  expect_equal(
    resample_nearest(df$x, df$y, 10:1 - c(0.08, 0.1), max_distance = 0.09),
    c(20, NA, 18, NA, 16, NA, 14, NA, 12, NA)
  )
})

test_that("bs_version_info() works",{
  expect_is(bs_version_info(), "character")
  expect_length(bs_version_info(), 1)
  expect_match(bs_version_info(), "^bsrto")
})
