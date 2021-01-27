
test_that("bs_cache() works", {
  # if there is a cache, these tests won't work
  skip_if(bs_has_cache())

  expect_identical(
    withr::with_options(list(bsrto.cache = "foo"), bs_cache()),
    "foo"
  )

  expect_identical(
    withr::with_envvar(list(R_BSRTO_CACHE = "foo"), bs_cache()),
    "foo"
  )
})