
test_that("build_navigator_meta() returns a named list() of character()", {
  meta <- build_navigator_meta(Sys.time(), Sys.time())
  expect_true(all(vapply(meta, is.character, logical(1))))
  expect_true(all(vapply(meta, length, integer(1)) == 1))
  expect_true(!is.null(names(meta)))
  expect_true(all(names(meta) != ""))
})
