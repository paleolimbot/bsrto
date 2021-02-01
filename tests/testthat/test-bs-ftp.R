
test_that("bs_ftp_list works", {
  skip_if_offline()
  skip_if_not(interactive())

  expect_is(bs_ftp_list("barrow", quiet = TRUE), "tbl_df")
  expect_message(bs_ftp_list("barrow", quiet = FALSE), "Listing directory")
  expect_output(bs_ftp_list("barrow", print = TRUE, quiet = TRUE), "barrow/")

  mctd_files <- bs_ftp_list("barrow/1998/mctd", recursive = TRUE, quiet = TRUE)
  mctd_files2 <- bs_ftp_list(
    c("barrow/1998/mctd",
      "barrow/1998/mctd/processed",
      "barrow/1998/mctd/raw",
      "barrow/1998/mctd/raw/CalibrationFiles"
    ),
    quiet = TRUE
  )
  expect_identical(mctd_files, mctd_files2)

  mctd_files_asc <- bs_ftp_list(
    "barrow/1998/mctd", "\\.ASC$",
    recursive = TRUE,
    quiet = TRUE
  )

  expect_true(all(grepl("\\.ASC$", mctd_files_asc$file)))
  expect_identical(
    bs_ftp_list(character()),
    tibble::tibble(file = character(), size = numeric())
  )
})

test_that("bs_cached() errors for an invalid data frame", {
  expect_error(bs_cached(data.frame()), "Can't use")
})

test_that("bs_cached() works", {
  skip_if_offline()
  skip_if_not(interactive())

  expect_identical(bs_cached(character()), character())

  cache <- tempfile()
  expect_identical(
    bs_cached("barrow/BarrowStraitDataSummary.xlsx", cache = cache, quiet = TRUE),
    file.path(cache, "barrow/BarrowStraitDataSummary.xlsx")
  )
  expect_true(file.exists(file.path(cache, "barrow/BarrowStraitDataSummary.xlsx")))

  unlink(cache, recursive = TRUE)
})


test_that("bs_cached() works with async = TRUE", {
  skip_if_offline()
  skip_if_not(interactive())

  expect_identical(bs_cached(character(), async = TRUE), character())

  cache <- tempfile()
  expect_identical(
    bs_cached("barrow/BarrowStraitDataSummary.xlsx", cache = cache, async = TRUE, quiet = TRUE),
    file.path(cache, "barrow/BarrowStraitDataSummary.xlsx")
  )
  expect_true(file.exists(file.path(cache, "barrow/BarrowStraitDataSummary.xlsx")))

  unlink(cache, recursive = TRUE)
})
