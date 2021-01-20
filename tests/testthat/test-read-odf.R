
test_that("read_odf_header() works", {
  file <- bs_example("odf/CTD_98911_10P_11_DN.ODF")
  header <- read_odf_header_lines(file)
  expect_length(header, 186)
  expect_equal(readr::read_lines(file, skip = 186, n_max = 1), " -- DATA --")

  # force detection of header size
  header <- read_odf_header_lines(file, n_header = 10)
  expect_length(header, 186)

  # invalid file
  tmpfile <- tempfile()
  write("totally bogus information", tmpfile)
  expect_error(read_odf_header_lines(tmpfile), "Can't find")
  unlink(tmpfile)
})

test_that("read_odf_header() works", {
  file <- bs_example("odf/CTD_98911_10P_11_DN.ODF")
  header <- read_odf_header(file)
  expect_true(all(c("PARAMETER_HEADER", "ODF_HEADER") %in% names(header)))
})

test_that("read_odf_colmeta() works", {
  file <- bs_example("odf/CTD_98911_10P_11_DN.ODF")
  colmeta <- read_odf_colmeta(file)
  expect_is(colmeta, "tbl_df")
})

test_that("read_odf() works", {
  file <- bs_example("odf/CTD_98911_10P_11_DN.ODF")
  tbl <- read_odf(file)
  expect_identical(
    colnames(tbl),
    c("PRES", "TEMP", "COND", "PSAL", "POTM", "SIGP")
  )
})
