
test_that("read_icl() works", {
  file <- bs_example("icl/SAF2564_20191010_19.txt")
  icl <- read_icl(file)
  expect_identical(
    names(icl)[1:7],
    c("Time", "Comment", "Temperature [C]", "Humidity [%]",
      "Sequence #", "Data Points", "0")
  )

  # read_icl() carries spec attribute from readr
  expect_equivalent(
    read_icl(file),
    read_icl_vector(file)[-1]
  )
})

test_that("read_icl_header() works", {
  file <- bs_example("icl/SAF2564_20191010_19.txt")
  header <- read_icl_header(file)
  expect_identical(names(header), c("File Details", "Device Details", "Setup"))
  for (item in names(header)) {
    expect_is(header[[!! item]], "tbl_df")
    expect_identical(names(header[[!! item]]), c("key", "value"))
  }
})

test_that("read_icl_header_lines() works", {
  file <- bs_example("icl/SAF2564_20191010_19.txt")
  expect_length(read_icl_header_lines(file), 28)
})
