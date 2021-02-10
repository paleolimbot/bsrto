
test_that("read_mc() works for mcA files", {
  mca_file <- bs_example("mcA/19101018.mcA")
  mca <- read_mc(mca_file)
  expect_identical(
    mca$date_time,
    readr::parse_datetime("2019-10-10 18:00:11", locale = readr::locale(tz = "UTC"))
  )
  expect_identical(
    read_mc(mca_file),
    read_mc_vector(mca_file)[-1]
  )
  expect_true("date_time" %in% colnames(read_mc_vector(character())))
})

test_that("read_mc() works for mcH files", {
  mc_file <- bs_example("mcH/19101019.mcH")
  mc <- read_mc(mc_file)
  expect_identical(
    mc$date_time,
    readr::parse_datetime("2019-10-10 18:00:01", locale = readr::locale(tz = "UTC"))
  )
  expect_identical(
    read_mc(mc_file),
    read_mc_vector(mc_file)[-1]
  )
  expect_true("date_time" %in% colnames(read_mc_vector(character())))
})

test_that("read_mc() works for mcI files", {
  mc_file <- bs_example("mcI/19101023.mcI")
  mc <- read_mc(mc_file)
  expect_identical(
    mc$date_time[1],
    readr::parse_datetime("2019-10-10 00:00:10", locale = readr::locale(tz = "UTC"))
  )
  expect_identical(
    read_mc(mc_file),
    read_mc_vector(mc_file)[-1]
  )
  expect_true("date_time" %in% colnames(read_mc_vector(character())))

  # malformed file should not error
  expect_warning(
    read_mc(bs_example("mcI/20010323.mcI"), col_names = mci_col_names(2018)),
    "parsing failure"
  )
})

test_that("read_mci_meta() works", {
  mc_file <- bs_example("mcI/19101023.mcI")
  mc <- read_mci_meta(mc_file)
  expect_identical(
    mc$`start time`[1],
    readr::parse_datetime("2019-10-10 00:00:10", locale = readr::locale(tz = "UTC"))
  )
  expect_identical(
    read_mci_meta(mc_file),
    read_mci_meta_vector(mc_file)[-1]
  )
  expect_true("start time" %in% colnames(read_mci_meta_vector(character())))
})
