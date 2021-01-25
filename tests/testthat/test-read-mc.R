
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

test_that("read_mc() works for all mcA files in the cache", {
  skip_if_not(bs_has_cache())

  # >15,000 files
  files <- bs_ftp_snapshot_latest$file[grepl("\\.mcA$", bs_ftp_snapshot_latest$file)]
  cached <- bs_cache(files)
  cached <- cached[file.exists(cached)]

  all <- read_mc_vector(cached[1:1000])
  expect_true(all(all$temperature > -2 & all$temperature < 5, na.rm = TRUE))
})

test_that("read_mc() works for all mcI files in the cache", {
  skip_if_not(bs_has_cache())

  # >1,300 files
  files <- bs_ftp_snapshot_latest$file[grepl("\\.mcI$", bs_ftp_snapshot_latest$file)]
  cached <- bs_cache(files)
  exists <- file.exists(cached)
  files <- files[exists]
  cached <- cached[exists]

  # different directories have slightly different mcI file structures
  no_header_dirs <- c(
    "barrow/2017/bsrto/node/IPS/data/sbe",
    "barrow/2017/bsrto/real-time/mcI"
  )
  mci_no_header <- cached[dirname(files) %in% no_header_dirs]
  no_header <- read_mc_vector(mci_no_header)
  expect_true(all(no_header$temperature > -2 & no_header$temperature < 6, na.rm = TRUE))

  with_header <- setdiff(cached, mci_no_header)
  # this file is tested above
  with_header <- with_header[basename(with_header) != "20010323.mcI"]

  all <- read_mc_vector(with_header)
  expect_true(all(all$temperature > -2 & all$temperature < 5, na.rm = TRUE))

  all_meta <- read_mci_meta_vector(with_header)
  expect_true(all(!is.na(all_meta$`start time`[file.size(with_header) > 0])))
})

test_that("read_mc() works for all mcH files in the cache", {
  skip_if_not(bs_has_cache())

  # >34,000 files
  files <- bs_ftp_snapshot_latest$file[grepl("\\.mcH", bs_ftp_snapshot_latest$file)]
  cached <- bs_cache(head(files, 5000))
  cached <- cached[file.exists(cached)]

  all <- read_mc_vector(cached[1:1000])
  expect_true(all(all$temperature > -2 & all$temperature < 0, na.rm = TRUE))
})
