
test_that("read_imm() works", {
  imm_file <- bs_example("imm/18082902.imm")
  imm <- read_imm(imm_file)
  expect_identical(
    imm$date_time,
    readr::parse_datetime("2018-08-29 02:00:01", locale = readr::locale(tz = "UTC"))
  )
  expect_identical(
    read_imm(imm_file),
    read_imm_vector(imm_file)[-1]
  )
})

test_that("read_imm() works for all files in the cache", {
  skip_if_not(bs_has_full_cache())

  # >11,000 files
  files <- bs_ftp_snapshot_latest$file[grepl("\\.imm$", bs_ftp_snapshot_latest$file)]
  cached <- bs_cache_dir(files)
  cached <- cached[file.exists(cached)]

  all <- suppressWarnings(read_imm_vector(cached[1:1000]))
  expect_true(all(all$temperature > -2 & all$temperature < 2, na.rm = TRUE))
})

