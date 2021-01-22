
test_that("read_hpb() works", {
  hpb_files <- list.files(bs_example("hpb"), "\\.hpb$", full.names = TRUE)
  hpb <- read_hpb(hpb_files, tz = "UTC")
  expect_identical(
    names(hpb),
    c("date_time", "atm_pres_mbar", "temp_c")
  )

  expect_identical(
    hpb$date_time,
    readr::parse_datetime("2019-08-28 20:30:37", locale = readr::locale(tz = "UTC"))
  )
})

test_that("read_hpb() works for all files in the cache", {
  skip_if_not(bs_has_cache())

  # >17,000 files
  files <- bs_ftp_snapshot_latest$file[grepl("\\.hpb$", bs_ftp_snapshot_latest$file)]
  cached <- bs_cache(files)
  cached <- cached[file.exists(cached)]

  all <- read_hpb(cached[1:300])
  expect_true(all(!is.na(all)))
})

