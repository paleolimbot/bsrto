
test_that("read_hpb() works", {
  hpb_files <- list.files(bs_example("hpb"), "\\.hpb$", full.names = TRUE)
  expect_identical(
    names(read_hpb(hpb_files)),
    c("date", "time", "atm_pres_mbar", "temp_c")
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

