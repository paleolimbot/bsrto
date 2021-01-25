
test_that("read_igh() works", {
  file <- bs_example("lgh/20191010.lgH")

  lgh <- read_lgh(file)
  expect_true(all(!is.na(lgh$date_time)))
  expect_true(all(vapply(lgh$log_text, is.character, logical(1))))

  expect_identical(lgh, read_lgh_vector(file)[-1])
  expect_identical(
    colnames(read_lgh_vector(character())),
    c("file", "date_time", "log_text")
  )
})

test_that("read_igh() works for all files in the cache", {
  skip_if_not(bs_has_cache())

  # >2,000 files
  files <- bs_ftp_snapshot_latest$file[grepl("\\.lgH", bs_ftp_snapshot_latest$file)]
  cached <- bs_cache(files)
  cached <- cached[file.exists(cached)]

  all <- read_lgh_vector(cached[1:100])
  expect_true(all(!is.na(all$date_time)))
})
