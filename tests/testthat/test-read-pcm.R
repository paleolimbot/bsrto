
test_that("read_pcm() works", {
  file <- bs_example("pcm/19101018.pcm")
  pcm <- read_pcm(file)
  expect_true(all(pcm$checksum_valid))
  ref_datetime <- readr::parse_datetime(
    "2019-10-10 18:01:28",
    locale = readr::locale(tz = "UTC")
  )
  expect_true(all(pcm$last_date_time == ref_datetime))
  expect_identical(read_pcm_vector(file)[-1], read_pcm(file))
  expect_true(
    all(c("last_date_time", "true_heading") %in%
          colnames(read_pcm_vector(character())))
  )
})

test_that("read_pcm() works for all files in the cache", {
  skip_if_not(bs_has_full_cache())

  # >27,000 files
  files <- bs_ftp_snapshot_latest$file[grepl("\\.pcm", bs_ftp_snapshot_latest$file)]
  cached <- bs_cache_dir(head(files, 5000))
  cached <- cached[file.exists(cached)]

  all <- read_pcm_vector(cached[1:500])
  expect_true(all(!is.na(all$last_date_time)))
  expect_true(all(all$true_heading >= 0 & all$true_heading <= 360))
  expect_true(all(all$checksum_valid))
})
