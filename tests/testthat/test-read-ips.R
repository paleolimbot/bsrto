
test_that("read_ips_bn() works", {
  bn_file <- bs_example("ips/191010AA.bn1")

  bn <- read_ips_bn(bn_file)
  expect_true(all(is.na(bn$error)))
  expect_true(all(bn$station_id == "BSRTO 51061"))
  expect_identical(colnames(bn), colnames(read_ips_bn(character())))
})

test_that("read_ips_bn() works for all files in the cache", {
  skip_if_not(bs_has_cache())

  # about 4,000 files, only use those that exist
  bn_files <- bs_ftp_snapshot_latest$file[grepl("\\.bn[0-9]$", bs_ftp_snapshot_latest$file)]
  bn_cached <- bs_cache(bn_files)
  bn_cached <- bn_cached[file.exists(bn_cached)]

  bn_all <- read_ips_bn(bn_cached[1:300])
  expect_true(all(is.na(bn_all$error)))
})
