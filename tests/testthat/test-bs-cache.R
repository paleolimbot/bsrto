
test_that("bs_cache_dir() works", {
  expect_identical(
    withr::with_options(list(bsrto.cache = "foo"), bs_cache_dir()),
    "foo"
  )

  expect_identical(
    withr::with_envvar(list(R_BSRTO_CACHE = "foo"), bs_cache_dir()),
    "foo"
  )
})

test_that("bs_build_cache_dir() works", {
  expect_identical(
    withr::with_options(list(bsrto.build_cache = "foo"), bs_build_cache_dir()),
    "foo"
  )

  expect_identical(
    withr::with_envvar(list(R_BSRTO_BUILD_CACHE = "foo"), bs_build_cache_dir()),
    "foo"
  )
})

test_that("bs_build_cache_dir() works", {
  expect_identical(
    withr::with_options(list(bsrto.ftp_server = "foo"), bs_ftp_server()),
    "foo"
  )

  expect_identical(
    withr::with_envvar(list(R_BSRTO_FTP_SERVER = "foo"), bs_ftp_server()),
    "foo"
  )
})
