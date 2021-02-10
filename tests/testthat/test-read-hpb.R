
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

  expect_identical(
    read_hpb(hpb_files),
    read_hpb_vector(hpb_files)[-1]
  )
})
