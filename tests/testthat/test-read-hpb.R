
test_that("read_hpb() works", {
  hpb_files <- list.files(bs_example("hpb"), "\\.hpb$", full.names = TRUE)
  expect_identical(
    names(read_hpb(hpb_files)),
    c("date", "time", "atm_pres_mbar", "temp_c")
  )
})
