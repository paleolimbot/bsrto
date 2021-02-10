
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
