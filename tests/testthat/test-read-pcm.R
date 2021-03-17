
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
    all(c("last_date_time", "heading_magnetic") %in%
          colnames(read_pcm_vector(character())))
  )
})

test_that("read_pcm() works for mangled files", {
  file <- bs_example("pcm/20022402.pcm")
  # depending on the locale this can spit out more than one warning,
  # so it's difficult to use expect_warning()
  expect_true(suppressWarnings(all(!is.na(read_pcm(file)$last_date_time))))
})
