
test_that("read_ec_climate_hourly() works", {
  file <- bs_example("met/ec-climate_hourly_54199_2019_08.csv")
  ec <- read_ec_climate_hourly(file, utc_offset = -6)
  expect_true(all(!is.na(ec$date_time)))
  expect_identical(
    ec$date_time[1],
    readr::parse_datetime("2019-08-01 06:00:00", locale = readr::locale(tz = "UTC"))
  )

  # placement of date_time column is different but data is the same
  expect_identical(
    read_ec_climate_hourly(file)[-(1:5)],
    read_ec_climate_hourly_vector(file)[-(1:6)]
  )
})

test_that("ec_download_summary_hourly() works", {
  summary_2020 <- ec_download_summary_hourly(1234, "2020-01-01", "2020-12-31")
  expect_equal(summary_2020$month, 1:12)
})

test_that("ec_bulk_data_url() works", {
  expect_match(ec_bulk_data_url("monthly", 1234), "stationID=1234")
  expect_match(ec_bulk_data_url("daily", 1234, Year = 5678), "stationID=1234")
  expect_match(
    ec_bulk_data_url("hourly", 1234, Year = 5678, Month = 90),
    "stationID=1234"
  )
  expect_error(ec_bulk_data_url("fish"), "must be one of")
})

test_that("ec_bulk_data_dest() works", {
  expect_equal(
    ec_bulk_data_dest("monthly", 1234),
    "ec-climate_monthly_1234.csv"
  )

  expect_equal(
    ec_bulk_data_dest("daily", 1234, 5678),
    "ec-climate_daily_1234_5678.csv"
  )

  expect_equal(
    ec_bulk_data_dest("hourly", 1234, 5678, 90),
    "ec-climate_hourly_1234_5678_90.csv"
  )

  expect_error(ec_bulk_data_dest("fish"), "must be one of")
})
