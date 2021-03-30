
test_that("pressure correction works", {
  expect_equal(barometric_pressure_from_sea_level(101.3, 0), 101.3)
  expect_equal(sea_level_pressure_from_barometric(101.3, 0), 101.3)

  expect_equal(
    round(
      barometric_pressure_from_sea_level(101.3, 100) - barometric_pressure_from_sea_level(101.3, 0),
      1
    ),
    1.2
  )

  expect_equal(
    round(
      sea_level_pressure_from_barometric(101.3, 0) - sea_level_pressure_from_barometric(101.3, 100),
      1
    ),
    1.2
  )
})

test_that("barrow_strait_declination() is a reasonable approximation", {
  lon <- -91.25105
  lat <- 74.60635
  date_time <- seq(
    as.POSIXct("2019-01-01 00:00:00", tz = "UTC"),
    as.POSIXct("2024-12-31 11:59:00", tz = "UTC"),
    by = "year"
  )
  # dec_model <- oce::magneticField(
  #   rep(lon, length(date_time)),
  #   rep(lat, length(date_time)),
  #   date_time
  # )$declination
  dec_model <- c(
    -26.5661318453884, -25.5111607524511, -24.4518662848595,
    -23.4294772283977, -22.4427159686452, -21.4903077363711
  )
  dec_approx <- barrow_strait_declination(date_time)

  expect_true(
    max(abs(dec_model - dec_approx)) < 0.001
  )
})

test_that("salinity calculator gives reasonable results", {
  expect_identical(
    round(salinity_from_cond_temp_pres(2.57867, -1.0966, 45.181), 3),
    round(31.8174, 3)
  )

  # internally the instruments must be using the unesco backend and not
  # gsw (hence the reason these only agree to the nearest m/s)
  expect_identical(
    round(sound_speed_from_psal_temp_pres(31.8174, -1.0966, 45.181), 0),
    round(1440.467, 0)
  )
})


test_that("2d rotator works", {
  coords <- rbind(
    c(0, 0),
    c(0, 1),
    c(1, 0),
    c(sqrt(2) / 2, sqrt(2) / 2)
  )

  expect_identical(rotate_about_origin(coords, 0), coords)
  expect_equal(
    rotate_about_origin(coords, 90),
    rbind(
      c(0, 0),
      c(1, 0),
      c(0, -1),
      c(sqrt(2) / 2, -sqrt(2) / 2)
    )
  )

  expect_equal(
    rotate_about_origin(coords, -45),
    rbind(
      c(0, 0),
      c(-sqrt(2) / 2, sqrt(2) / 2),
      c(sqrt(2) / 2, sqrt(2) / 2),
      c(0, 1)
    )
  )
})

test_that("2d rotator is vectorized along rotation", {
  coords <- rbind(
    c(0, 0),
    c(0, 1),
    c(1, 0),
    c(sqrt(2) / 2, sqrt(2) / 2)
  )

  expect_equal(
    rotate_about_origin(coords, c(45, 90, 135, 180)),
    rbind(
      c(0, 0),
      c(1, 0),
      c(-sqrt(2) / 2, -sqrt(2) / 2),
      c(-sqrt(2) / 2, -sqrt(2) / 2)
    )
  )
})
