
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
