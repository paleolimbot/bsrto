
test_that("pressure correction works", {
  expect_equal(barometric_pressure_from_sea_level(101300, 0), 101300)
  expect_equal(sea_level_pressure_from_barometric(101300, 0), 101300)

  expect_equal(
    round(
      barometric_pressure_from_sea_level(101300, 100) - barometric_pressure_from_sea_level(101300, 0),
      -2
    ),
    1200
  )

  expect_equal(
    round(
      sea_level_pressure_from_barometric(101300, 0) - sea_level_pressure_from_barometric(101300, 100),
      -2
    ),
    1200
  )
})
