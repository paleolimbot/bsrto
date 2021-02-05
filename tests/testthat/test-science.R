
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

test_that("uv--heading conversion works", {
  headings <- 0:360

  expect_equal(
    heading_from_uv(uv_from_heading(headings)),
    c(headings[-length(headings)], 0)
  )

  expect_equal(
    uv_from_heading(0),
    tibble::tibble(u = 0, v = 1)
  )

  expect_equal(
    uv_from_heading(90),
    tibble::tibble(u = 1, v = 0)
  )

  expect_equal(
    uv_from_heading(180),
    tibble::tibble(u = 0, v = -1)
  )

  expect_equal(
    uv_from_heading(270),
    tibble::tibble(u = -1, v = 0)
  )
})

test_that("mean and sd of headings works", {
  expect_equal(heading_mean(0:10), 5)
  expect_equal(heading_mean(-5:5), 0)
  expect_equal(heading_mean(c(350, 10)), 0)
  expect_equal(heading_sd(-5:5), sd(0:10))

  expect_identical(heading_mean(c(1, 1, NA), na.rm = FALSE), NA_real_)
  expect_identical(heading_mean(c(1, 1, NA), na.rm = TRUE), 1)
  expect_identical(heading_sd(c(1, 1, NA), na.rm = FALSE), NA_real_)
  expect_identical(heading_sd(c(1, 1, NA), na.rm = TRUE), 0)
})
