
test_that("read_rdi() works", {
  file <- bs_example("rdi/19101018.rdi")

  rdi <- read_rdi(file)

  expect_identical(
    rdi$real_time_clock,
    readr::parse_datetime(
      "2019-10-10 18:00:03.08",
      locale = readr::locale(tz = "UTC")
    )
  )

  expect_identical(
    read_rdi(file),
    read_rdi_vector(file)[-1]
  )
})

test_that("read_rdi() works on gzipped files", {
  expect_identical(
    read_rdi(bs_example("rdi/19101018.rdi")),
    read_rdi(bs_example("rdi/19101018.rdi.gz"))
  )
})

test_that("read_rdi_internal() aligns with results from oce::read.adp.rdi()", {
  file <- bs_example("rdi/19101018.rdi")
  # debug(oce:::decodeHeaderRDI)
  # oce_rdi <- oce::read.adp.rdi(file)

  rdi <- read_rdi_internal(file, offset = 0)

  # pick values towards the end of the structs
  # that are likely to be misaligned if any error occurred

  # fixed leader

  expect_identical(
    rdi$header$data_offset[[1]],
    c(20L, 79L, 144L, 346L, 448L, 550L, 652L)
  )

  expect_identical(
    rdi$fixed_leader$cpu_board_serial_number[[1]],
    "3a.00.00.02.80.86.01.09"
  )

  expect_identical(
    rdi$fixed_leader$serial_number,
    9088L
  )

  # variable leader

  expect_identical(
    rdi$variable_leader$transducer_depth,
    61.3
  )

  expect_identical(
    rdi$variable_leader$contamination_sensor,
    as.raw(159L)
  )

  expect_identical(
    rdi$variable_leader$pressure,
    61.535
  )

  # bottom track

  expect_identical(
    rdi$bottom_track$bv[[1]] / 1000,
    c(-0.357, -0.279, 0.006, -0.001)
  )

  expect_identical(
    rdi$bottom_track$bc[[1]],
    c(254L, 254L, 255L, 254L)
  )

  expect_identical(
    rdi$bottom_track$ba[[1]],
    c(78L, 79L, 82L, 76L)
  )

  expect_identical(
    rdi$bottom_track$bg[[1]],
    c(0L, 0L, 0L, 100L)
  )

  expect_identical(
    rdi$bottom_track$range_lsb[[1]],
    c(6179L, 6082L, 6106L, 6130L)
  )

  expect_identical(
    rdi$bottom_track$range_msb[[1]],
    c(0L, 0L, 0L, 0L)
  )

  # velocity

  # (note transposed relative to oce_rdi@data$[v, q, g, ])
  expect_identical(
    rdi$velocity$velocity[[1]][, 25],
    c(-0.147, -0.039, 0.014, NA)
  )

  expect_identical(
    rdi$correlation$correlation[[1]][, 25],
    as.raw(c(75L, 56L, 50L, 73L))
  )

  expect_identical(
    rdi$echo_intensity$echo_intensity[[1]][, 25],
    as.raw(c(91L, 82L, 86L, 91L))
  )

  expect_identical(
    rdi$pct_good$pct_good[[1]][, 25],
    as.raw(c(18L, 0L, 81L, 0L))
  )
})

test_that("read_rdi_internal() errors when passed an invalid offset", {
  file <- bs_example("rdi/19101018.rdi")
  expect_error(
    read_rdi_internal(file, offset = 1),
    "Expected 0x7f7f"
  )
})
