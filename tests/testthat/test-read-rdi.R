
test_that("read_rdi_meta_single() aligns with results from oce::read.adp.rdi()", {
  file <- bs_example("rdi/19101018.rdi")
  # debug(oce:::decodeHeaderRDI)
  # oce_rdi <- oce::read.adp.rdi(file)

  rdi <- read_rdi_meta_single(file)

  # pick values towards the end of the structs
  # that are likely to be misaligned if any error occurred
  expect_identical(
    rdi$header$data_offset[[1]],
    # dput(oce_rdi@metadata$dataOffset)
    c(20L, 79L, 144L, 346L, 448L, 550L, 652L)
  )

  expect_identical(
    rdi$fixed_leader$cpu_board_serial_number[[1]],
    # dput(oce_rdi@metadata$cpuBoardSerialNumber)
    c(58L, 0L, 0L, 2L, 128L, 134L, 1L, 9L)
  )

  expect_identical(
    readBin(as.raw(rdi$fixed_leader$serial_number[[1]]), "integer"),
    # dput(oce_rdi@metadata$serialNumber)
    9088L
  )

  expect_identical(
    rdi$variable_leader$tranducer_depth,
    # dput(oce_rdi@metadata$transducerDepth)
    613L
  )

  expect_identical(
    rdi$variable_leader$contamination_sensor,
    159L
  )

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
})
