
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
    rdi$fixed_leader_data$cpu_board_serial_number[[1]],
    # dput(oce_rdi@metadata$cpuBoardSerialNumber)
    c(58L, 0L, 0L, 2L, 128L, 134L, 1L, 9L)
  )

  expect_identical(
    readBin(as.raw(rdi$fixed_leader_data$serial_number[[1]]), "integer"),
    # dput(oce_rdi@metadata$serialNumber)
    9088L
  )

  expect_identical(
    rdi$variable_leader_data$tranducer_depth,
    # dput(oce_rdi@metadata$transducerDepth)
    613L
  )
})
