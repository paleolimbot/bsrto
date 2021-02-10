
test_that("read_ips_bn() works", {
  bn_file <- bs_example("ips/191010AA.bn1")

  bn <- read_ips_bn(bn_file)
  expect_true(all(bn$station_id == "BSRTO 51061"))
  expect_identical(colnames(bn), colnames(read_ips_bn_vector(character())))
})
