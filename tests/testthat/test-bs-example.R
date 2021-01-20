
test_that("bs_example() works", {
  expect_identical(
    bs_example("odf/CTD_98911_10P_11_DN.ODF"),
    system.file("ex", "odf/CTD_98911_10P_11_DN.ODF", package = "bsrto")
  )

  expect_error(bs_example("B.S."), "does not exist in package")
})
