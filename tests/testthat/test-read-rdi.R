
test_that("read_rdi() works", {
  file <- bs_example("rdi/19101018.rdi")
  oce_rdi <- oce::read.adp.rdi(file)
  read_rdi(file)



})
