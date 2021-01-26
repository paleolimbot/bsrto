
# https://github.com/dankelley/oce/blob/9a2327894ad7924ecc6045207bb68c1f523f7715/R/adp.rdi.R
# https://github.com/dankelley/oce/blob/9a2327894ad7924ecc6045207bb68c1f523f7715/src/ldc_rdi_in_file.cpp
# https://github.com/richardsc/bsrto/blob/master/adp.R

read_rdi_meta_single <- function(file) {
  rdi <- .Call(bsrto_c_read_rdi, file)
  lapply(rdi, tibble::new_tibble, nrow = 1)
}
