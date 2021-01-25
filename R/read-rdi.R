
# https://github.com/dankelley/oce/blob/9a2327894ad7924ecc6045207bb68c1f523f7715/R/adp.rdi.R
# https://github.com/richardsc/bsrto/blob/master/adp.R


read_rdi <- function(file) {
  .Call(bsrto_c_read_rdi, file)
}
