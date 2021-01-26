
# https://github.com/dankelley/oce/blob/9a2327894ad7924ecc6045207bb68c1f523f7715/R/adp.rdi.R
# https://github.com/richardsc/bsrto/blob/master/adp.R


read_rdi <- function(file) {
  rdi <- .Call(bsrto_c_read_rdi, file)
  rdi <- rdi[!vapply(rdi, is.null, logical(1))]
  structure(
    lapply(rdi, tibble::new_tibble, nrow = 1),
    class = "bsrto_rdi"
  )
}
