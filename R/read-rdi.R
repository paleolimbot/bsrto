
# https://github.com/dankelley/oce/blob/9a2327894ad7924ecc6045207bb68c1f523f7715/R/adp.rdi.R
# https://github.com/dankelley/oce/blob/9a2327894ad7924ecc6045207bb68c1f523f7715/src/ldc_rdi_in_file.cpp
# https://github.com/richardsc/bsrto/blob/master/adp.R


# TODO: several issues with current approach, notably that only files
# with one profile work (most of our files for bsrto). Also,
# velocity and other actual data is not yet loaded. Ideally this
# will live in another package some day with more files on which to test.

read_rdi_meta_single <- function(file, read_offsets = FALSE) {
  rdi <- .Call(bsrto_c_read_rdi, file, as.logical(read_offsets)[1])
  lapply(rdi, tibble::new_tibble, nrow = 1)
}
