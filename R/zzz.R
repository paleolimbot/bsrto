
bs_temp_dir_internal <- NULL

.onLoad <- function(...) {
  bs_temp_dir_internal <<- tempfile()
  dir.create(bs_temp_dir_internal)
}

.onUnload <- function(...) {
  unlink(bs_temp_dir_internal, recursive = TRUE)
}
