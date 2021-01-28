


# Currently the C code just reads one ensemble at a time and reads
# everything. All the BSRTO files are a single ensemble when uploaded
# so it works well here. Realistically there should be an 'rdi' or
# 'adcp' package that reads these files. This was written using the
# explicit and implicit documentation provided by oce::read.adp.rdi()
# by Daniel Kelley.
# https://github.com/dankelley/oce/blob/develop/R/adp.rdi.R
# https://github.com/dankelley/oce/blob/develop/src/ldc_rdi_in_file.cpp
read_rdi_internal <- function(file, offset = 0L) {
  rdi <- .Call(bsrto_c_read_rdi, file, as.integer(offset)[1])

  # Should really be done in C if this starts to limit speed
  is_fixed_leader <- names(rdi) == "fixed_leader"
  rdi[is_fixed_leader] <- lapply(rdi[is_fixed_leader], read_rdi_fix_fixed_leader)
  is_variable_leader <- names(rdi) == "variable_leader"
  rdi[is_variable_leader] <- lapply(rdi[is_variable_leader], read_rdi_fix_variable_leader)
  is_bottom_track <- names(rdi) == "bottom_track"
  rdi[is_bottom_track] <- lapply(rdi[is_bottom_track], read_rdi_fix_bottom_track)

  lapply(rdi, tibble::new_tibble, nrow = 1)
}

read_rdi_fix_fixed_leader <- function(item) {
  item$serial_number <- readBin(
    as.raw(item$serial_number[[1]]),
    "integer", n = 1, size = 4, endian = "little", signed = TRUE
  )
  item$firmware_version <-
    as.numeric(paste0(item$firmware_version[[1]], collapse = "."))
  item$cpu_board_serial_number <-
    paste0(as.raw(item$cpu_board_serial_number[[1]]), collapse = ".")

  # interpretation of system config is device dependent, but this can be stored
  # as an integer and interpreted with bit masking if needed (not implemented)
  item$system_config <- readBin(
    as.raw(item$system_config[[1]]),
    "integer", n = 1, size = 2, endian = "big", signed = FALSE
  )

  item
}

read_rdi_fix_variable_leader <- function(item) {
  rtc <- item$real_time_clock[[1]]
  # I think parsing is better left to a higher level of abstraction
  item$real_time_clock <- sprintf(
    # "%y-%m-%d %H:%M:%OS"
    "%02d-%02d-%02d %02d:%02d:%02d.%02d",
    rtc[1], rtc[2], rtc[3], rtc[4], rtc[5], rtc[6], rtc[7]
  )

  item$pressure <- readBin(
    as.raw(item$pressure[[1]]),
    "integer", n = 1, size = 4, endian = "little", signed = TRUE
  ) / 1000.0

  item$pressure_std <- readBin(
    as.raw(item$pressure_std[[1]]),
    "integer", n = 1, size = 4, endian = "little", signed = TRUE
  ) / 1000.0

  item
}
