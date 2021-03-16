
#' Read RDI files
#'
#' @inheritParams read_hpb
#' @param types The variable types to extract from the file.
#'   Defaults to all variable types found in the first file.
#'
#' @return A [tibble::tibble()]
#'
#' @export
#'
#' @examples
#' rdi_file <- bs_example("rdi/19101018.rdi")
#' read_rdi(rdi_file)
#' read_rdi_vector(rdi_file)
#'
read_rdi <- function(file, types = guess_rdi_types(file), tz = "UTC") {
  stopifnot(length(file) == 1)
  read_rdi_vector(file, types = types, tz = tz)[-1]
}

#' @rdname read_rdi
#' @export
read_rdi_vector <- function(file_vector, types = guess_rdi_types(file_vector[1]),
                            tz = "UTC") {
  pb <- bs_progress(file_vector)
  on.exit(bs_progress_finish(pb))

  results <- lapply(file_vector, read_rdi_single, pb = pb, types = types)
  lengths <- vapply(results, nrow, integer(1))
  results_all <- vctrs::vec_rbind(
    tibble::tibble(file = character(), real_time_clock = character()),
    !!! results
  )

  results_all$file <- vctrs::vec_rep_each(file_vector, lengths)

  results_all$real_time_clock <- readr::parse_datetime(
    results_all$real_time_clock,
    "%y-%m-%d %H:%M:%OS",
    locale = readr::locale(tz = tz)
  )

  # the raw() columns are confusing and are better expressed as integers
  is_raw <- vapply(results_all, is.raw, logical(1))
  results_all[is_raw] <- lapply(results_all[is_raw], as.integer)

  results_all
}

read_rdi_single <- function(file, types = NULL, pb = NULL) {
  bs_tick(pb, file)

  rdi <- try(read_rdi_internal(file))
  if (inherits(rdi, "try-error")) {
    return(tibble::tibble())
  }

  if (!is.null(types)) {
    rdi <- rdi[intersect(types, names(rdi))]
  }

  # get rid of 'magic number' columns
  rdi <- lapply(rdi, "[", -1)

  # flatten to a single row
  vctrs::vec_cbind(!!! unname(rdi))
}

#' @rdname read_rdi
#' @export
guess_rdi_types <- function(file) {
  setdiff(names(read_rdi_internal(file)), "header")
}

read_rdi_internal <- function(file, offset = 0L) {

  # not using readr for the base read, but for consistency, support
  # gzipped files and urls
  is_url <- stringr::str_detect(file, "^[a-z]{3,5}://")
  is_gz <- stringr::str_detect(file, "\\.gz$")
  if (is_url || is_gz) {
    out_file <- tempfile()
    close_con <- TRUE
    out_con <- file(out_file, open = "wb")
    on.exit({if (close_con) close(out_con); unlink(out_file)})
    writeBin(readr::read_file_raw(file), out_con)
    close(out_con)
    close_con <- FALSE
    file <- out_file
  }

  readrdi::read_rdi(
    file,
    index = data.frame(offset = 0),
    # readrdi::rdi_detect_data_types(rdi_file) %>% dput()
    types = c(
      header = 32639L,
      fixed_leader = 0L,
      variable_leader = 128L,
      velocity = 256L,
      correlation = 512L,
      echo_intensity = 768L,
      pct_good = 1024L,
      bottom_track = 1536L
    )
  )
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

  item$transducer_depth <- item$transducer_depth / 10.0

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

read_rdi_fix_bottom_track <- function(item) {
  item$bottom_track_velocity <- lapply(item$bottom_track_velocity, "/", 1000)
  item
}
