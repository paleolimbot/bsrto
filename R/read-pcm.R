
#' Read PCM files
#'
#' @inheritParams read_lgh
#'
#' @return A [tibble::tibble()]
#' @export
#'
#' @examples
#' pcm_file <- bs_example("pcm/19101018.pcm")
#' read_pcm(pcm_file)
#' read_pcm_vector(pcm_file)
#'
#'
read_pcm <- function(file, tz = "UTC", pb = NULL) {
  bs_tick(pb, file)

  content <- readr::read_file(file)

  sentences <- stringr::str_match_all(
    content,
    "\\$IIHDT,([^,]+),T\\*([0-9A-Fa-f]{2})"
  )[[1]]


  # NMEA checksum is the bitwise xor of all characters
  # between $ and *, exclusive
  # this operation only takes ~5 ms
  checksum <- as.raw(paste0("0x", sentences[, 3]))
  data_checksum <- vapply(paste0("IIHDT,", sentences[, 2], ",T"), function(x) {
    bytes <- charToRaw(x)
    bits <- matrix(as.integer(rawToBits(bytes)), nrow = 8)
    packBits(as.integer(rowSums(bits) %% 2), "raw")
  }, raw(1), USE.NAMES = FALSE)
  checksum_valid <- checksum == data_checksum

  tibble::tibble(
    last_date_time = readr::parse_datetime(
      readr::read_lines(content, n_max = 1)[1],
      "%m/%d/%Y %H:%M:%S",
      locale = readr::locale(tz = tz)
    ),
    true_heading = readr::parse_double(sentences[, 2]),
    checksum_valid = checksum_valid
  )
}

#' @rdname read_pcm
#' @export
read_pcm_vector <- function(file_vector, tz = "UTC") {
  pb <- bs_progress(file_vector)
  on.exit(bs_progress_finish(pb))

  results <- lapply(file_vector, read_pcm, pb = pb, tz = tz)
  lengths <- vapply(results, nrow, integer(1))
  results_all <- vctrs::vec_rbind(
    tibble::tibble(
      last_date_time = as.POSIXct(character()),
      true_heading = double(),
      checksum_valid = logical()
    ),
    !!! results
  )

  vctrs::vec_cbind(
    file = vctrs::vec_rep_each(file_vector, lengths),
    results_all
  )
}
