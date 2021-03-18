
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

  sentences <- nmea::read_nmea(file, sentence_end = "\r\n")
  parsed <- nmea::nmea_extract(
    sentences,
    spec = nmea::nmea_spec(heading = nmea::nmea_col_double())
  )

  # date is at start of file
  date_line <- readr::read_lines(file, n_max = 1L)

  tibble::tibble(
    last_date_time = readr::parse_datetime(
      date_line,
      "%m/%d/%Y %H:%M:%S",
      locale = readr::locale(tz = tz)
    ),
    heading_magnetic = parsed$heading,
    checksum_valid = parsed$checksum_valid
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
      heading_magnetic = double(),
      checksum_valid = logical()
    ),
    !!! results
  )

  vctrs::vec_cbind(
    file = vctrs::vec_rep_each(file_vector, lengths),
    results_all
  )
}
