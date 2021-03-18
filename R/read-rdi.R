
#' Read RDI files
#'
#' @inheritParams read_hpb
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
read_rdi <- function(file, tz = "UTC") {
  stopifnot(length(file) == 1)
  read_rdi_vector(file, tz = tz)[-1]
}

#' @rdname read_rdi
#' @export
read_rdi_vector <- function(file_vector, tz = "UTC") {
  pb <- bs_progress(file_vector)
  on.exit(bs_progress_finish(pb))

  results <- lapply(file_vector, read_rdi_single, pb = pb)
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

read_rdi_single <- function(file, pb = NULL) {
  bs_tick(pb, file)

  rdi <- try(read_rdi_internal(file))
  if (inherits(rdi, "try-error")) {
    return(tibble::tibble())
  }

  # get rid of 'magic number' columns
  rdi <- lapply(rdi, function(x) x[setdiff(names(x), "magic_number")])

  # flatten to a single row
  vctrs::vec_cbind(!!! unname(rdi))
}

read_rdi_internal <- function(file) {

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
