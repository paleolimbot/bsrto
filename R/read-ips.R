
#' Read IPS files
#'
#' @inheritParams read_icl
#' @inheritParams read_hpb
#'
#' @return A [tibble::tibble()]
#' @export
#'
#' @examples
#' bn_file <- bs_example("ips/191010AA.bn1")
#' read_ips_bn(bn_file)
#' read_ips_bn_vector(bn_file)
#'
read_ips_bn <- function(file, tz = "UTC") {
  stopifnot(length(file) == 1)
  read_ips_bn_vector(file, tz = tz)[-1]
}

#' @rdname read_ips_bn
#' @export
read_ips_bn_vector <- function(file_vector, tz = "UTC") {
  empty <- ips_bn_empty()

  if (length(file_vector) == 0) {
    return(empty)
  }

  pb <- bs_progress(file_vector)
  on.exit(bs_progress_finish(pb))

  results <- lapply(
    file_vector,
    read_ips_bn_single,
    regex = ips_bn_entry_regex(),
    pb = pb
  )
  lengths <- vapply(results, nrow, integer(1))

  results_all <- do.call(rbind, results)
  colnames(results_all) <- colnames(empty)
  results_all <- tibble::as_tibble(results_all)

  results_all$date_time <- readr::parse_datetime(
    results_all$date_time,
    "%a %b %d %H:%M:%S %Y",
    locale = readr::locale(tz = tz)
  )
  results_all[4:21] <- lapply(results_all[4:21], readr::parse_double)
  results_all$bins <- strsplit(results_all$bins, ",", fixed = TRUE)
  results_all$bins <- lapply(results_all$bins, readr::parse_double)

  vctrs::vec_cbind(
    file = vctrs::vec_rep_each(file_vector, lengths),
    results_all
  )
}

read_ips_bn_single <- function(file, regex = ips_bn_entry_regex(), pb = NULL) {
  bs_tick(pb, file)
  content <- readr::read_file(file)
  stringr::str_match_all(content, regex)[[1]][, -1, drop = FALSE]
}

ips_bn_empty <- function() {
  tibble::tibble(
    measurement_id = character(),
    date_time = as.POSIXct(character()),
    # line 2
    station_id = character(),
    # line 3
    draft_max = double(),
    draft_min = double(),
    draft_mean = double(),
    draft_sd = double(),
    # line 4
    n_ranges = double(),
    n_partial_ranges = double(),
    sound_speed = double(),
    density = double(), # supposed to be pressure according to docs but more likely density
    gravity = double(),
    # line 5
    pressure_max = double(),
    pressure_min = double(),
    temp_max = double(),
    temp_min = double(),
    # line 6
    max_pitch = double(),
    max_roll_pitch = double(),
    max_roll = double(),
    max_pitch_roll = double(),
    max_inclination = double(),
    # line 7 (histogram)
    bins = list()
  )
}

# using a regex here is ~100x faster than parsing each record line by line
# sample entry:
# 1570903201 Sat Oct 12 18:00:01 2019
# BSRTO 51061
# 10.10 9.74 9.96 0.00
# 2399 0 1440.60 1025.94 9.8285
# 55.78 55.20 0.00 -1.58
# 0.00 0.00 1.56 0.71 1.56
# 0,0,0,0,0,0,0,2,163,1922,311,1,0,0,0,0,0
#
ips_bn_entry_regex <- function() {
  nl <- "\r?\n"
  ws <- "\\s+"
  int <- "[0-9]+"
  dbl <- "[0-9.eE-]+"
  whatever <- ".*?"

  raw_regex <- glue("
    ({int}) ({whatever})
    ({whatever})
    ({dbl}) ({dbl}) ({dbl}) ({dbl})
    ({dbl}) ({dbl}) ({dbl}) ({dbl}) ({dbl})
    ({dbl}) ({dbl}) ({dbl}) ({dbl})
    ({dbl}) ({dbl}) ({dbl}) ({dbl}) ({dbl})
    ([0-9,]+)
  ")

  esc <- function(x) gsub("\\\\", "\\\\\\\\", x)
  compiled_regex <- stringr::str_trim(raw_regex)
  compiled_regex <- gsub("\r?\n\\s+", esc(nl), compiled_regex)
  compiled_regex <- gsub(" ", esc(ws), compiled_regex)
  compiled_regex
}
