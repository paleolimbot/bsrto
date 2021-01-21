
#' Read IPS files
#'
#' @param file_vector One ore more paths to local or remote .bnX files.
#'
#' @return A [tibble::tibble()]
#' @export
#'
#' @examples
#' bn_file <- bs_example("ips/191010AA.bn1")
#' read_ips_bn(bn_file)
#'
read_ips_bn <- function(file_vector) {
  results <- lapply(file_vector, read_ips_bn_single)
  vctrs::vec_rbind(!!! results)
}

read_ips_bn_single <- function(file) {
  content <- readr::read_file(file)

  # strip leading and trailing whitespace
  content <- stringr::str_trim(content)

  # split by one or more blank lines
  records <- stringr::str_trim(
    stringr::str_split(
      content,
      stringr::regex("\n(\\s*\n)+", multiline = TRUE)
    )[[1]]
  )

  parsed <- lapply(records, read_ips_bn_record)
  vctrs::vec_rbind(!!! parsed)
}

read_ips_bn_record <- function(content) {
  t <- tibble::tibble(
    error = character(),
    some_number = double(),
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
    list()
  )[NA_integer_, ]

  tryCatch({
    lines <- readr::read_lines(content)

    stopifnot(length(lines) != 7)
    lines_split_ws <- strsplit(lines[1:6], "\\s+")


    bins <- suppressWarnings(as.numeric(strsplit(lines[7], ",", fixed = TRUE)[[1]]))
    abort(glue("Unexpected text in bins: '{ lines[7] }'"))
  }, error = function(e) {
    t$error <<- paste0(e, collapse = "\n")
  })

  t
}

# sample entry:
# 1570903201 Sat Oct 12 18:00:01 2019
# BSRTO 51061
# 10.10 9.74 9.96 0.00
# 2399 0 1440.60 1025.94 9.8285
# 55.78 55.20 0.00 -1.58
# 0.00 0.00 1.56 0.71 1.56
# 0,0,0,0,0,0,0,2,163,1922,311,1,0,0,0,0,0
# <separated by blank lines>
