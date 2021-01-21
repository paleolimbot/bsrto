
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
  pb <- bs_progress(file_vector)
  on.exit(bs_progress_finish(pb))
  results <- lapply(file_vector, read_ips_bn_single, pb = pb)
  vctrs::vec_rbind(!!! results, .ptype = ips_bn_empty())
}

read_ips_bn_single <- function(file, pb = NULL) {
  bs_tick(pb, file)
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

  parsed <- lapply(
    records,
    read_ips_bn_record, ips_bn_empty()[NA_integer_, ]
  )
  vctrs::vec_rbind(!!! parsed)
}

read_ips_bn_record <- function(record, t = ips_bn_empty()[NA_integer_, ]) {
  tryCatch({
    lines <- readr::read_lines(record)

    stopifnot(length(lines) == 7)

    # line 1
    s1 <- stringr::str_match(lines[1], "^([0-9]+)\\s+(.*)$")
    t$measurement_id <- s1[2]
    t$date_time <- readr::parse_datetime(
      s1[3],
      "%a %b %d %H:%M:%S %Y",
      locale = readr::locale(tz = "UTC")
    )

    # line 2
    t$station_id <- lines[2]

    s <- strsplit(lines[3:6], "\\s+")
    s <- lapply(s, function(x) suppressWarnings(as.numeric(x)))

    # line 3
    t$draft_max <- s[[c(1, 1)]]; t$draft_min <- s[[c(1, 2)]]
    t$draft_mean <- s[[c(1, 3)]]; t$draft_sd <- s[[c(1, 4)]]

    # line 4
    t$n_ranges <- s[[c(2, 1)]]; t$n_partial_ranges <- s[[c(2, 2)]]
    t$sound_speed <- s[[c(2, 3)]]; t$density <- s[[c(2, 4)]]
    t$gravity <- s[[c(2, 5)]]

    # line 5
    t$pressure_max <- s[[c(3, 1)]]; t$pressure_min <- s[[c(3, 2)]]
    t$temp_max <- s[[c(3, 3)]]; t$temp_min <- s[[c(3, 4)]]

    # line 6
    t$max_pitch <- s[[c(4, 1)]]; t$max_roll_pitch <- s[[c(4, 2)]]
    t$max_roll <- s[[c(4, 3)]]; t$max_pitch_roll <- s[[c(4, 4)]]
    t$max_inclination <- s[[c(4, 5)]]

    bins <- suppressWarnings(as.numeric(strsplit(lines[7], ",", fixed = TRUE)[[1]]))
    if (any(is.na(bins))) {
      abort(glue("Unexpected text in bins: '{ lines[7] }'"))
    }

    t$bins <- list(bins)
  }, error = function(e) {
    t$error <<- paste0(e, collapse = "\n")
  })

  t
}

ips_bn_empty <- function() {
  tibble::tibble(
    error = character(),
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

# sample entry:
# 1570903201 Sat Oct 12 18:00:01 2019
# BSRTO 51061
# 10.10 9.74 9.96 0.00
# 2399 0 1440.60 1025.94 9.8285
# 55.78 55.20 0.00 -1.58
# 0.00 0.00 1.56 0.71 1.56
# 0,0,0,0,0,0,0,2,163,1922,311,1,0,0,0,0,0
# <separated by blank lines>
