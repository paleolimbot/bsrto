
#' Read mcA, mcI, and mcH files
#'
#' @inheritParams read_imm
#' @param year The year to use to specify column names in the range
#'   2017-2019.
#'
#' @return A [tibble::tibble()]
#' @export
#'
#' @examples
#' mca_file <- bs_example("mcA/19101018.mcA")
#' read_mc(mca_file)
#' read_mc_vector(mca_file)
#'
read_mc <- function(file, col_names = guess_mc_col_names(file),
                    skip = guess_mc_skip(file), tz = "UTC") {
  stopifnot(length(file) == 1)
  read_mc_vector(file, col_names = col_names, skip = skip, tz = tz)[-1]
}

#' @rdname read_mc
#' @export
read_mc_vector <- function(file_vector,
                           col_names = guess_mc_col_names(file_vector[1]),
                           skip = guess_mc_skip(file_vector[1]),
                           tz = "UTC") {
  pb <- bs_progress(file_vector)
  on.exit(bs_progress_finish(pb))

  results <- lapply(
    file_vector,
    read_mc_single,
    col_names = col_names,
    skip = skip,
    pb = pb
  )

  lengths <- vapply(results, nrow, integer(1))

  # these might not all have the same column names
  # but we can use a zero-length prototype to ensure
  # that all the common columns exist
  results_all <- vctrs::vec_rbind(
    tibble::tibble(
      temperature = double(),
      conductivity = double(),
      pressure = double(),
      date = as.Date(character()),
      time = hms::hms()
    ),
    !!! results
  )

  vctrs::vec_cbind(
    file = vctrs::vec_rep_each(file_vector, lengths),
    date_time = date_time(results_all$date, results_all$time),
    results_all[setdiff(names(results_all), c("date", "time"))]
  )
}

#' @rdname read_mc
#' @export
read_mc_single <- function(file, col_names = NULL, skip = NULL, pb = NULL) {
  bs_tick(pb, file)
  col_names <- col_names %||% guess_mc_col_names(file)
  skip <- skip %||% guess_mc_skip(file)

  cols <- readr::cols(
    serial_number = readr::col_character(),
    date = readr::col_date("%d %b %Y"),
    time = readr::col_time(),
    .default = readr::col_double()
  )

  # off-brand usage of cols!
  cols$cols <- cols$cols[intersect(names(cols$cols), col_names)]

  readr::read_csv(
    file,
    skip = skip,
    col_names = col_names,
    col_types = cols
  )
}

#' @rdname read_mc
#' @export
guess_mc_skip <- function(file) {
  switch(
    tolower(tools::file_ext(file)),
    "NA" = NULL,
    "mci" = 3,
    0
  )
}

#' @rdname read_mc
#' @export
guess_mc_col_names <- function(file) {
  switch(
    tolower(tools::file_ext(file)),
    "NA" = NULL,
    "mca" = {
      first_line <- readr::read_lines(file, n_max = 1)
      n_commas <- stringr::str_count(first_line, ",")[1]
      switch(
        as.character(n_commas),
        "8" = mca_col_names("2017"),
        "7" = mca_col_names("2018"),
        "6" = mca_col_names("2019"),
        abort(glue("Can't guess mcA column names for file '{ file }'"))
      )
    },
    "mci" = {
      first_line <- readr::read_lines(file, n_max = 4)
      n_commas <- stringr::str_count(first_line, ",")[4]
      switch(
        as.character(n_commas),
        "6" = mci_col_names("2017"),
        "7" = mca_col_names("2018"),
        abort(glue("Can't guess mcI column names for file '{ file }'"))
      )
    },
    "mch" = c(
      "temperature", "conductivity", "pressure", "salinity", "sound_speed",
      "date", "time"
    ),
    abort(glue("`guess_mc_col_names()` can't guess column names for file '{ file }'"))
  )
}

#' @rdname read_mc
#' @export
mca_col_names <- function(year) {
  switch(
    as.character(year),
    "2017" = c(
      "temperature", "conductivity", "pressure", "oxygen", "salinity",
      "sound_speed", "date", "time", "sample"
    ),
    "2018" = c(
      "temperature", "conductivity", "pressure", "oxygen", "salinity",
      "sound_speed", "date", "time"
    ),
    "2019" = c(
      "temperature", "conductivity", "pressure", "oxygen", "salinity",
      "date", "time"
    ),
    abort(glue("Can't fetch mcA column names for year '{ year }'"))
  )
}

#' @rdname read_mc
#' @export
mci_col_names <- function(year) {
  switch(
    as.character(year),
    "2017" = c(
      "temperature", "conductivity", "pressure", "salinity", "sound_speed",
      "date", "time"
    ),
    "2018" = c(
      "temperature", "conductivity", "pressure", "oxygen", "salinity", "sound_speed",
      "date", "time"
    ),
    abort(glue("Can't fetch mcI column names for year '{ year }'"))
  )
}
