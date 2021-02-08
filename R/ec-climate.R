
#' Read Environment Canada Climate CSVs
#'
#' @inheritParams read_icl
#' @param utc_offset Environment Canada provides dates and times in
#'   "local standard time", which has a fixed UTC offset (i.e., does not
#'   observe daylight savings time).
#'
#' @return A [tibble::tibble()]
#' @export
#'
#' @examples
#' ec_file <- bs_example("met/ec-climate_hourly_54199_2019_08.csv")
#' read_ec_climate_hourly(ec_file)
#' read_ec_climate_hourly_vector(ec_file)
#'
read_ec_climate_hourly <- function(file, pb = NULL, utc_offset = 0) {
  bs_tick(pb, file)

  # the program writing CSVs for EC tends to abandon incomplete records
  # rather than put in the right number of commas, which leads to
  # warnings about the number of expected columns
  result <- suppressWarnings(
    readr::read_csv(
      file,
      col_types = ec_cols_hourly(),
      locale = readr::locale(tz = "UTC")
    )
  )

  # we really do want to know about other parse errors if there are any
  # (e.g., env. canada changes the csv format)
  problems <- attr(result, "problems")
  not_a_problem <- (problems$expected == "30 columns") &
    (problems$actual == "9 columns")
  if (all(not_a_problem)) {
    attr(result, "problems") <- NULL
  } else {
    attr(result, "problems") <- problems[!not_a_problem, ]
  }

  # cheap way to call the internal readr::warn_problems() with revised
  # problems list
  asNamespace("readr")$warn_problems(result)

  result$`Date/Time (LST)` <- result$`Date/Time (LST)` -
    as.difftime(utc_offset, units = "hours")
  names(result)[names(result) == "Date/Time (LST)"] <- "date_time"

  result
}

#' @rdname read_ec_climate_hourly
#' @export
read_ec_climate_hourly_vector <- function(file_vector, utc_offset = 0) {
  pb <- bs_progress(file_vector)
  on.exit(bs_progress_finish(file_vector))

  results <- lapply(file_vector, read_ec_climate_hourly, pb, utc_offset)
  lengths <- vapply(results, nrow, integer(1))
  results_all <- vctrs::vec_rbind(
    tibble::tibble(date_time = as.POSIXct(character())),
    !!! results
  )

  vctrs::vec_cbind(
    file = vctrs::vec_rep_each(file_vector, lengths),
    results_all
  )
}

ec_nice_names <- function(names) {
  names <- stringr::str_remove(tolower(names), "\\(.*$")
  names <- stringr::str_trim(names)
  stringr::str_replace_all(names, "[\\s.]+", "_")
}

ec_download_summary_hourly <- function(station_id, start_date, end_date) {
  all_months <- seq(as.Date(start_date), as.Date(end_date), "month")

  datelt <- as.POSIXlt(all_months)
  year <- datelt$year + 1900
  month <- datelt$mon + 1

  url <- ec_bulk_data_url("hourly", station_id, year, month)
  dest <- ec_bulk_data_dest("hourly", station_id, year, month)

  tibble::tibble(year = year, month = month, url = url, dest = dest)
}

ec_bulk_data_dest <- function(timeframe, station_id, Year = NULL, Month = NULL) {
  switch(
    timeframe,
    "monthly" = sprintf("ec-climate_%s_%s.csv", timeframe, station_id),
    "daily" = sprintf("ec-climate_%s_%s_%04d.csv", timeframe, station_id, Year),
    "hourly" = sprintf("ec-climate_%s_%s_%04d_%02d.csv", timeframe, station_id, Year, Month),
    abort("`timeframe` must be one of 'monthly', 'daily', or 'hourly")
  )
}

ec_bulk_data_url <- function(timeframe, station_id, Year = NULL, Month = NULL) {
  url_query <- switch(
    timeframe,
    "monthly" = glue("stationID={ station_id }&timeframe=3"),
    "daily" = glue("stationID={ station_id }&Year={ Year }&timeframe=2"),
    "hourly" = glue("stationID={ station_id }&Year={ Year }&Month={ Month }&timeframe=1"),
    abort("`timeframe` must be one of 'monthly', 'daily', or 'hourly'")
  )

  base_url <- "http://climate.weather.gc.ca/climate_data/bulk_data_e.html"
  glue("{base_url}?format=csv&{ url_query }&submit=Download+Data")
}

ec_cols_hourly <- function() {
  cols <- readr::cols(
    .default = readr::col_character(),
    `Longitude (x)` = readr::col_double(),
    `Latitude (y)` = readr::col_double(),
    `Climate ID` = readr::col_double(),
    `Date/Time (LST)` = readr::col_datetime(),
    Year = readr::col_skip(),
    Month = readr::col_skip(),
    Day = readr::col_skip(),
    `Time (LST)` = readr::col_skip(),
    `Temp (DEGREESC)` = readr::col_double(),
    `Dew Point Temp (DEGREESC)` = readr::col_double(),
    `Rel Hum (%)` = readr::col_double(),
    `Precip. Amount (mm)` = readr::col_double(),
    `Wind Dir (10s deg)` = readr::col_double(),
    `Wind Spd (km/h)` = readr::col_double(),
    `Visibility (km)` = readr::col_double(),
    `Stn Press (kPa)` = readr::col_double(),
    Hmdx = readr::col_double(),
    `Wind Chill` = readr::col_double()
  )

  # hack to fix the "\u00B0" can't be translated to native encoding error
  names(cols$cols) <- gsub("DEGREES", "\u00B0", names(cols$cols))
  cols
}
