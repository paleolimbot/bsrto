
header_lines <- function(file, end_header_function, n_header = 100) {
  stopifnot(n_header > 0)

  lines <- readr::read_lines(file, n_max = n_header)
  end_header <- end_header_function(lines)

  while ((length(lines) == n_header) && !any(end_header)) {
    n_header <- n_header * 2
    lines <- readr::read_lines(file, n_max = n_header)
    end_header <- end_header_function(lines)
  }

  if (!any(end_header)) {
    abort(glue("Can't find '-- DATA --' at end of header in '{ file }'.\nIs it an ODF file?"))
  }

  lines[seq_len(which(end_header)[1] - 1)]
}

date_time <- function(date, time, tz = "UTC") {
  date <- as.POSIXct(date)
  attr(date, "tzone") <- tz
  date + time
}
