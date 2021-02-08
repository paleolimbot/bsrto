
cat_file <- function(file) {
  cat(readr::read_file(file))
}

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
    abort(glue("Can't find end of header in '{ file }'."))
  }

  lines[seq_len(which(end_header)[1] - 1)]
}

date_time <- function(date, time, tz = "UTC") {
  date <- as.POSIXct(date)
  attr(date, "tzone") <- tz
  date + time
}

resample_exact <- function(x, y, xout) {
  y[match(xout, x)]
}

resample_linear <- function(x, y, xout) {
  stats::approx(as.numeric(x), y, as.numeric(xout))$y
}

resample_last <- function(x, y, xout) {
  stats::approx(
    as.numeric(x), y,
    as.numeric(xout),
    method = "constant", f = 0, rule = 1:2
  )$y
}

resample_nearest <- function(x, y, xout, max_distance = Inf) {
  x <- as.numeric(x)
  xout <- as.numeric(xout)
  nearest <- vapply(xout, function(xi) which.min(abs(x - xi)), integer(1))
  result <- y[nearest]

  if (!identical(max_distance, Inf)) {
    dist <- vapply(xout, function(xi) min(abs(x - xi)), double(1))
    result[dist > max_distance] <- y[NA_integer_]
  }

  result
}
