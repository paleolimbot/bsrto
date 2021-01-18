
#' Read 'ODF' files
#'
#' ODF (Ocean Data) files are whitespace-delimited files
#' whose column names and are specified in a loosely formatted header.
#'
#' @param file A file, URL, or connection. Files ending in .gz, .bz2, .xz, or
#'   .zip will be automatically uncompressed; URLs will be automatically
#'   downloaded. See [readr::read_lines()] for a full description of how
#'   this parameter is interpreted.
#' @param skip Number of non-header rows to skip.
#' @param n_max Maximum number of rows to read.
#' @param header A previously read value obtained from [read_odf_header()].
#' @param header_lines A previously read value obtained from
#'   [read_odf_header_lines()].
#' @param n_header The starting guess for number of header lines.
#'
#' @export
#'
#' @examples
#' odf_file <- bs_example("CTD_98911_10P_11_DN.ODF")
#' read_odf(odf_file)
#' read_odf_colmeta(odf_file)
#' str(read_odf_header(odf_file))
#'
read_odf <- function(file, skip = 0, n_max = Inf) {
  header_lines <- read_odf_header_lines(file)
  header <- read_odf_header(file, header_lines)
  col_header <- header$PARAMETER_HEADER

  col_names <- vapply(col_header, "[[", "NAME", FUN.VALUE = character(1))

  type_names <- vapply(col_header, "[[", "TYPE", FUN.VALUE = character(1))
  readr_types <- list(
    DOUB = readr::col_double(),
    INTE = readr::col_integer(),
    SYTM = readr::col_datetime("%d-%b-%Y %H:%M:%OS")
  )[type_names]
  names(readr_types) <- col_names
  col_types <- do.call(readr::cols, readr_types[!vapply(readr_types, is.null, logical(1))])

  readr::read_fwf(
    file, trim_ws = TRUE,
    col_positions = readr::fwf_empty(
      file,
      col_names = col_names,
      skip = length(header_lines) + 1 + skip,
      n = 100L
    ),
    col_types = col_types,
    skip = length(header_lines) + 1 + skip,
    n_max = n_max,
  )
}

#' @rdname read_odf
#' @export
read_odf_colmeta <- function(file, header = read_odf_header(file)) {
  params <- lapply(header$PARAMETER_HEADER, tibble::as_tibble)
  params_tbl <- vctrs::vec_rbind(!!! unname(params), .names_to = "index")
  params_tbl[-1] <- lapply(params_tbl[-1], readr::parse_guess)
  params_tbl
}

#' @rdname read_odf
#' @export
read_odf_header <- function(file, header_lines = read_odf_header_lines(file)) {
  # extract components
  components <- stringr::str_match(
    header_lines,
    "^(\\s*)([A-Za-z0-9_]+\\s*=\\s*)?'?\\s*(.*?)\\s*'?\\s*,?\\s*$"
  )

  whitespace <- components[, 2]
  name <- stringr::str_remove(components[, 3], "\\s*=\\s*$")
  name[is.na(name)] <- ""
  value <- components[, 4]

  # model with two levels of whitespace: one for top level headers
  # one for everything else
  n_whitespace <- stringr::str_length(whitespace)
  is_top_header <- (n_whitespace == min(n_whitespace)) & (header_lines != whitespace)
  top_headers <- value[is_top_header]
  which_top_header <- cumsum(is_top_header)

  parsed <- rep(list(NULL), length(top_headers))
  for (i in seq_along(top_headers)) {
    value_i <- value[which_top_header == i][-1]
    names(value_i) <- name[which_top_header == i][-1]
    parsed[[i]] <- collapse_by_name(value_i)
  }

  names(parsed) <- top_headers
  collapse_by_name(parsed)
}

#' @rdname read_odf
#' @export
read_odf_header_lines <- function(file, n_header = 1000) {
  stopifnot(n_header > 0)

  lines <- readr::read_lines(file, n_max = n_header)
  end_header <- grepl("\\s*-- DATA --\\s*", lines)

  while ((length(lines) == n_header) && !any(end_header)) {
    n_header <- n_header * 2
    lines <- readr::read_lines(file, n_max = n_header)
    end_header <- grepl("\\s*-- DATA --\\s*", lines)
  }

  if (!any(end_header)) {
    abort(glue("Can't find '-- DATA --' at end of header in '{ file }'.\nIs it an ODF file?"))
  }

  lines[seq_len(which(end_header)[1] - 1)]
}

collapse_by_name <- function(x) {
  out_names <- unique(names(x))
  out <- rep(list(NULL), length(out_names))
  for (i in seq_along(out_names)) {
    items <- unname(x[names(x) == out_names[i]])
    out[[i]] <- items
  }

  names(out) <- out_names
  out
}
