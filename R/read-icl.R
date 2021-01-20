
#' Read icl files
#'
#' @inheritParams read_odf
#' @param skip The number of header rows to skip
#'
#' @return A [tibble::tibble()]
#' @export
#'
#' @examples
#' icl_file <- bs_example("icl/SAF2564_20191010_19.txt")
#' read_icl(icl_file)
#' read_icl_header(icl_file)
#' read_icl_header_lines(icl_file)
#'
read_icl <- function(file, skip = length(read_icl_header_lines(file)) + 1,
                     n_max = Inf) {
  readr::read_tsv(
    file,
    col_types = readr::cols(
      Time = readr::col_time(),
      Comment = readr::col_character(),
      `Temperature [C]` = readr::col_double(),
      `Humidity [%]` = readr::col_double(),
      .default = readr::col_integer()
    ),
    skip = skip,
    n_max = n_max
  )
}

#' @rdname read_icl
#' @export
read_icl_header <- function(file, header_lines = read_icl_header_lines(file)) {
  section_start <- grep(":$", header_lines)
  section_end <- grep("^\\s*$", header_lines)
  if (length(section_start) != length(section_end)) {
    abort(glue("Differing lengths for section start and end in read_icl_header('{ file }')"))
  }

  section_title <- gsub(":$", "", header_lines[section_start])
  section_content <- Map("[", list(header_lines), Map(":", section_start + 1, section_end))
  section_content_parsed <- lapply(section_content, function(x) {
    x <- x[-length(x)]
    split <- stringr::str_split_fixed(x, "\t", 2)
    tibble::tibble(key = split[, 1], value = split[, 2])
  })

  names(section_content_parsed) <- section_title
  section_content_parsed
}

#' @rdname read_icl
#' @export
read_icl_header_lines <- function(file, n_header = 29) {
  header_lines(
    file,
    function(x) grepl("^Data:\\s*$", x),
    n_header = n_header
  )
}
