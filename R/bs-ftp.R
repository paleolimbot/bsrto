
#' Cache, fetch, and list files from the BSRTO FTP server
#'
#' @param x A path relative to /pub/dfo
#' @param ftp The location of the FTP server
#' @param cache Path to a local copy of /pub/dfo where files will be cached
#'   or written.
#' @param quiet Use `TRUE` to suppress progress messages
#' @param async Use `TRUE` to download files asynchronously (much faster
#'   for many small files)
#' @param recursive Use `TRUE` to recurse directories
#' @param pattern A regular expression against which files are matched
#'   or `NULL` to match all files. See [stringr::str_detect()] for how
#'   this argument is interpreted.
#' @param print Use `TRUE` to print out matching files as the listings are
#'   downloaded.
#'
#' @return `bs_ftp_cached()` returns the file path to the local version of the
#'   file after downloading; `bs_ftp_list()` returns a list of files (not
#'   directories!) that match `pattern`.
#' @export
#'
#' @examples
#' \dontrun{
#' bs_ftp_cached("barrow/BarrowStraitDataSummary.xlsx", cache = tempfile())
#' bs_ftp_list("barrow")
#' }
bs_ftp_cached <- function(x,
                          ftp = getOption("bsrto.ftp", "ftp://dfoftp.ocean.dal.ca/pub/dfo"),
                          cache = bs_cache(), async = FALSE, quiet = FALSE) {
  if (is.null(cache)) {
    abort("Can't use `NULL` cache.\nDid you forget to set `options(bsrto.cache = '')`?")
  }

  ftp <- gsub("/?$", "/", ftp)
  url <- paste0(ftp, x)
  cached_path <- file.path(cache, x)

  exists <- file.exists(cached_path)

  if (async) {
    multi_file_download_async(url[!exists], cached_path[!exists])
  } else {
    multi_file_download(url[!exists], cached_path[!exists])
  }

  cached_path
}

#' @rdname bs_ftp_cached
#' @export
bs_ftp_list <- function(x, pattern = NULL, recursive = FALSE,
                        ftp = getOption("bsrto.ftp", "ftp://dfoftp.ocean.dal.ca/pub/dfo"),
                        print = FALSE, quiet = FALSE) {
  if (length(x) == 0) {
    return(tibble::tibble(file = character(), size = numeric()))
  } else if (length(x) != 1) {
    results <- lapply(
      x,
      bs_ftp_list,
      pattern = pattern,
      recursive = recursive,
      ftp = ftp,
      quiet = quiet,
      print = print
    )

    return(do.call(rbind, results))
  }

  # make sure x and ftp end with a trailing slash
  if (!(x %in% c("", "/"))) {
    x <- gsub("/?$", "/", x)
  }
  ftp <- gsub("/?$", "/", ftp)

  ftp_address <- paste0(ftp, x)
  if (!quiet) message(glue("Listing directory '{ ftp_address }'"))
  result <- curl::curl_fetch_memory(ftp_address)

  # parse the results
  listing <- stringr::str_split_fixed(readr::read_lines(result$content), pattern = "\\s+", 9)

  is_dir <- grepl("^d", listing[, 1])
  files <- if (any(!is_dir)) paste0(x, listing[, 9][!is_dir]) else character()
  file_sizes <- as.numeric(listing[, 5][!is_dir])
  dirs <- if (any(is_dir)) paste0(x, listing[, 9][is_dir]) else character()

  # filter by pattern
  if (!is.null(pattern)) {
    files_match <- stringr::str_detect(basename(files), pattern)
    files <- files[files_match]
    file_sizes <- file_sizes[files_match]
  }

  if (print) {
    cat(paste0(glue::glue("{ files } [{ file_sizes } bytes]"), collapse = "\n"))
    cat("\n")
  }

  files_df <- tibble::tibble(file = files, size = file_sizes)

  if (recursive && (length(dirs) > 0)) {
    subdirectory_listings <- lapply(
      dirs,
      bs_ftp_list,
      pattern = pattern,
      recursive = TRUE,
      quiet = quiet,
      print = FALSE
    )

    files_df <- rbind(files_df, do.call(rbind, subdirectory_listings))
  }

  if (print) {
    invisible(files_df)
  } else {
    files_df
  }
}
