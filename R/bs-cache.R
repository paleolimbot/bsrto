
#' Cache locations
#'
#' Two types of caches are used in the bsrto package. The download cache, whose
#' location is returned by [bs_cache_dir()] is a folder within which the files
#' are organized as a mirror image of the FTP server. The build cache as
#' returned by [bs_build_cache_dir()] is used by [bs_build_realtime()] to keep
#' intermediary build stages of the data that would otherwise be expensive to
#' recalculate. These can be set via `options(bsrto.cache = "...")` and
#' `options(bsrto.build_cache = "...")` in an R session or by the environment
#' variables `R_BSRTO_CACHE` and `R_BSRTO_BUILD_CACHE` (useful on CI). By
#' default a temporary directory is set that is cleaned up on package unload.
#'
#' @param ... Passed to [file.path()]
#' @export
#'
#' @examples
#' bs_cache_dir()
#' bs_build_cache_dir()
#' bs_has_full_cache()
#'
bs_cache_dir <- function(...) {
  cache <- Sys.getenv("R_BSRTO_CACHE", "")
  if (!identical(cache, "")) {
    return(file.path(cache, ...))
  }

  cache <- getOption("bsrto.cache", NULL)
  if (!is.null(cache)) {
    return(file.path(cache, ...))
  }

  file.path(bs_temp_dir_internal, "bsrto_cache", ...)
}

#' @rdname bs_cache_dir
#' @export
bs_build_cache_dir <- function(...) {
  cache <- Sys.getenv("R_BSRTO_BUILD_CACHE", "")
  if (!identical(cache, "")) {
    return(file.path(cache, ...))
  }

  cache <- getOption("bsrto.build_cache", NULL)
  if (!is.null(cache)) {
    return(file.path(cache, ...))
  }

  file.path(bs_temp_dir_internal, "bsrto_build_cache", ...)
}

#' @rdname bs_cache_dir
#' @export
bs_ftp_server <- function(...) {
  ftp <- Sys.getenv("R_BSRTO_FTP_SERVER", "")
  if (!identical(ftp, "")) {
    return(file.path(ftp, ..., fsep = "/"))
  }

  ftp <- getOption("bsrto.ftp_server", NULL)
  if (!is.null(ftp)) {
    return(file.path(ftp, ..., fsep = "/"))
  }

  abort(
    "Must set options(bsrto.ftp_server = '...') or R_BSRTO_FTP_SERVER environment variable."
  )
}


#' @rdname bs_cache_dir
#' @export
bs_has_full_cache <- function() {
  # need to sort this out!
  FALSE
}
