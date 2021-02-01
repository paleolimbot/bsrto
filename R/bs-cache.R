
#' Find a local cache of the BSRTO FTP server
#'
#' @param ... Passed to [file.path()]
#' @export
#'
#' @examples
#' bs_cache_dir()
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
bs_has_full_cache <- function() {
  bs_cache_dir() != file.path(bs_temp_dir_internal, "bsrto_cache")
}
