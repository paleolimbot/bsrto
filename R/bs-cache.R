
#' Find a local cache of the BSRTO FTP server
#'
#' @export
#'
#' @examples
#' bs_cache()
#' bs_has_cache()
#'
bs_cache <- function() {
  cache <- Sys.getenv("R_BSRTO_CACHE", "")
  if (!identical(cache, "")) {
    return(cache)
  }

  cache <- getOption("bsrto.cache", NULL)
  if (!is.null(cache)) {
    return(cache)
  }

  NULL
}

#' @rdname bs_cache
#' @export
bs_has_cache <- function() {
  !is.null(bs_cache())
}
