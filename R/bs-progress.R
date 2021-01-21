
bs_progress <- function(along, total = length(along)) {
  if (identical(getOption("bsrto.progress"), FALSE)) {
    return(NULL)
  }

  pb <- progress::progress_bar$new(
    "[:bar] :file",
    total = total
  )
  pb$tick(0)
  pb
}

bs_tick <- function(pb, file) {
  if (!is.null(pb)) pb$tick(tokens = list(file = basename(file)))
  invisible(pb)
}

bs_progress_finish <- function(pb) {

}
