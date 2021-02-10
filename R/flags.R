
#' Internal BSRTO flag scheme
#'
#' @param flag_label A human-readable
#' @param flag An integer flag value
#'
#' @export
#'
#' @examples
#' bs_flag(c("not assessed", "probably good data"))
#' bs_flag_label(1:5)
#' bs_flag_scheme()
#'
bs_flag <- function(flag_label) {
  result <- unname(bs_flag_scheme_internal[flag_label])

  # This function should never return NA...it is useful to catch errors
  # here as these are probably typos.
  if (any(is.na(result))) {
    bad_labels <- paste0("'", flag_label[is.na(result)], "'", collapse = ", ")
    abort(glue("Unknown flag label(s): { bad_labels }"))
  }

  result
}

#' @rdname bs_flag
#' @export
bs_flag_label <- function(flag) {
  names(bs_flag_scheme_internal)[match(flag, bs_flag_scheme_internal)]
}

#' @rdname bs_flag
#' @export
bs_flag_scheme <- function() {
  tibble::enframe(bs_flag_scheme_internal, name = "flag_label", value = "flag")
}

bs_flag_scheme_internal <- c(
  "not assessed" = 0L,
  "probably good data" = 1L,
  "probably bad data" = 2L,
  "missing" = 3L,
  "not measured" = 4L
)
