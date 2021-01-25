
#' Read LGH log files
#'
#' @inheritParams read_imm
#' @inheritParams read_icl
#'
#' @return A [tibble::tibble()]
#' @export
#'
#' @examples
#' lgh_file <- bs_example("lgh/20191010.lgH")
#' read_lgh(lgh_file)
#' read_lgh_vector(lgh_file)
#'
#' library(tidyr)
#' read_lgh(lgh_file) %>% unnest(log_text)
#'
read_lgh <- function(file, tz = "UTC", pb = NULL) {
  bs_tick(pb, file)

  lines <- readr::read_lines(file)
  session_start <- stringr::str_which(lines, "#+\\s*NEW SESSION\\s*#+")

  if(length(session_start) == 0) {
    return(
      tibble::tibble(
        date_time = as.POSIXct(character()),
        log_text = list()
      )
    )
  }

  session_end <- c(session_start[-1] - 1, length(lines))

  content <- Map("[", list(lines), Map(":", session_start, session_end))
  timestamp <- stringr::str_subset(lines, "BOOT_TS:")


  if (length(session_start) != length(timestamp)) {
    tibble::tibble(
      date_time = as.POSIXct(NA_character_),
      log_text = content
    )
  } else {
    tibble::tibble(
      date_time = readr::parse_datetime(
        stringr::str_match(timestamp, "BOOT_TS:\\s*(.*)$")[, 2],
        format = "%a %b %d %H:%M:%S %Y",
        locale = readr::locale(tz = tz)
      ),
      log_text = content
    )
  }
}

#' @rdname read_lgh
#' @export
read_lgh_vector <- function(file_vector, tz = "UTC") {
  pb <- bs_progress(file_vector)
  on.exit(bs_progress_finish(pb))

  results <- lapply(file_vector, read_lgh, tz = tz, pb = pb)
  results_all <- vctrs::vec_rbind(
    tibble::tibble(date_time = as.POSIXct(character()), log_text = list()),
    !!! results
  )
  lengths <- vapply(results, nrow, integer(1))

  vctrs::vec_cbind(
    file = vctrs::vec_rep_each(file_vector, lengths),
    results_all
  )
}
