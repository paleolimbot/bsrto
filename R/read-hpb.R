
#' Read hpb files
#'
#' @param file_vector A vector of filenames
#'
#' @return A [tibble::tibble()]
#' @export
#'
#' @examples
#' hpb_files <- list.files(bs_example("hpb"), "\\.hpb$", full.names = TRUE)
#' read_hpb(hpb_files)
#'
read_hpb <- function(file_vector, tz = "UTC") {
  # dd: vroom::vroom_fwf() is about 1.5 times faster but has
  # no opportunity for progress reporting
  # (for faster but more depressing reading)
  pb <- bs_progress(file_vector)
  on.exit(bs_progress_finish(pb))

  results <- lapply(file_vector, read_hpb_single, pb = pb)

  results_all <- vctrs::vec_rbind(
    !!! results,
    .ptype = tibble::tibble(
      date = as.Date(character()),
      time = hms::hms(),
      atm_pres_mbar = double(),
      temp_c = double()
    )
  )

  results_all$date <- as.POSIXct(results_all$date)
  attr(results_all$date, "tzone") <- tz

  tibble::tibble(
    date_time = results_all$date + results_all$time,
    atm_pres_mbar = results_all$atm_pres_mbar,
    temp_c = results_all$temp_c
  )
}

read_hpb_single <- function(file, pb = NULL) {
  bs_tick(pb, file)
  readr::read_table(
    file,
    col_names = c("date", "time", "atm_pres_mbar", "temp_c"),
    col_types = readr::cols(
      date = readr::col_date("%m/%d/%Y"),
      time = readr::col_time(),
      atm_pres_mbar = readr::col_double(),
      temp_c = readr::col_double()
    )
  )
}
