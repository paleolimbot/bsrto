
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
read_hpb <- function(file_vector) {
  results <- lapply(file_vector, read_hpb_single)
  vctrs::vec_rbind(
    !!! results,
    .ptype = tibble::tibble(
      date = as.Date(character()),
      time = hms::hms(),
      atm_pres_mbar = double(),
      temp_c = double()
    )
  )
}

read_hpb_single <- function(file) {
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