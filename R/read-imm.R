
#' Read IMM files
#'
#' @inheritParams read_hpb
#'
#' @return A [tibble::tibble()]
#' @export
#'
#' @examples
#' imm_file <- bs_example("imm/18082902.imm")
#' read_imm(imm_file)
#' read_imm_vector(imm_file)
#'
read_imm <- function(file, tz = "UTC") {
  stopifnot(length(file) == 1)
  read_imm_vector(file, tz = tz)[-1]
}

#' @rdname read_imm
#' @export
read_imm_vector <- function(file_vector, tz = "UTC") {
  pb <- bs_progress(file_vector)
  on.exit(bs_progress_finish(pb))

  results <- lapply(file_vector, read_imm_single, pb = pb)
  lengths <- vapply(results, nrow, integer(1))

  results_all <- vctrs::vec_rbind(
    !!! results,
    .ptype = tibble::tibble(
      serial_number = character(),
      temperature = double(),
      conductivity = double(),
      date = as.Date(character()),
      time = hms::hms(),
      pressure = double(),
      sample = double()
    )
  )

  vctrs::vec_cbind(
    file = vctrs::vec_rep_each(file_vector, lengths),
    date_time = date_time(results_all$date, results_all$time),
    results_all[setdiff(names(results_all), c("date", "time"))]
  )
}

read_imm_single <- function(file, pb = NULL) {
  bs_tick(pb, file)

  readr::read_csv(
    file,
    col_names = c(
      "serial_number", "temperature", "conductivity",
      "pressure", "date", "time", "sample"
    ),
    col_types = readr::cols(
      serial_number = readr::col_character(),
      date = readr::col_date("%d %b %Y"),
      time = readr::col_time(),
      .default = readr::col_double()
    )
  )
}
