
#' Build real-time data from the 2019 deployment
#'
#' @param out_dir The directory in which output files should be
#'   generated.
#'
#' @return `out_dir`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' bs_build_2019()
#' }
#'
bs_build_2019 <- function(out_dir = ".") {
  build_2019_met(out_dir)
  build_2019_hpb(out_dir)
  build_2019_icl(out_dir)
  build_2019_ips(out_dir)
  build_2019_lgh(out_dir)
  build_2019_mca(out_dir)
  build_2019_mch(out_dir)
  build_2019_mci(out_dir)
  build_2019_pcm(out_dir)
  build_2019_rdi(out_dir)

  invisible(out_dir)
}

# met here refers to environment canada hourly data from resolute
# these files aren't cached on the ftp server but are available from
# environment canada
build_2019_met <- function(out_dir = ".") {
  cli::cat_rule(glue("build_2019_met('{ out_dir }')"))
  cache_dir <- bs_cache_dir("BSRTO/2019-2020/met")

  ec_files <- ec_download_summary_hourly(54199, "2019-08-01", Sys.Date())
  ec_files$dest <- file.path(cache_dir, ec_files$dest)

  # need to re-download updated version for this month (so delete cache file)
  unlink(ec_files$dest[nrow(ec_files)])

  dest_exists <- file.exists(ec_files$dest)
  cli::cat_line(glue("About to download { sum(!dest_exists) } file(s)"))

  for (i in seq_len(4)) {
    try(multi_file_download_async(ec_files$url[!dest_exists], ec_files$dest[!dest_exists]))
    dest_exists <- file.exists(ec_files$dest)

    if (all(dest_exists)) {
      break
    }
  }

  if (any(!dest_exists)) {
    abort("Failed to download all required climate files.")
  }

  build_2019_log_about_to_read(ec_files$dest)
  all <- read_ec_climate_hourly_vector(ec_files$dest, utc_offset = -6)
  all$file <- build_2019_file_relative(all$file)
  names(all) <- ec_nice_names(names(all))

  # skip the station information which is repeated for every row
  station_info <- c("longitude", "latitude", "station_name", "climate_id")
  all <- all[setdiff(names(all), station_info)]

  out_file <- file.path(out_dir, "met.csv")
  cli::cat_line(glue("Writing '{ out_file }'"))
  readr::write_csv(all, out_file)
}

build_2019_hpb <- function(out_dir = ".") {
  cli::cat_rule(glue("build_2019_hpb('{ out_dir }')"))
  dir <- "BSRTO/2019-2020/hpb"
  cached <- build_2019_list_and_cache(dir)

  build_2019_log_about_to_read(cached)
  all <- read_hpb_vector(cached)
  all$file <- build_2019_file_relative(all$file)

  out_file <- file.path(out_dir, "hpb.csv")
  cli::cat_line(glue("Writing '{ out_file }'"))
  readr::write_csv(all, out_file)
}

build_2019_icl <- function(out_dir = ".") {
  cli::cat_rule(glue("build_2019_icl('{ out_dir }')"))
  dir <- "BSRTO/2019-2020/icl"
  cached <- build_2019_list_and_cache(dir)

  build_2019_log_about_to_read(cached)
  all <- read_icl_vector(cached)
  all$file <- build_2019_file_relative(all$file)

  # there are a lot of malformed files...can check Time
  # Comment, and data_points columns for validity
  time_valid <- !is.na(all$Time)
  comment_valid <- all$Comment %in% c("", "Time Adjusted")
  data_points_valid <- is.finite(all$`Data Points`) & all$`Data Points` == 410
  rows_valid <- time_valid & comment_valid & data_points_valid

  build_2019_log_qc(all, rows_valid)

  # TODO: split off the histograms as NetCDF

  out_file <- file.path(out_dir, "icl.csv")
  cli::cat_line(glue("Writing '{ out_file }'"))
  readr::write_csv(all[rows_valid, ], out_file)
}

build_2019_ips <- function(out_dir = ".") {
  cli::cat_rule(glue("build_2019_ips('{ out_dir }')"))
  dir <- "BSRTO/2019-2020/ips"
  cached <- build_2019_list_and_cache(dir)

  build_2019_log_about_to_read(cached)
  all <- read_ips_bn_vector(cached)
  all$file <- build_2019_file_relative(all$file)

  # TODO: need to find a way to write histograms
  all$bins <- NULL

  out_file <- file.path(out_dir, "ips.csv")
  cli::cat_line(glue("Writing '{ out_file }'"))
  readr::write_csv(all, out_file)
}

build_2019_lgh <- function(out_dir = ".") {
  cli::cat_rule(glue("build_2019_lgh('{ out_dir }')"))
  dir <- "BSRTO/2019-2020/lgH"
  cached <- build_2019_list_and_cache(dir)

  build_2019_log_about_to_read(cached)
  all <- read_lgh_vector(cached)
  all$file <- build_2019_file_relative(all$file)

  # TODO: find a way for logs to get loaded
  all$log_text <- NULL

  out_file <- file.path(out_dir, "lgh.csv")
  cli::cat_line(glue("Writing '{ out_file }'"))
  readr::write_csv(all, out_file)
}

build_2019_mca <- function(out_dir = ".") {
  cli::cat_rule(glue("build_2019_mca('{ out_dir }')"))
  dir <- "BSRTO/2019-2020/mcA"

  cached <- build_2019_list_and_cache(dir)

  build_2019_log_about_to_read(cached)
  all <- read_mc_vector(cached)
  all$file <- build_2019_file_relative(all$file)

  # basic QC to filter out mangled rows
  temp_valid <- !is.na(all$temperature) & (all$temperature > -20) &( all$temperature < 20)
  datetime_valid <- !is.na(all$date_time)
  rows_valid <- temp_valid & datetime_valid

  build_2019_log_qc(all, rows_valid)

  out_file <- file.path(out_dir, "mca.csv")
  cli::cat_line(glue("Writing '{ out_file }'"))
  readr::write_csv(all[rows_valid, ], out_file)
}

build_2019_mch <- function(out_dir = ".") {
  cli::cat_rule(glue("build_2019_mch('{ out_dir }')"))
  dir <- "BSRTO/2019-2020/mcH"

  cached <- build_2019_list_and_cache(dir)

  build_2019_log_about_to_read(cached)
  all <- read_mc_vector(cached)
  all$file <- build_2019_file_relative(all$file)

  # basic QC to filter out mangled rows
  temp_valid <- !is.na(all$temperature) & (all$temperature > -20) &( all$temperature < 20)
  datetime_valid <- !is.na(all$date_time)
  rows_valid <- temp_valid & datetime_valid

  build_2019_log_qc(all, rows_valid)

  out_file <- file.path(out_dir, "mch.csv")
  cli::cat_line(glue("Writing '{ out_file }'"))
  readr::write_csv(all[rows_valid, ], out_file)
}

build_2019_mci <- function(out_dir = ".") {
  cli::cat_rule(glue("build_2019_mci('{ out_dir }')"))
  dir <- "BSRTO/2019-2020/mcI"

  cached <- build_2019_list_and_cache(dir)

  build_2019_log_about_to_read(cached)
  all <- read_mc_vector(cached)
  all$file <- build_2019_file_relative(all$file)

  # basic QC to filter out mangled rows
  temp_valid <- !is.na(all$temperature) & (all$temperature > -20) &( all$temperature < 20)
  datetime_valid <- !is.na(all$date_time)
  rows_valid <- temp_valid & datetime_valid

  build_2019_log_qc(all, rows_valid)

  out_file <- file.path(out_dir, "mci.csv")
  cli::cat_line(glue("Writing '{ out_file }'"))
  readr::write_csv(all[rows_valid, ], out_file)
}

build_2019_pcm <- function(out_dir = ".") {
  cli::cat_rule(glue("build_2019_pcm('{ out_dir }')"))
  dir <- "BSRTO/2019-2020/pcm"

  cached <- build_2019_list_and_cache(dir)

  build_2019_log_about_to_read(cached)
  all <- read_pcm_vector(cached)
  all$file <- build_2019_file_relative(all$file)

  # basic QC to filter out mangled rows
  rows_valid <- all$checksum_valid
  build_2019_log_qc(all, rows_valid)

  out_file <- file.path(out_dir, "pcm.csv")
  cli::cat_line(glue("Writing '{ out_file }'"))
  readr::write_csv(all[rows_valid, ], out_file)
}

build_2019_rdi <- function(out_dir = ".") {
  cli::cat_rule(glue("build_2019_rdi('{ out_dir }')"))
  dir <- "BSRTO/2019-2020/rdi"

  cached <- build_2019_list_and_cache(dir)

  build_2019_log_about_to_read(cached)
  all <- read_rdi_vector(cached)
  all$file <- build_2019_file_relative(all$file)

  # basic QC to filter out mangled rows
  rows_valid <- all$checksum_valid
  build_2019_log_qc(all, rows_valid)

  # TODO: find a way to write list columns
  all <- all[!vapply(all, is.list, logical(1))]

  out_file <- file.path(out_dir, "rdi.csv")
  cli::cat_line(glue("Writing '{ out_file }'"))
  readr::write_csv(all, out_file)
}

build_2019_log_about_to_read <- function(cached) {
  cli::cat_line(glue("Reading { length(cached) } file(s)"))
  if (length(cached) >= 2) {
    cli::cat_line(glue("'{ basename(cached[1]) }'...'{ basename(cached[length(cached)]) }'"))
  } else if (length(cached) >= 1) {
    cli::cat_line(glue("'{ basename(cached) }'"))
  }
}

build_2019_log_qc <- function(all, rows_valid) {
  files_with_errors <- unique(all$file[!rows_valid])

  cli::cat_line(
    glue("Removing { sum(!rows_valid) } unreadable rows ({ round(mean(!rows_valid) * 100, 1)}%)")
  )
  cli::cat_line(
    glue("{ length(files_with_errors) }/{ nrow(all) } files had errors")
  )
}

build_2019_list_and_cache <- function(dir, retries = 4) {
  cli::cat_line(glue("Updating cache for '{ dir }'"))
  for (i in seq_len(retries)) {
    tryCatch({
      files <- bs_ftp_list(dir)
      break
    }, error = function(e) {
      cli::cat_line(paste0(e, collapse = " "), col = "red")
    })
  }

  if (!exists("files", inherits = FALSE)) {
    abort(glue("Failed to list '{ dir }' after { retries } retries."))
  }

  # there is one file that must have permissions set that don't allow access
  files <- files[files$file != "BSRTO/2019-2020/ips/200224AA.bn4", ]

  summary_size <- build_2019_friendly_file_size(sum(files$size))
  cli::cat_line(glue("Summary: { nrow(files) } file(s) ({ summary_size })"))

  needs_download <- !file.exists(bs_cache_dir(files$file))
  if (any(needs_download)) {
    download_size <- build_2019_friendly_file_size(sum(files$size[needs_download]))
    cli::cat_line(
      glue(
        "About to download { sum(needs_download) } file(s) ({ download_size })"
      )
    )
  }

  cached <- bs_cached(files, async = TRUE)

  zero_size <- file.size(cached) == 0
  if (sum(zero_size) > 0) {
    zero_size_files <- paste0("'", basename(cached[zero_size]), "'", collapse = "\n")
    cli::cat_line(glue("Skipping { sum(zero_size) } files(s) with zero size:\n{ zero_size_files }"))
  }

  cached[!zero_size]
}

build_2019_file_relative <- function(file) {
  basename(file)
}

build_2019_friendly_file_size <- function(size) {
  if (size > 2^20) {
    sprintf("%0.1f MiB", size / 2^20)
  } else if (size > 2^10) {
    sprintf("%0.1f KiB", size / 2^10)
  } else if (size != 1) {
    sprintf("%d bytes", size)
  } else {
    sprintf("%d byte", size)
  }
}
