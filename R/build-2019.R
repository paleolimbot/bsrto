
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

  # Read functions are concerned with taking raw data files and filtering
  # out data that is corrupted or otherwise unreadable. These functions also
  # check for new files and download them if they aren't present locally.
  steps <- c(
    "met", "hpb", "icl", "ips", "lgh",
    "mca", "mch", "mci", "pcm", "rdi"
  )

  names(steps) <- steps

  built <- lapply(steps, read_2019_cached)

  # Write functions take care of corrections and QC checks that might require
  # values from other files (e.g., corrections for pressure, heading)
  write_2019_met(built, out_dir)
  write_2019_hpb(built, out_dir)
  write_2019_icl(built, out_dir)
  write_2019_ips(built, out_dir)
  write_2019_lgh(built, out_dir)
  write_2019_mca(built, out_dir)
  write_2019_mch(built, out_dir)
  write_2019_mci(built, out_dir)
  write_2019_pcm(built, out_dir)
  write_2019_rdi(built, out_dir)

  # (any real-time outputs need to re-read these files, which are the source
  # of truth for this deployment)

  invisible(out_dir)
}

write_2019_met <- function(built, out_dir = ".") {
  cli::cat_rule("write_2019_met()")
  readr::write_csv(built$met, file.path(out_dir, "met.csv"))
}

write_2019_hpb <- function(built, out_dir = ".") {
  cli::cat_rule("write_2019_hpb()")
  readr::write_csv(built$hpb, file.path(out_dir, "icl.csv"))
}

write_2019_icl <- function(built, out_dir = ".") {
  cli::cat_rule("write_2019_icl()")
  readr::write_csv(built$icl, file.path(out_dir, "icl.csv"))
}

write_2019_ips <- function(built, out_dir = ".") {
  cli::cat_rule("write_2019_ips()")

  # bins is a list-col: join by whitespace
  built$ips$bins <- vapply(built$ips$bins, paste0, collapse = " ", FUN.VALUE = character(1))

  readr::write_csv(built$ips, file.path(out_dir, "ips.csv"))
}

write_2019_lgh <- function(built, out_dir = ".") {
  cli::cat_rule("write_2019_lgh()")

  # log_text is a list-col, but we can unnest it
  log_text <- built$lgh$log_text
  lengths <- vapply(log_text, length, integer(1))
  built$lgh$log_text <- NULL
  built$lgh <- vctrs::vec_rep_each(built$lgh, lengths)
  built$lgh$log_text <- do.call(c, log_text)

  readr::write_csv(built$lgh, file.path(out_dir, "lgh.csv"))
}

write_2019_mca <- function(built, out_dir = ".") {
  cli::cat_rule("write_2019_mca()")
  readr::write_csv(built$mca, file.path(out_dir, "mca.csv"))
}

write_2019_mch <- function(built, out_dir = ".") {
  cli::cat_rule("write_2019_mch()")
  readr::write_csv(built$mch, file.path(out_dir, "mch.csv"))
}

write_2019_mci <- function(built, out_dir = ".") {
  cli::cat_rule("write_2019_mci()")
  readr::write_csv(built$mci, file.path(out_dir, "mci.csv"))
}

write_2019_pcm <- function(built, out_dir = ".") {
  cli::cat_rule("write_2019_pcm()")
  readr::write_csv(built$pcm, file.path(out_dir, "pcm.csv"))
}

write_2019_rdi <- function(built, out_dir = ".") {
  cli::cat_rule("write_2019_rdi()")

  # list columns need to be joined by whitespace before writing
  is_list <- vapply(built$rdi, is.list, logical(1))
  built$rdi[is_list] <- lapply(built$rdi[is_list], function(col) {
    vapply(col, paste0, collapse = " ", FUN.VALUE = character(1))
  })


  readr::write_csv(built$rdi, file.path(out_dir, "rdi.csv"))
}

read_2019_cached <- function(step, build_cache = bs_build_cache_dir("realtime"),
                             use_cache = TRUE, save_cache = TRUE) {
  cached_file <- file.path(build_cache, glue("{ step }.rds"))

  if (use_cache && file.exists(cached_file)) {
    cli::cat_line(glue("Loading previous '{ step }' from '{ build_cache }'"))
    previous <- readRDS(cached_file)
  } else {
    previous <- NULL
  }

  result <- switch(
    step,
    met = read_2019_met(previous),
    hpb = read_2019_hpb(previous),
    icl = read_2019_icl(previous),
    ips = read_2019_ips(previous),
    lgh = read_2019_lgh(previous),
    mca = read_2019_mca(previous),
    mch = read_2019_mch(previous),
    mci = read_2019_mci(previous),
    pcm = read_2019_pcm(previous),
    rdi = read_2019_rdi(previous),
    abort(glue("Unknown step: '{ step }'"))
  )

  if (save_cache) {
    cli::cat_line(glue("Saving cached '{ step }'"))
    if (!dir.exists(build_cache)) dir.create(build_cache, recursive = TRUE)
    saveRDS(result, cached_file)
  }

  result
}

# met here refers to environment canada hourly data from resolute
# these files aren't cached on the ftp server but are instead
# downloaded from environment canada
read_2019_met <- function(previous = NULL) {
  cli::cat_rule("read_2019_met()")

  if (identical(attr(previous, "date_generated"), Sys.Date())) {
    cli::cat_line(glue("Using `previous` as it was generated on { Sys.Date() }"))
    return(previous)
  }

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

  # this needs to be regenerated every day, so there is no point using
  # the file list as the cache key (also, there are rarely many of these files
  # and reading them is fast)
  attr(all, "date_generated") <- Sys.Date()

  all
}

read_2019_hpb <- function(previous = NULL) {
  cli::cat_rule("read_2019_hpb()")

  dir <- "BSRTO/2019-2020/hpb"
  cached <- build_2019_list_and_cache(dir)
  new_files <- cached[!(basename(cached) %in% attr(previous, "files"))]

  if (length(new_files) > 0) {
    cli::cat_line(
      glue(
        "Using previously read value for { length(attr(previous, 'files')) } files"
      )
    )
    build_2019_log_about_to_read(new_files)
    all <- read_hpb_vector(new_files)
    all$file <- build_2019_file_relative(all$file)
    all <- rbind(previous, all)
  } else {
    cli::cat_line("Using `previous` (no new files since last build)")
    all <- previous
  }

  build_2019_with_files_ref(all, basename(cached))
}

read_2019_icl <- function(previous = NULL) {
  cli::cat_rule("read_2019_icl()")

  dir <- "BSRTO/2019-2020/icl"
  cached <- build_2019_list_and_cache(dir)
  new_files <- cached[!(basename(cached) %in% attr(previous, "files"))]

  if (length(new_files) > 0) {
    cli::cat_line(
      glue(
        "Using previously read value for { length(attr(previous, 'files')) } files"
      )
    )
    build_2019_log_about_to_read(new_files)
    all <- read_icl_vector(new_files)
    all$file <- build_2019_file_relative(all$file)

    # there are a lot of malformed files...can check Time
    # Comment, and data_points columns for validity
    time_valid <- !is.na(all$Time)
    comment_valid <- all$Comment %in% c("", "Time Adjusted")
    data_points_valid <- is.finite(all$`Data Points`) & all$`Data Points` == 410
    rows_valid <- time_valid & comment_valid & data_points_valid

    build_2019_log_qc(all, rows_valid)

    all <- rbind(previous, all[rows_valid, ])
  } else {
    cli::cat_line("Using `previous` (no new files since last build)")
    all <- previous
  }

  build_2019_with_files_ref(all, basename(cached))
}

read_2019_ips <- function(previous = NULL) {
  cli::cat_rule("read_2019_ips()")

  dir <- "BSRTO/2019-2020/ips"
  cached <- build_2019_list_and_cache(dir)
  new_files <- cached[!(basename(cached) %in% attr(previous, "files"))]

  if (length(new_files) > 0) {
    cli::cat_line(
      glue(
        "Using previously read value for { length(attr(previous, 'files')) } files"
      )
    )

    build_2019_log_about_to_read(new_files)
    all <- read_ips_bn_vector(new_files)
    all$file <- build_2019_file_relative(all$file)

    all <- rbind(previous, all)
  } else {
    cli::cat_line("Using `previous` (no new files since last build)")
    all <- previous
  }

  build_2019_with_files_ref(all, basename(cached))
}

read_2019_lgh <- function(previous = NULL) {
  cli::cat_rule("read_2019_lgh()")

  dir <- "BSRTO/2019-2020/lgH"
  cached <- build_2019_list_and_cache(dir)
  new_files <- cached[!(basename(cached) %in% attr(previous, "files"))]

  if (length(new_files) > 0) {
    cli::cat_line(
      glue(
        "Using previously read value for { length(attr(previous, 'files')) } files"
      )
    )

    build_2019_log_about_to_read(new_files)
    all <- read_lgh_vector(new_files)
    all$file <- build_2019_file_relative(all$file)
    all <- rbind(previous, all)
  } else {
    cli::cat_line("Using `previous` (no new files since last build)")
    all <- previous
  }

  build_2019_with_files_ref(all, basename(cached))
}

read_2019_mca <- function(previous = NULL) {
  cli::cat_rule("read_2019_mca()")

  dir <- "BSRTO/2019-2020/mcA"
  cached <- build_2019_list_and_cache(dir)
  new_files <- cached[!(basename(cached) %in% attr(previous, "files"))]

  if (length(new_files) > 0) {
    cli::cat_line(
      glue(
        "Using previously read value for { length(attr(previous, 'files')) } files"
      )
    )

    build_2019_log_about_to_read(new_files)
    all <- read_mc_vector(new_files)
    all$file <- build_2019_file_relative(all$file)

    # basic QC to filter out mangled rows
    temp_valid <- !is.na(all$temperature) & (all$temperature > -20) &( all$temperature < 20)
    datetime_valid <- !is.na(all$date_time)
    rows_valid <- temp_valid & datetime_valid

    build_2019_log_qc(all, rows_valid)
    all <- rbind(previous, all[rows_valid, ])
  } else {
    all <- previous
  }

  build_2019_with_files_ref(all, basename(cached))
}

read_2019_mch <- function(previous = NULL) {
  cli::cat_rule("read_2019_mch()")

  dir <- "BSRTO/2019-2020/mcH"
  cached <- build_2019_list_and_cache(dir)
  new_files <- cached[!(basename(cached) %in% attr(previous, "files"))]

  if (length(new_files) > 0) {
    cli::cat_line(
      glue(
        "Using previously read value for { length(attr(previous, 'files')) } files"
      )
    )

    build_2019_log_about_to_read(new_files)
    all <- read_mc_vector(new_files)
    all$file <- build_2019_file_relative(all$file)

    # basic QC to filter out mangled rows
    temp_valid <- !is.na(all$temperature) & (all$temperature > -20) &( all$temperature < 20)
    datetime_valid <- !is.na(all$date_time)
    rows_valid <- temp_valid & datetime_valid

    build_2019_log_qc(all, rows_valid)
    all <- rbind(previous, all[rows_valid, ])
  } else {
    cli::cat_line("Using `previous` (no new files since last build)")
    all <- previous
  }

  build_2019_with_files_ref(all, basename(cached))
}

read_2019_mci <- function(previous = NULL) {
  cli::cat_rule("read_2019_mci()")

  dir <- "BSRTO/2019-2020/mcI"
  cached <- build_2019_list_and_cache(dir)
  new_files <- cached[!(basename(cached) %in% attr(previous, "files"))]

  if (length(new_files) > 0) {
    cli::cat_line(
      glue(
        "Using previously read value for { length(attr(previous, 'files')) } files"
      )
    )

    build_2019_log_about_to_read(new_files)
    all <- read_mc_vector(new_files)
    all$file <- build_2019_file_relative(all$file)

    # basic QC to filter out mangled rows
    temp_valid <- !is.na(all$temperature) & (all$temperature > -20) &( all$temperature < 20)
    datetime_valid <- !is.na(all$date_time)
    rows_valid <- temp_valid & datetime_valid

    build_2019_log_qc(all, rows_valid)
    all <- rbind(previous, all[rows_valid, ])
  } else {
    cli::cat_line("Using `previous` (no new files since last build)")
    all <- previous
  }

  build_2019_with_files_ref(all, basename(cached))
}

read_2019_pcm <- function(previous = NULL) {
  cli::cat_rule("read_2019_pcm()")

  dir <- "BSRTO/2019-2020/pcm"
  cached <- build_2019_list_and_cache(dir)
  new_files <- cached[!(basename(cached) %in% attr(previous, "files"))]

  if (length(new_files) > 0) {
    cli::cat_line(
      glue(
        "Using previously read value for { length(attr(previous, 'files')) } files"
      )
    )

    build_2019_log_about_to_read(new_files)
    all <- read_pcm_vector(new_files)
    all$file <- build_2019_file_relative(all$file)

    # basic QC to filter out mangled rows
    rows_valid <- all$checksum_valid
    build_2019_log_qc(all, rows_valid)
    all <- rbind(previous, all[rows_valid, ])
  } else {
    cli::cat_line("Using `previous` (no new files since last build)")
    all <- previous
  }

  build_2019_with_files_ref(all, basename(cached))
}

read_2019_rdi <- function(previous = NULL) {
  cli::cat_rule("read_2019_rdi()")

  dir <- "BSRTO/2019-2020/rdi"
  cached <- build_2019_list_and_cache(dir)
  new_files <- cached[!(basename(cached) %in% attr(previous, "files"))]

  if (length(new_files) > 0) {
    cli::cat_line(
      glue(
        "Using previously read value for { length(attr(previous, 'files')) } files"
      )
    )

    build_2019_log_about_to_read(new_files)
    all <- read_rdi_vector(new_files)
    all$file <- build_2019_file_relative(all$file)

    # use 'date_time' instead of 'real_time_clock' like the others
    names(all)[names(all) == "real_time_clock"] <- "date_time"

    # at least one row is missing values for the data sections
    rows_valid <- !vapply(all$range_msb, is.null, logical(1))
    build_2019_log_qc(all, rows_valid)
    all <- rbind(previous, all[rows_valid, ])
  } else {
    cli::cat_line("Using `previous` (no new files since last build)")
    all <- previous
  }

  build_2019_with_files_ref(all, basename(cached))
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

build_2019_with_files_ref <- function(all, files) {
  attr(all, "files") <- files
  all
}
