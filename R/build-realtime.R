
#' Build real-time data from the 2019 deployment
#'
#' The build of real-time data is split into multiple steps that allow
#' certain types of changes to be applied without unnecessary loading
#' of files. In the code these are split into "read" and "write" functions.
#'
#' Read functions are concerned with taking raw data files and filtering
#' out data that is corrupted or otherwise unreadable. These functions also
#' check for new files and download them if they aren't present locally.
#' These functions add to the previously calculated version present in the
#' build cache, which keeps the processing time to a minimum and keeps the
#' build logs from being cluttered with parse errors from unreadable files
#' that have already been parsed months ago. Note that the value of
#' [bs_cache_dir()] and [bs_build_cache_dir()] are used to determine where
#' FTP downloads and intermediary build files are stored.
#'
#' Whereas the output of read functions is generally stable, methods to
#' flag bad measurements and perform corrections that require data from
#' multiple sensors can and should be updated frequently. In the code these
#' are grouped as "write" functions. These calculations are rarely expensive
#' and thus the result is not cached.
#'
#' @param out_dir The directory in which output files should be
#'   generated.
#'
#' @return `out_dir`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' bs_build_realtime()
#' }
#'
bs_build_realtime <- function(out_dir = ".") {
  steps <- c(
    "met", "hpb", "icl", "ips", "lgh",
    "mca", "mch", "mci", "pcm", "rdi"
  )
  names(steps) <- steps
  built <- lapply(steps, read_realtime_cached)

  # Here `built` mutable, so changes made by subsequent steps
  # are available for the following steps. This is useful to keep functions
  # small but means they are less isolated.
  # TODO: make explicit the objects each step needs and have each step
  # return anything needed by subsequent steps
  built <- as.environment(built)

  # these processed values are used as corrections in later outputs
  baro <- write_realtime_baro(built, out_dir)
  met <- write_realtime_met(built, out_dir)
  pc <- write_realtime_pcm(built, out_dir)

  write_realtime_adp(built, out_dir)

  write_realtime_icl(built, out_dir)
  write_realtime_ips(built, out_dir)
  write_realtime_lgh(built, out_dir)
  write_realtime_mc(built, out_dir)


  invisible(out_dir)
}



write_realtime_baro <- function(built, out_dir = ".") {
  cli::cat_rule("write_realtime_baro()")

  # same naming convention and units as environment canada values
  baro <- tibble::tibble(
    file = built$hpb$file,
    date_time = built$hpb$date_time,
    shore_press = built$hpb$atm_pres_mbar / 10, # mbar -> kPa
    shore_press_flag = NA_character_,
    shore_temp = built$hpb$temp_c,
    shore_temp_flag = NA_character_
  )

  readr::write_csv(baro, file.path(out_dir, "baro.csv"))
}

write_realtime_met <- function(built, out_dir = ".") {
  cli::cat_rule("write_realtime_met()")

  # Resolute station altitude is 68 m above sea level
  met <- built$met
  met$sea_level_press <-
    sea_level_pressure_from_barometric(built$met$stn_press, 68)
  met$sea_level_press_flag <- built$met$stn_press_flag

  # check correction
  # plot(built$met$stn_press, built$met$sea_level_press)

  # give every variable a _flag column
  met$weather_flag <- NA_character_

  readr::write_csv(met, file.path(out_dir, "met.csv"))
}

write_realtime_pcm <- function(built, out_dir = ".") {
  cli::cat_rule("write_realtime_pcm()")

  # https://github.com/richardsc/bsrto/blob/master/pc.R#L28-L45

  readr::write_csv(built$pcm, file.path(out_dir, "pcm.csv"))
}

write_realtime_adp <- function(built, out_dir = ".") {
  cli::cat_rule("write_realtime_adp()")

  # https://github.com/richardsc/bsrto/blob/master/adp.R#L32-L94

  # list columns need to be joined by whitespace before writing
  is_list <- vapply(built$rdi, is.list, logical(1))
  built$rdi[is_list] <- lapply(built$rdi[is_list], function(col) {
    vapply(col, paste0, collapse = " ", FUN.VALUE = character(1))
  })


  readr::write_csv(built$rdi, file.path(out_dir, "rdi.csv"))
}

write_realtime_icl <- function(built, out_dir = ".") {
  cli::cat_rule("write_realtime_icl()")

  # This data is exportable as .csv but fits more naturally as a NetCDF
  # Use column names and flag conventions following that of Env Canada
  # climate data

  # Need to make sure each row represents a unique date/time or the
  # netCDF magic below won't work. These are all (as far as I can tell)
  # a result of mangled files. While there's no guarantee that the first
  # non-duplicated date-time is the valid one, it's the most likely. This
  # only represents ~40 rows.
  icl <- built$icl
  duplicated_date_times <- duplicated(icl$Time)

  # flag wildly out-of-range values (mangled data, not bad measurements)
  temp_out_of_range <- (icl$`Temperature [C]` > 10) | (icl$`Temperature [C]` < -10)
  hum_out_of_range <- (icl$`Humidity [%]` > 100) | (icl$`Humidity [%]` < 0)

  # not logging because this should be moved to the read_icl_realtime function
  rows_invalid <- duplicated_date_times | temp_out_of_range | hum_out_of_range
  icl <- icl[!rows_invalid, ]

  # separate meta information for now
  icl_meta <- tibble::tibble(
    file = icl$file,
    date_time = icl$Time,
    icl_temp = icl$`Temperature [C]`,
    icl_temp_flag = 0L,
    icl_rel_hum = icl$`Humidity [%]`,
    icl_rel_hum_flag = 0L
  )

  # separate spectra
  spec_wide <- icl[grepl("^[0-9.]+$", names(icl))]
  frequencies <- as.numeric(names(spec_wide))
  # using t() here to keep values from the same spectrum together
  # rather than values from the same frequency (because matrices are
  # column-major in R)
  intensity <- as.numeric(t(as.matrix(spec_wide)))

  # flag suspected bad intensity values using int type
  # (because we're headed to NetCDF where character is hard)
  intensity_flag <- as.integer(
    (intensity > 500) |
      (intensity < -50) |
      (suppressWarnings(intensity %% 1) != 0)
  )
  intensity_flag[is.na(intensity)] <- 2L


  # define NetCDF dimensions and variables
  dim_date_time <- ncdf4::ncdim_def(
    "date_time",
    units = "seconds since 1970-01-01 00:00:00 UTC",
    vals = as.numeric(icl_meta$date_time, origin = "1970-01-01 00:00:00")
  )

  dim_frequency <- ncdf4::ncdim_def(
    "frequency",
    units = "Hz",
    vals = frequencies
  )

  dim_string23 <- ncdf4::ncdim_def(
    "string23",
    units = "count",
    vals = 1:23
  )

  # create NetCDF
  nc <- ncdf4::nc_create(
    file.path(out_dir, "icl.nc"),
    list(
      ncdf4::ncvar_def(
        "file",
        units = "character",
        dim = list(dim_string23, dim_date_time),
        longname = "Source filename",
        prec = "char"
      ),
      ncdf4::ncvar_def(
        "icl_temp",
        units = "Degrees C",
        dim = list(dim_date_time),
        longname = "Operating temperature",
        prec = "float"
      ),
      ncdf4::ncvar_def(
        "icl_temp_flag",
        units = "Non-zero for possible bad data",
        dim = list(dim_date_time),
        prec = "short"
      ),
      ncdf4::ncvar_def(
        "icl_rel_hum",
        units = "%",
        dim = list(dim_date_time),
        longname = "Operating relative humidity",
        prec = "float"
      ),
      ncdf4::ncvar_def(
        "icl_rel_hum_flag",
        units = "Non-zero for possible bad data",
        dim = list(dim_date_time),
        prec = "short"
      ),
      ncdf4::ncvar_def(
        "icl_intensity",
        units = "Relative intensity",
        dim = list(dim_date_time, dim_frequency),
        prec = "integer"
      ),
      ncdf4::ncvar_def(
        "icl_intensity_flag",
        units = "Non-zero for possible bad data",
        dim = list(dim_date_time, dim_frequency),
        prec = "short"
      )
    )
  )
  on.exit(ncdf4::nc_close(nc))

  ncdf4::ncvar_put(nc, "file", icl_meta$file)
  ncdf4::ncvar_put(nc, "icl_temp", icl_meta$icl_temp)
  ncdf4::ncvar_put(nc, "icl_temp_flag", icl_meta$icl_temp_flag)
  ncdf4::ncvar_put(nc, "icl_rel_hum", icl_meta$icl_rel_hum)
  ncdf4::ncvar_put(nc, "icl_rel_hum_flag", icl_meta$icl_rel_hum_flag)
  ncdf4::ncvar_put(nc, "icl_intensity", as.integer(intensity))
  ncdf4::ncvar_put(nc, "icl_intensity_flag", intensity_flag)

}

write_realtime_ips <- function(built, out_dir = ".") {
  cli::cat_rule("write_realtime_ips()")

  # https://github.com/richardsc/bsrto/blob/master/ips.R#L74-L94
  # https://github.com/richardsc/bsrto/blob/master/save_data_file.R#L15

  # bins is a list-col: join by whitespace
  built$ips$bins <- vapply(built$ips$bins, paste0, collapse = " ", FUN.VALUE = character(1))

  readr::write_csv(built$ips, file.path(out_dir, "ips.csv"))
}

write_realtime_lgh <- function(built, out_dir = ".") {
  cli::cat_rule("write_realtime_lgh()")

  # no processing of these in current app

  # log_text is a list-col, but we can unnest it
  log_text <- built$lgh$log_text
  lengths <- vapply(log_text, length, integer(1))
  built$lgh$log_text <- NULL
  built$lgh <- vctrs::vec_rep_each(built$lgh, lengths)
  built$lgh$log_text <- do.call(c, log_text)

  readr::write_csv(built$lgh, file.path(out_dir, "lgh.csv"))
}

write_realtime_mc <- function(built, out_dir = ".") {
  cli::cat_rule("write_realtime_mc()")

  # https://github.com/richardsc/bsrto/blob/master/mc.R#L55-L72

  readr::write_csv(built$mca, file.path(out_dir, "mca.csv"))
  readr::write_csv(built$mch, file.path(out_dir, "mch.csv"))
  readr::write_csv(built$mci, file.path(out_dir, "mci.csv"))
}

read_realtime_cached <- function(step, build_cache = bs_build_cache_dir("realtime"),
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
    met = read_realtime_met(previous),
    hpb = read_realtime_hpb(previous),
    icl = read_realtime_icl(previous),
    ips = read_realtime_ips(previous),
    lgh = read_realtime_lgh(previous),
    mca = read_realtime_mca(previous),
    mch = read_realtime_mch(previous),
    mci = read_realtime_mci(previous),
    pcm = read_realtime_pcm(previous),
    rdi = read_realtime_rdi(previous),
    abort(glue("Unknown step: '{ step }'"))
  )

  # nice for build logs to have a glimpse of the raw outputs
  cli::cat_rule(glue("[built${ step }]"))
  print(tibble::as_tibble(result))
  cli::cat_rule(glue("[/built${ step }]"))

  if (save_cache) {
    cli::cat_line(glue("Saving cached '{ step }'"))
    if (!dir.exists(build_cache)) dir.create(build_cache, recursive = TRUE)
    # compression doesn't make a difference with speed here but makes a huge
    # difference with the size of the cache
    saveRDS(result, cached_file)
  }

  result
}

# met here refers to environment canada hourly data from resolute
# these files aren't cached on the ftp server but are instead
# downloaded from environment canada
read_realtime_met <- function(previous = NULL) {
  cli::cat_rule("read_realtime_met()")

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

  build_realtime_log_about_to_read(ec_files$dest)
  all <- read_ec_climate_hourly_vector(ec_files$dest, utc_offset = -6)
  all$file <- build_realtime_file_relative(all$file)
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

read_realtime_hpb <- function(previous = NULL) {
  cli::cat_rule("read_realtime_hpb()")

  dir <- "BSRTO/2019-2020/hpb"
  cached <- build_realtime_list_and_cache(dir)
  new_files <- cached[!(basename(cached) %in% attr(previous, "files"))]

  if (length(new_files) > 0) {
    cli::cat_line(
      glue(
        "Using previously read value for { length(attr(previous, 'files')) } files"
      )
    )
    build_realtime_log_about_to_read(new_files)
    all <- read_hpb_vector(new_files)
    all$file <- build_realtime_file_relative(all$file)
    all <- rbind(previous, all)
  } else {
    cli::cat_line("Using `previous` (no new files since last build)")
    all <- previous
  }

  build_realtime_with_files_ref(all, basename(cached))
}

read_realtime_icl <- function(previous = NULL) {
  cli::cat_rule("read_realtime_icl()")

  dir <- "BSRTO/2019-2020/icl"
  cached <- build_realtime_list_and_cache(dir)
  new_files <- cached[!(basename(cached) %in% attr(previous, "files"))]

  if (length(new_files) > 0) {
    cli::cat_line(
      glue(
        "Using previously read value for { length(attr(previous, 'files')) } files"
      )
    )
    build_realtime_log_about_to_read(new_files)
    all <- read_icl_vector(new_files)
    all$file <- build_realtime_file_relative(all$file)

    # there are a lot of malformed files...can check Time
    # Comment, and data_points columns for validity
    time_valid <- !is.na(all$Time)
    comment_valid <- all$Comment %in% c("", "Time Adjusted")
    data_points_valid <- is.finite(all$`Data Points`) & all$`Data Points` == 410
    rows_valid <- time_valid & comment_valid & data_points_valid

    build_realtime_log_qc(all, rows_valid)

    all <- rbind(previous, all[rows_valid, ])
  } else {
    cli::cat_line("Using `previous` (no new files since last build)")
    all <- previous
  }

  build_realtime_with_files_ref(all, basename(cached))
}

read_realtime_ips <- function(previous = NULL) {
  cli::cat_rule("read_realtime_ips()")

  dir <- "BSRTO/2019-2020/ips"
  cached <- build_realtime_list_and_cache(dir)
  new_files <- cached[!(basename(cached) %in% attr(previous, "files"))]

  if (length(new_files) > 0) {
    cli::cat_line(
      glue(
        "Using previously read value for { length(attr(previous, 'files')) } files"
      )
    )

    build_realtime_log_about_to_read(new_files)
    all <- read_ips_bn_vector(new_files)
    all$file <- build_realtime_file_relative(all$file)

    all <- rbind(previous, all)
  } else {
    cli::cat_line("Using `previous` (no new files since last build)")
    all <- previous
  }

  build_realtime_with_files_ref(all, basename(cached))
}

read_realtime_lgh <- function(previous = NULL) {
  cli::cat_rule("read_realtime_lgh()")

  dir <- "BSRTO/2019-2020/lgH"
  cached <- build_realtime_list_and_cache(dir)
  new_files <- cached[!(basename(cached) %in% attr(previous, "files"))]

  if (length(new_files) > 0) {
    cli::cat_line(
      glue(
        "Using previously read value for { length(attr(previous, 'files')) } files"
      )
    )

    build_realtime_log_about_to_read(new_files)
    all <- read_lgh_vector(new_files)
    all$file <- build_realtime_file_relative(all$file)
    all <- rbind(previous, all)
  } else {
    cli::cat_line("Using `previous` (no new files since last build)")
    all <- previous
  }

  build_realtime_with_files_ref(all, basename(cached))
}

read_realtime_mca <- function(previous = NULL) {
  cli::cat_rule("read_realtime_mca()")

  dir <- "BSRTO/2019-2020/mcA"
  cached <- build_realtime_list_and_cache(dir)
  new_files <- cached[!(basename(cached) %in% attr(previous, "files"))]

  if (length(new_files) > 0) {
    cli::cat_line(
      glue(
        "Using previously read value for { length(attr(previous, 'files')) } files"
      )
    )

    build_realtime_log_about_to_read(new_files)
    all <- read_mc_vector(new_files)
    all$file <- build_realtime_file_relative(all$file)

    # basic QC to filter out mangled rows
    temp_valid <- !is.na(all$temperature) & (all$temperature > -20) &( all$temperature < 20)
    datetime_valid <- !is.na(all$date_time)
    rows_valid <- temp_valid & datetime_valid

    build_realtime_log_qc(all, rows_valid)
    all <- rbind(previous, all[rows_valid, ])
  } else {
    all <- previous
  }

  build_realtime_with_files_ref(all, basename(cached))
}

read_realtime_mch <- function(previous = NULL) {
  cli::cat_rule("read_realtime_mch()")

  dir <- "BSRTO/2019-2020/mcH"
  cached <- build_realtime_list_and_cache(dir)
  new_files <- cached[!(basename(cached) %in% attr(previous, "files"))]

  if (length(new_files) > 0) {
    cli::cat_line(
      glue(
        "Using previously read value for { length(attr(previous, 'files')) } files"
      )
    )

    build_realtime_log_about_to_read(new_files)
    all <- read_mc_vector(new_files)
    all$file <- build_realtime_file_relative(all$file)

    # basic QC to filter out mangled rows
    temp_valid <- !is.na(all$temperature) & (all$temperature > -20) &( all$temperature < 20)
    datetime_valid <- !is.na(all$date_time)
    rows_valid <- temp_valid & datetime_valid

    build_realtime_log_qc(all, rows_valid)
    all <- rbind(previous, all[rows_valid, ])
  } else {
    cli::cat_line("Using `previous` (no new files since last build)")
    all <- previous
  }

  build_realtime_with_files_ref(all, basename(cached))
}

read_realtime_mci <- function(previous = NULL) {
  cli::cat_rule("read_realtime_mci()")

  dir <- "BSRTO/2019-2020/mcI"
  cached <- build_realtime_list_and_cache(dir)
  new_files <- cached[!(basename(cached) %in% attr(previous, "files"))]

  if (length(new_files) > 0) {
    cli::cat_line(
      glue(
        "Using previously read value for { length(attr(previous, 'files')) } files"
      )
    )

    build_realtime_log_about_to_read(new_files)
    all <- read_mc_vector(new_files)
    all$file <- build_realtime_file_relative(all$file)

    # basic QC to filter out mangled rows
    temp_valid <- !is.na(all$temperature) & (all$temperature > -20) &( all$temperature < 20)
    datetime_valid <- !is.na(all$date_time)
    rows_valid <- temp_valid & datetime_valid

    build_realtime_log_qc(all, rows_valid)
    all <- rbind(previous, all[rows_valid, ])
  } else {
    cli::cat_line("Using `previous` (no new files since last build)")
    all <- previous
  }

  build_realtime_with_files_ref(all, basename(cached))
}

read_realtime_pcm <- function(previous = NULL) {
  cli::cat_rule("read_realtime_pcm()")

  dir <- "BSRTO/2019-2020/pcm"
  cached <- build_realtime_list_and_cache(dir)
  new_files <- cached[!(basename(cached) %in% attr(previous, "files"))]

  if (length(new_files) > 0) {
    cli::cat_line(
      glue(
        "Using previously read value for { length(attr(previous, 'files')) } files"
      )
    )

    build_realtime_log_about_to_read(new_files)
    all <- read_pcm_vector(new_files)
    all$file <- build_realtime_file_relative(all$file)

    # basic QC to filter out mangled rows
    rows_valid <- all$checksum_valid
    build_realtime_log_qc(all, rows_valid)
    all <- rbind(previous, all[rows_valid, ])
  } else {
    cli::cat_line("Using `previous` (no new files since last build)")
    all <- previous
  }

  build_realtime_with_files_ref(all, basename(cached))
}

read_realtime_rdi <- function(previous = NULL) {
  cli::cat_rule("read_realtime_rdi()")

  dir <- "BSRTO/2019-2020/rdi"
  cached <- build_realtime_list_and_cache(dir)
  new_files <- cached[!(basename(cached) %in% attr(previous, "files"))]

  if (length(new_files) > 0) {
    cli::cat_line(
      glue(
        "Using previously read value for { length(attr(previous, 'files')) } files"
      )
    )

    build_realtime_log_about_to_read(new_files)
    all <- read_rdi_vector(new_files)
    all$file <- build_realtime_file_relative(all$file)

    # use 'date_time' instead of 'real_time_clock' like the others
    names(all)[names(all) == "real_time_clock"] <- "date_time"

    # at least one row is missing values for the data sections
    rows_valid <- !vapply(all$range_msb, is.null, logical(1))
    build_realtime_log_qc(all, rows_valid)
    all <- rbind(previous, all[rows_valid, ])
  } else {
    cli::cat_line("Using `previous` (no new files since last build)")
    all <- previous
  }

  build_realtime_with_files_ref(all, basename(cached))
}

build_realtime_log_about_to_read <- function(cached) {
  cli::cat_line(glue("Reading { length(cached) } file(s)"))
  if (length(cached) >= 2) {
    cli::cat_line(glue("'{ basename(cached[1]) }'...'{ basename(cached[length(cached)]) }'"))
  } else if (length(cached) >= 1) {
    cli::cat_line(glue("'{ basename(cached) }'"))
  }
}

build_realtime_log_qc <- function(all, rows_valid) {
  files_with_errors <- unique(all$file[!rows_valid])

  cli::cat_line(
    glue("Removing { sum(!rows_valid) } unreadable rows ({ round(mean(!rows_valid) * 100, 1)}%)")
  )
  cli::cat_line(
    glue("{ length(files_with_errors) }/{ nrow(all) } files had errors")
  )
}

build_realtime_list_and_cache <- function(dir, retries = 4) {
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

  summary_size <- build_realtime_friendly_file_size(sum(files$size))
  cli::cat_line(glue("Summary: { nrow(files) } file(s) ({ summary_size })"))

  needs_download <- !file.exists(bs_cache_dir(files$file))
  if (any(needs_download)) {
    download_size <- build_realtime_friendly_file_size(sum(files$size[needs_download]))
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

build_realtime_file_relative <- function(file) {
  basename(file)
}

build_realtime_friendly_file_size <- function(size) {
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

build_realtime_with_files_ref <- function(all, files) {
  attr(all, "files") <- files
  all
}
