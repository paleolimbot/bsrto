
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
  # make sure out_dir exists
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  steps <- c(
    "met", "hpb", "icl", "ips", "lgh",
    "mca", "mch", "mci", "pcm", "rdi"
  )
  names(steps) <- steps
  built <- lapply(steps, read_realtime_cached)

  # these processed values are used as corrections/qc in later outputs
  met_clean <- write_realtime_met(built$met, out_dir)
  baro <- write_realtime_baro(built$hpb, met_clean, out_dir)
  pc <- write_realtime_pcm(built$pcm, out_dir)
  mc <- write_realtime_mc(built[c("mca", "mch", "mci")], out_dir)

  write_realtime_adp(built$rdi, pc, out_dir)
  write_realtime_icl(built$icl, out_dir)
  write_realtime_ips(built$ips, baro, out_dir)
  write_realtime_lgh(built$lgh, out_dir)

  invisible(out_dir)
}

# need to be careful to sync this with above...allows a workflow like
# devtools::load_all(".")
# bs_build_interactive()
# ... (step through any code in this file)
bs_build_interactive <- function(out_dir = ".", .env = parent.frame()) {
  .env$out_dir <- out_dir

  # useful for stepping through read_* functions()
  .env$previous <- NULL

  # make sure out_dir exists
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  steps <- c(
    "met", "hpb", "icl", "ips", "lgh",
    "mca", "mch", "mci", "pcm", "rdi"
  )
  names(steps) <- steps
  .env$built <- lapply(steps, read_realtime_cached)
  for (step in steps) {
    .env[[step]] <- .env$built[[step]]
  }

  # these processed values are used as corrections in later outputs
  .env$met_clean <- write_realtime_met(.env$built$met, out_dir)
  .env$baro <- write_realtime_baro(.env$built$hpb, .env$met_clean, out_dir)
  .env$pc <- write_realtime_pcm(.env$built$pcm, out_dir)
  .env$mc <- write_realtime_mc(.env$built[c("mca", "mch", "mci")], out_dir)
}

write_realtime_met <- function(met, out_dir = ".") {
  cli::cat_rule("write_realtime_met()")

  # Use dbar for pressure everywhere
  met$stn_press <- met$stn_press / 10 # kPa -> dbar

  # Resolute station altitude is 68 m above sea level
  met$sea_level_press <-
    sea_level_pressure_from_barometric(met$stn_press, 68)
  met$sea_level_press_flag <- met$stn_press_flag

  # check correction
  # plot(built$met$stn_press, built$met$sea_level_press)

  # remove columns where nothing has ever been measured
  no_data_cols <- c("precip_amount", "visibility", "hmdx", "weather")
  no_data_flag_cols <- paste0(setdiff(no_data_cols, "weather"), "_flag")
  met <- met[setdiff(names(met), c(no_data_cols, no_data_flag_cols))]

  # Re-flag data with the bs_flag() scheme. The only flags so far in the
  # weather data are NA and "M" (missing). Be conservative and call any other
  # flagged data "probably bad data".
  flag_cols <- grepl("_flag$", names(met))
  met[flag_cols] <- lapply(met[flag_cols], function(x) {
    result <- ifelse(x == "M", bs_flag("missing"), bs_flag("probably bad data"))
    result[is.na(result)] <- bs_flag("probably good data")
    result
  })

  out_file <- file.path(out_dir, "met.csv")
  cli::cat_line(glue("Writing '{ out_file }'"))

  readr::write_csv(met, out_file)
}

write_realtime_baro <- function(hpb, met_clean, out_dir = ".") {
  cli::cat_rule("write_realtime_baro()")

  # same naming convention as environment canada values,
  # use dbar for pressure everywhere
  baro <- tibble::tibble(
    file = hpb$file,
    date_time = hpb$date_time,
    shore_press = hpb$atm_pres_mbar / 100, # mbar -> dbar
    shore_temp = hpb$temp_c,
  )

  # QC pressure and temperature using Env. Canada observations
  # Shore station pressures are within 0.10 to 0.22 of estimated
  # sea-level pressure from stn_press at Resolute.
  met_nearest_press <- resample_nearest(
    met_clean$date_time,
    met_clean$sea_level_press,
    baro$date_time,
    # only observations within 30 mins
    max_distance = 60 * 30
  )
  met_press_diff <- baro$shore_press - met_nearest_press

  baro$shore_press_flag <- ifelse(
    met_press_diff > 0.23 | met_press_diff < 0.1,
    bs_flag("probably bad data"),
    bs_flag("probably good data")
  )

  # temperature is difficult to QC in this way...no obvious outliers
  # but up to 20 degrees difference
  baro$shore_temp_flag <- bs_flag("not assessed")

  out_file <- file.path(out_dir, "baro.csv")
  cli::cat_line(glue("Writing '{ out_file }'"))

  readr::write_csv(baro, out_file)
}

write_realtime_pcm <- function(pcm, out_dir = ".") {
  cli::cat_rule("write_realtime_pcm()")

  # Zero readings are suspected to be bad readings and often are
  # there are so many measurements that removing them doesn't impact
  # data quality
  pcm$zero_heading <- pcm$heading_magnetic == 0

  # Summarise to once per file (roughly once every two hours)
  # there are ~14-20 measurements per file, but they seem to be clumped
  # such that I'm not sure one can assume equal spacing of the measurements
  # over a two-hour period. Assumption here is that they are representative
  # of the start time of the file.
  pc <- pcm %>%
    group_by(.data$file) %>%
    summarise(
      date_time = .data$last_date_time[1],
      pc_heading_sd = headings::hdg_sd(.data$heading_magnetic[!.data$zero_heading]),
      pc_heading = headings::hdg_mean(.data$heading_magnetic[!.data$zero_heading]),
      pc_true_heading = headings::hdg_norm(
        .data$pc_heading + barrow_strait_declination(.data$date_time)
      ),
      pc_heading_n = sum(!.data$zero_heading),
      .groups = "drop"
    ) %>%
    mutate(
      pc_heading_flag = bs_flag("probably good data")
    )

  out_file <- file.path(out_dir, "pcm_summary.csv")
  cli::cat_line(glue("Writing '{ out_file }'"))

  readr::write_csv(pc, out_file)
}


write_realtime_mc <- function(built, out_dir = ".") {
  cli::cat_rule("write_realtime_mc()")

  mca <- built$mca
  mch <- built$mch
  mci <- built$mci

  # add label information for each mcX
  # https://github.com/richardsc/bsrto/blob/master/server.R#L54-L65
  mca$depth_label <- 60
  mch$depth_label <- 160
  mci$depth_label <- 40

  # Add flag columns for "missing" here, because after we rbind there will
  # be implicit missings for parameters that were not measured by one of
  # the instruments. There are few if any missing values but may be some
  # resulting from reading mangled files from which only a few values
  # are available.
  mc_list <- list(mca, mch, mci)
  mc_list <- lapply(mc_list, function(mcx) {
    cols <- setdiff(names(mcx), c("file", "date_time", "depth_label"))
    flag_cols <- paste0(cols, "_flag")
    mcx[flag_cols] <- lapply(
      mcx[cols], function(x)
        ifelse(is.na(x), bs_flag("missing"), bs_flag("not assessed"))
    )
    mcx
  })

  # combine all the CTD measurements
  mc <- vctrs::vec_rbind(!!! mc_list) %>%
    dplyr::arrange(.data$depth_label, .data$date_time)

  # calculate salinity and sound speed (only reported by some instruments)
  mc$salinity_calc <- salinity_from_cond_temp_pres(
    mc$conductivity,
    mc$temperature,
    mc$pressure
  )
  mc$salinity_calc_flag <- bs_flag("not assessed")
  # RMSE: ~0.01
  stopifnot(
    mean(
      (mc$salinity - mc$salinity_calc) ^ 2,
      na.rm = TRUE
    ) < 0.02
  )

  mc$sound_speed_calc <- sound_speed_from_psal_temp_pres(
    mc$salinity_calc,
    mc$temperature,
    mc$pressure
  )
  # Check ballpark values (mostly as a guard against unit problems)
  stopifnot(
    mean(
      (mc$sound_speed - mc$sound_speed_calc) ^ 2,
      na.rm = TRUE
    ) < 1
  )
  mc$sound_speed_calc_flag <- bs_flag("not assessed")

  # flag out-of-range values for some parameters
  temp_out_of_range <- (mc$temperature < -2) | (mc$temperature > 20)
  psal_out_of_range <- (mc$salinity < 1) | (mc$salinity > 40)
  psal_calc_out_of_range <- (mc$salinity_calc < 1) | (mc$salinity_calc > 40)

  mc$temperature_flag <- replace(
    mc$temperature_flag,
    temp_out_of_range,
    bs_flag("probably bad data")
  )

  mc$salinity_flag <- replace(
    mc$salinity_flag,
    psal_out_of_range,
    bs_flag("probably bad data")
  )

  mc$salinity_calc_flag <- replace(
    mc$salinity_calc_flag,
    psal_calc_out_of_range,
    bs_flag("probably bad data")
  )

  out_file <- file.path(out_dir, "ctd.csv")
  cli::cat_line(glue("Writing '{ out_file }'"))

  readr::write_csv(mc, out_file)
}

write_realtime_adp <- function(rdi, pc, out_dir = ".") {
  cli::cat_rule("write_realtime_adp()")

  # Validate assumptions regarding coordinate transformation.
  # The config for the 2019 deployment exports coordinates as
  # starboard-forward-mast such that "mast" is directly upward
  # (as indicated by tilt_used == TRUE). This simplifies the
  # calculation significantly since it need only account for
  # a 2d affine transformation in the horizontal dimensions.
  rdi_coord <- readrdi::rdi_unpack_coord_transform(rdi$coord_transform)
  stopifnot(
    all(rdi_coord$coord_system == "sfm"),
    all(rdi_coord$tilt_used)
  )

  # Declare variables ----

  # These columns had a constant value between the start of the
  # deployment and 2021-02-01.
  cols_config <- c(
    "firmware_version", "system_config", "real_sim_flag", "lag_length",
    "n_beams", "n_cells", "pings_per_ensemble", "cell_size", "blank_after_transmit",
    "profiling_mode", "low_corr_thresh", "n_code_reps", "pct_gd_min",
    "error_velocity_maximum", "tpp_minutes", "tpp_seconds", "tpp_hundredths",
    "coord_transform", "heading_alignment", "heading_bias", "sensor_source",
    "sensors_available", "wp_ref_layer_average", "false_target_threshold",
    "transmit_lag_distance", "cpu_board_serial_number", "system_bandwidth",
    "system_power", "serial_number", "beam_angle",
    # Treating bin1_distance as a constant
    # (6.02) even though it is 6.03 in a handful of profiles. Treating
    # transmit_pulse_length as a constant (4.06) even though it can be
    # 4.05 or 4.07 in a handful of profiles.
    "bin1_distance", "transmit_pulse_length"
  )

  # These columns are list(matrix(n_beams * n_cells))
  cols_n_beams_n_cells <- c("correlation", "echo_intensity", "pct_good")

  # These columns are list(c(n_beams))
  cols_n_beams <- c(
    "bottom_range", "bottom_correlation",
    "bottom_amplitude", "bottom_pct_good"
  )

  # Velocities need to be rotated and including them along the `n_beam`
  # dimension is disingenuous, since the values are starboard, forward mast,
  # error velocity, and will be east, north, up, error velocity.
  cols_velocity <- c("velocity", "bottom_velocity")

  # All other columns have one value per profile
  cols_prof_meta <- setdiff(
    names(rdi),
    c(
      cols_config, cols_n_beams_n_cells, cols_n_beams,
      "data_offset", "data_type", "velocity", "bottom_velocity"
    )
  )

  # Make objects ----

  rdi_config <- rdi[1, cols_config]
  rdi_meta <- rdi[cols_prof_meta]

  # These will be along date_time, n_beams, n_cells
  rdi_n_beams_n_cells <- lapply(
    rdi[cols_n_beams_n_cells],
    abind::abind, along = 0
  )

  # These will be along date_time, n_beams
  rdi_n_beams <- lapply(rdi[cols_n_beams], abind::abind, along = 0)

  # Distance is a more meaningful way to dimension along n_cells
  n_cell_distance <- seq(
    rdi_config$bin1_distance,
    by = rdi_config$cell_size,
    length.out = rdi_config$n_cells
  )

  # Apply corrections! ----

  # Get the nearest pole compass true heading
  pc_true_heading_interp <- resample_nearest(
    pc$date_time,
    pc$pc_true_heading,
    rdi_meta$date_time,
    max_distance = 60 * 10 # only use values within 10 minutes
  )

  # correct for beam alignment to the pole compass
  rdi_meta$beam_heading_corrected <- headings::hdg_norm(pc_true_heading_interp + 45)

  # compare to heading from rdi file...you can theoretically predict the error
  # based on the original heading (bumped slightly for continuity)

  # heading_orig <- ifelse(rdi_meta$heading > 240, rdi_meta$heading - 360, rdi_meta$heading)
  # beam_heading_diff <- heading_diff(rdi_meta$beam_heading_corrected, heading_orig)
  # plot(heading_orig, beam_heading_diff)
  # fit <- lm(beam_heading_diff ~ poly(heading_orig, 5), na.action = na.exclude)
  # vals <- tibble::tibble(heading_orig, pred = predict(fit))
  # lines(vals[order(vals$heading_orig), ], col = "red")
  # sqrt(mean(residuals(fit) ^ 2, na.rm = TRUE)) # ~12 degrees

  # Rotate velocity vectors, whose dimensions are starboard, forward, mast,
  # error velocity. Because "mast" has already been corrected to
  # be "up" according to rdi$coord_transform, we only need to rotate the
  # starboard and forward dimensions.
  velocity <- abind::abind(rdi$velocity, along = 0)
  for (i in seq_len(5)) {
    velocity[i, 1:2, ] <- t(
      rotate_about_origin(
        t(velocity[i, 1:2, , drop = TRUE]),
        # tested to align with the results of oce::toEnu()
        rdi_meta$beam_heading_corrected[i]
      )
    )
  }

  # velocity is in its own dimension family (date_time x enu x distance)
  rdi_east_north_up_n_cells <- list(velocity = velocity[, 1:3, , drop = FALSE])

  # error velocity is in its own dimension family (date_time x distance)
  rdi_n_cells <- list(list(error_velocity = velocity[, 4, , drop = TRUE]))

  bottom_velocity <- abind::abind(rdi$bottom_velocity, along = 0)
  bottom_velocity[, 1:2] <-
    rotate_about_origin(
      bottom_velocity[, 1:2, drop = FALSE],
      # tested to align with the results of oce::toEnu()
      rdi_meta$beam_heading_corrected
    )

  # bottom velocity is in its own dimension family
  rdi_east_north_up <- list(bottom_velocity = bottom_velocity[, 1:3, drop = FALSE])

  # bottom error velocity shares dimensions with rdi_meta (just date_time)
  rdi_meta$bottom_error_velocity <- bottom_velocity[, 4, drop = TRUE]

  # Flags ----

  # TODO: Can't tell from original code which velocity bins are getting the ax
  # based on distance to surface. There are also no velocity series that
  # have >50% NA, which were flagged in the initial version of this code.
  # https://github.com/richardsc/bsrto/blob/master/adp.R#L61-L71

  # Could likely use error velocities and pct good as a flag
  # (there's a good guide in the processing repo for adcp data on
  # gccode)

  # Prepare NetCDF ----

  compression <- 5

  dim_date_time <- ncdf4::ncdim_def(
    "date_time",
    units = "seconds since 1970-01-01 00:00:00 UTC",
    vals = as.numeric(rdi_meta$date_time, origin = "1970-01-01 00:00:00")
  )

  dim_string12 <- ncdf4::ncdim_def(
    "string12",
    units = "count",
    vals = 1:12
  )

  dim_n_beams <- ncdf4::ncdim_def(
    "n_beams",
    units = "count",
    vals = seq_len(rdi_config$n_beams)
  )

  dim_enu <- ncdf4::ncdim_def(
    "east_north_up",
    units = "count",
    vals = seq_len(3)
  )

  dim_distance <- ncdf4::ncdim_def(
    "distance",
    units = "meters",
    vals = n_cell_distance
  )

  file_var <- ncdf4::ncvar_def(
    "file",
    units = "character",
    dim = list(dim_string12, dim_date_time),
    longname = "Source filename",
    prec = "char",
    compression = compression
  )

  meta_vars <- lapply(
    setdiff(names(rdi_meta), c("file", "date_time")),
    function(col) {
      val <- rdi_meta[[col]]
      ncdf4::ncvar_def(
        name = col,
        units = "", # TODO: need units for all types of tables
        dim = list(dim_date_time),
        missval = val[NA_integer_],
        prec = switch(
          typeof(val),
          integer = "integer",
          double = "double",
          abort(glue("Can't guess NetCDF prec from class '{ typeof(val) }'"))
        ),
        compression = compression
      )
  })

  n_beams_vars <- lapply(
    names(rdi_n_beams),
    function(col) {
      val <- rdi_n_beams[[col]]
      ncdf4::ncvar_def(
        name = col,
        units = "", # TODO: need units for all types of tables
        dim = list(dim_date_time, dim_n_beams),
        missval = val[NA_integer_],
        prec = switch(
          typeof(val),
          integer = "integer",
          double = "double",
          abort(glue("Can't guess NetCDF prec from class '{ typeof(val) }'"))
        ),
        compression = compression
      )
    })

  n_beams_n_cells_vars <- lapply(
    names(rdi_n_beams_n_cells),
    function(col) {
      val <- rdi_n_beams_n_cells[[col]]
      ncdf4::ncvar_def(
        name = col,
        units = "", # TODO: need units for all types of tables
        dim = list(dim_date_time, dim_n_beams, dim_distance),
        missval = if (!is.raw(val)) val[NA_integer_],
        prec = switch(
          typeof(val),
          integer = "integer",
          double = "float",
          raw = "integer",
          abort(glue("Can't guess NetCDF prec from class '{ typeof(val) }'"))
        ),
        compression = compression
      )
    })

  east_north_up_vars <- lapply(
    names(rdi_east_north_up),
    function(col) {
      val <- rdi_east_north_up[[col]]
      ncdf4::ncvar_def(
        name = col,
        units = "", # TODO: need units for all types of tables
        dim = list(dim_date_time, dim_enu),
        missval = if (!is.raw(val)) val[NA_integer_],
        prec = switch(
          typeof(val),
          integer = "integer",
          double = "float",
          raw = "integer",
          abort(glue("Can't guess NetCDF prec from class '{ typeof(val) }'"))
        ),
        compression = compression
      )
    })

  east_north_up_n_cells_vars <- lapply(
    names(rdi_east_north_up_n_cells),
    function(col) {
      val <- rdi_east_north_up_n_cells[[col]]
      ncdf4::ncvar_def(
        name = col,
        units = "", # TODO: need units for all types of tables
        dim = list(dim_date_time, dim_enu, dim_distance),
        missval = if (!is.raw(val)) val[NA_integer_],
        prec = switch(
          typeof(val),
          integer = "integer",
          double = "float",
          raw = "integer",
          abort(glue("Can't guess NetCDF prec from class '{ typeof(val) }'"))
        ),
        compression = compression
      )
    })

  n_cells_vars <- lapply(
    names(rdi_n_cells),
    function(col) {
      val <- rdi_n_cells[[col]]
      ncdf4::ncvar_def(
        name = col,
        units = "", # TODO: need units for all types of tables
        dim = list(dim_date_time, dim_distance),
        missval = if (!is.raw(val)) val[NA_integer_],
        prec = switch(
          typeof(val),
          integer = "integer",
          double = "float",
          raw = "integer",
          abort(glue("Can't guess NetCDF prec from class '{ typeof(val) }'"))
        ),
        compression = compression
      )
    })

  # Write NetCDF ----

  out_file <- file.path(out_dir, "adp.nc")
  cli::cat_line(glue("Writing '{ out_file }'"))

  nc <- ncdf4::nc_create(
    out_file,
    c(
      list(file_var),
      meta_vars,
      n_beams_vars,
      n_beams_n_cells_vars,
      east_north_up_vars,
      east_north_up_n_cells_vars,
      n_cells_vars
    )
  )
  on.exit(ncdf4::nc_close(nc))

  # write rdi_config as global metadata
  for (col in names(rdi_config)) {
    ncdf4::ncatt_put(nc, 0, col, as.character(rdi_config[[col]]))
  }

  for (col in setdiff(names(rdi_meta), "date_time")) {
    ncdf4::ncvar_put(nc, col, rdi_meta[[col]])
  }

  for (col in names(rdi_n_beams)) {
    ncdf4::ncvar_put(nc, col, rdi_n_beams[[col]])
  }

  for (col in names(rdi_n_beams_n_cells)) {
    ncdf4::ncvar_put(nc, col, rdi_n_beams_n_cells[[col]])
  }

  for (col in names(rdi_east_north_up)) {
    ncdf4::ncvar_put(nc, col, rdi_east_north_up[[col]])
  }

  for (col in names(rdi_east_north_up_n_cells)) {
    ncdf4::ncvar_put(nc, col, rdi_east_north_up_n_cells[[col]])
  }

  for (col in names(rdi_n_cells)) {
    ncdf4::ncvar_put(nc, col, rdi_n_cells[[col]])
  }

  # on.exit() takes care of ncdf4::nc_close(nc)
}

write_realtime_icl <- function(icl, out_dir = ".") {
  cli::cat_rule("write_realtime_icl()")

  # This data is exportable as .csv but fits more naturally as a NetCDF
  # Use column names and flag conventions following that of Env Canada
  # climate data

  # Need to make sure each row represents a unique date/time or the
  # netCDF magic below won't work.
  stopifnot(all(!duplicated(icl$Time)))

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

  # some of the bad values are outside the integer range, which causes
  # warnings that we don't care about
  intensity <- suppressWarnings(as.integer(intensity))

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
  out_file <- file.path(out_dir, "icl.nc")
  cli::cat_line(glue("Writing '{ out_file }'"))

  # without compression, this file is >300 MB, which is bigger than the
  # CSV that would result from writing it without processing. A value of
  # 5 drops the size by a factor of 10.
  compression <- 5

  nc <- ncdf4::nc_create(
    out_file,
    list(
      ncdf4::ncvar_def(
        "file",
        units = "character",
        dim = list(dim_string23, dim_date_time),
        longname = "Source filename",
        prec = "char",
        compression = compression
      ),
      ncdf4::ncvar_def(
        "icl_temp",
        units = "Degrees C",
        dim = list(dim_date_time),
        longname = "Operating temperature",
        prec = "float",
        compression = compression
      ),
      ncdf4::ncvar_def(
        "icl_temp_flag",
        units = "Non-zero for possible bad data",
        dim = list(dim_date_time),
        prec = "short",
        compression = compression
      ),
      ncdf4::ncvar_def(
        "icl_rel_hum",
        units = "%",
        dim = list(dim_date_time),
        longname = "Operating relative humidity",
        prec = "float",
        compression = compression
      ),
      ncdf4::ncvar_def(
        "icl_rel_hum_flag",
        units = "Non-zero for possible bad data",
        dim = list(dim_date_time),
        prec = "short",
        compression = compression
      ),
      ncdf4::ncvar_def(
        "icl_intensity",
        units = "Relative intensity",
        dim = list(dim_date_time, dim_frequency),
        # note that using "short" here doesn't result in a smaller file
        # if compression is enabled
        prec = "integer",
        compression = compression
      ),
      ncdf4::ncvar_def(
        "icl_intensity_flag",
        units = "Non-zero for possible bad data",
        dim = list(dim_date_time, dim_frequency),
        prec = "short",
        compression = compression
      )
    )
  )
  on.exit(ncdf4::nc_close(nc))

  ncdf4::ncvar_put(nc, "file", icl_meta$file)
  ncdf4::ncvar_put(nc, "icl_temp", icl_meta$icl_temp)
  ncdf4::ncvar_put(nc, "icl_temp_flag", icl_meta$icl_temp_flag)
  ncdf4::ncvar_put(nc, "icl_rel_hum", icl_meta$icl_rel_hum)
  ncdf4::ncvar_put(nc, "icl_rel_hum_flag", icl_meta$icl_rel_hum_flag)
  ncdf4::ncvar_put(nc, "icl_intensity", intensity)
  ncdf4::ncvar_put(nc, "icl_intensity_flag", intensity_flag)

  # on.exit() takes care of nc_close(nc)
}

write_realtime_ips <- function(ips, baro, out_dir = ".") {
  cli::cat_rule("write_realtime_ips()")

  resampled_pressure <- resample_nearest(
    baro$date_time,
    baro$shore_press,
    ips$date_time,
    max_distance = 60 * 120 # constrain to ~2 hours
  )

  # redundant vars that don't get used later
  ips$secs_since_1970 <- NULL
  ips$station_id <- NULL

  # use first 130 bins for each bin (pad shorter lengths with NA)
  bin_lengths <- vapply(ips$bins, length, integer(1))
  distance <- seq(9, by = 0.1, length.out = 130)
  ips$bins <- lapply(ips$bins, "[", 1:130)

  # define NetCDF dimensions and variables
  compression <- 5

  dim_date_time <- ncdf4::ncdim_def(
    "date_time",
    units = "seconds since 1970-01-01 00:00:00 UTC",
    vals = as.numeric(ips$date_time, origin = "1970-01-01 00:00:00")
  )

  dim_string12 <- ncdf4::ncdim_def(
    "string12",
    units = "count",
    vals = 1:12
  )

  dim_distance <- ncdf4::ncdim_def(
    "distance",
    units = "meters",
    vals = distance
  )

  file_var <- ncdf4::ncvar_def(
    "file",
    units = "character",
    dim = list(dim_string12, dim_date_time),
    longname = "Source filename",
    prec = "char",
    compression = compression
  )

  meta_vars <- lapply(
    setdiff(names(ips), c("file", "date_time", "bins")),
    function(col) {
      val <- ips[[col]]
      ncdf4::ncvar_def(
        name = col,
        units = "", # TODO: need units for all types of tables
        dim = list(dim_date_time),
        missval = val[NA_integer_],
        prec = switch(
          typeof(val),
          integer = "integer",
          double = "double",
          abort(glue("Can't guess NetCDF prec from class '{ typeof(val) }'"))
        ),
        compression = compression
      )
    })

  bins_var <- ncdf4::ncvar_def(
    "ips_count",
    units = "counts",
    dim = list(dim_date_time, dim_distance),
    prec = "integer",
    compression = compression
  )

  # Write NetCDF

  out_file <- file.path(out_dir, "ips.nc")
  cli::cat_line(glue("Writing '{ out_file }'"))

  nc <- ncdf4::nc_create(
    out_file,
    c(
      list(file_var),
      meta_vars,
      list(bins_var)
    )
  )
  on.exit(ncdf4::nc_close(nc))

  for (col in setdiff(names(ips), c("date_time", "bins"))) {
    ncdf4::ncvar_put(nc, col, ips[[col]])
  }

  ncdf4::ncvar_put(nc, "ips_count", unlist(ips$bins))

  # on.exit() takes care of nc_close(nc)
}

write_realtime_lgh <- function(lgh, out_dir = ".") {
  cli::cat_rule("write_realtime_lgh()")

  # embedded newlines are possible in .csv and are better
  # at keeping the relevant log text together
  lgh$log_text <- vapply(lgh$log_text, paste, collapse = "\n", character(1))

  out_file <- file.path(out_dir, "lgh.csv")
  cli::cat_line(glue("Writing '{ out_file }'"))

  readr::write_csv(lgh, out_file)
}

read_realtime_cached <- function(file_type, build_cache = bs_build_cache_dir("realtime"),
                             use_cache = TRUE, save_cache = TRUE) {
  cached_file <- file.path(build_cache, glue("{ file_type }.rds"))

  if (use_cache && file.exists(cached_file)) {
    cli::cat_line(glue("Loading previous '{ file_type }' from '{ build_cache }'"))
    previous <- readRDS(cached_file)
  } else {
    previous <- NULL
  }

  result <- switch(
    file_type,
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
    abort(glue("Unknown file_type: '{ file_type }'"))
  )

  # nice for build logs to have a glimpse of the raw outputs
  cli::cat_rule(glue("[built${ file_type }]"))
  print(tibble::as_tibble(result))
  cli::cat_rule(glue("[/built${ file_type }]"))

  if (save_cache) {
    cli::cat_line(glue("Saving cached '{ file_type }'"))
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

    # remove wildly out-of-range values (mangled data, not bad measurements)
    temp_out_of_range <- (all$`Temperature [C]` > 10) | (all$`Temperature [C]` < -10)
    hum_out_of_range <- (all$`Humidity [%]` > 100) | (all$`Humidity [%]` < 0)

    rows_valid <- time_valid & comment_valid & data_points_valid &
      !temp_out_of_range & !hum_out_of_range

    build_realtime_log_qc(all, rows_valid)
    all <- all[rows_valid, ]

    # Some duplicated date times from mangled data (~13 rows)
    # Hard to do this before filtering the other mangled data out
    datetime_dup <- duplicated(all$Time)
    build_realtime_log_qc(all, !datetime_dup)
    all <- all[!datetime_dup, ]

    all <- rbind(previous, all)
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

    # check for a "missing" in one field
    rows_valid <- vapply(all$bottom_range, length, integer(1)) == 4
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
    cli::cat_line(glue("Skipping { sum(zero_size) } files(s) with zero size"))
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
