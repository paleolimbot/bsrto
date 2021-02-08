
#' Build data for Navigator
#'
#' For the purposes of [Navigator](https://navigator.oceansdata.ca/),
#' the BSRTO is treated as a single station at 74.605N/91.251W.
#' There is a single WMO code assigned to the station that is used for
#' the moorings that represent all three depths.
#'
#' @param built_dir The output directory used for
#'   [bs_build_realtime()] that contains the built data files.
#' @param out_dir Where the NetCDF files should be created.
#'
#' @return `out_dir`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' bs_build_realtime()
#' bs_build_realtime_navigator()
#' }
bs_build_realtime_navigator <- function(built_dir = ".", out_dir = build_dir) {

  dims <- build_navigator_dims()
  vars <- build_navigator_vars(dims)
  meta <- build_navigator_meta()

  out_file <- file.path(out_dir, "bsrto-navigator.nc")
  cli::cat_line("Writing '{ out_file }'")

  ctd <- build_navigator_ctd(out_dir)


  invisible(out_dir)
}

build_navigator_ctd <- function(build_dir = ".") {
  ctd_file <- file.path(build_dir, "ctd.csv")
  cli::cat_line(glue("Loading '{ ctd_file }'"))
  stopifnot(file.exists(ctd_file))

  ctd <- readr::read_csv(
    ctd_file,
    col_types = readr::cols(
      file = readr::col_character(),
      date_time = readr::col_datetime(format = ""),
      .default = col_double()
    )
  )

  # TODO: should add missing flag here to differentiate between the
  # implicit missings we're about to add by gridding the data

  # The date_time reported is not aligned between moorings,
  # but is usually within a few seconds. To make a useful
  # multidimensional array, these are rounded by <30 seconds
  # (and checked to make sure this is the case).
  date_time_round <- round.POSIXt(ctd$date_time, "hours")
  date_time_diff <- as.numeric(date_time_round - ctd$date_time, units = "secs")
  stopifnot(
    max(abs(date_time_diff)) < 30
  )
  ctd$date_time <- as.POSIXct(date_time_round)

  # The below code makes a sequence of dates and times every
  # two hours covering the span of the input. Check to make
  # sure all of the observations fit this requirement.
  stopifnot(
    all(as.POSIXlt(ctd$date_time)$hour %in% seq(0, 22, by = 2)),
    all(as.POSIXlt(ctd$date_time)$minute == 0),
    all(as.POSIXlt(ctd$date_time)$sec == 0)
  )

  # Use moored depth label as the DEPTH dimension
  date_time_range <- range(ctd$date_time)
  ctd_aligned <- expand.grid(
    depth_label = sort(unique(ctd$depth_label)),
    date_time = seq(date_time_range[1], date_time_range[2], by = "2 hours")
  ) %>%
    dplyr::left_join(ctd, by = c("date_time", "depth_label")) %>%
    dplyr::transmute(
      DEPTH = .data$depth_label,
      TIME = as.numeric(
        .data$date_time - as.POSIXct("1950-01-01 00:00:00", tz = "UTC"),
        units = "days"
      ),
      PRES = .data$pressure,
      DOXY = .data$oxygen,
      PSAL = .data$salinity,
      COND = .data$conductivity,
      SSPEED = .data$sound_speed
    )

  ctd_aligned
}

build_navigator_dims <- function() {

}

build_navigator_vars <- function(dims) {

}

navigator_var_qc <- function(name) {
  attrs <- list(
    long_name = "quality flag",
    conventions = "OceanSITES reference table 2",
    valid_min = 0L, valid_max = 9L,
    flag_values = 0:9,
    flag_meanings = paste(
      c("no_qc_performed", "good_data", "probably_good_data",
        "bad_data_that_are_potentially_correctable",
        "bad_data", "value_changed", "not_used", "nominal_value",
        "interpolated_value",  "missing_value"),
      collapse = " "
    )
  )
}

build_navigator_meta <- function(date_start, date_end, date_update = Sys.time()) {
  date_update <- navigator_datetime(date_update)
  station_lat <- "74.605"
  station_lon <- "-91.251"

  meta <- list(
    data_type = "Moored instrument",
    format_version = "0.1",
    platform_code = "68997",
    date_update = date_update,
    institution = "Bedford Institute of Oceanography (BIO)",
    institution_edmo_code = "1811",
    site_code = " ",
    wmo_platform_code = "WMO CODE",
    platform_name = "BSRTO",
    wmo_inst_type = "830",
    source = "moored instruments",
    source_platform_category_code = "PLATFORM_CATEGORY",
    history = glue("{ date_update } : Creation"),
    data_mode = "R",
    quality_control_indicator = "6",
    quality_index = "A",
    references = "https://doi.org/10.1145/3148675.3152195",
    comment = " ",
    Conventions = "CF-1.6 OceanSITES-Manual-1.2 Copernicus-InSituTAC-SRD-1.4 Copernicus-InSituTAC-ParametersList-3.1.0",
    netcdf_version = "netCDF-4 classic model",
    title = "Global Ocean - In Situ Observation Copernicus",
    summary = " ",
    naming_authority = "OceanSITES",
    id = "GL_PR_GL_68997_202010",
    cdm_data_type = "vertical profile",
    area = "Global Ocean",
    geospatial_lat_min = station_lat,
    geospatial_lat_max = station_lat,
    geospatial_lon_min = station_lon,
    geospatial_lon_max = station_lon,
    geospatial_vertical_min = "0",
    geospatial_vertical_max = "160",
    time_coverage_start = navigator_datetime(date_start),
    time_coverage_end = navigator_datetime(date_end),
    institution_references = " ",
    contact = "Clark.Richards@dfo-mpo.gc.ca",
    author = "Fisheries and Oceans Canada",
    data_assembly_center = "Bedford Institute of Oceanography",
    pi_name = "Clark Richards",
    distribution_statement = paste(
      "These data are public and free of charge. User assumes all risk for use of ",
      "data. User must display citation in any publication or product using data. ",
      "User must contact PI prior to any commercial use of data."
    ),
    citation = paste(
      "These data were collected and made freely available by Fisheries and ",
      "Oceans Canada and its partner organizations."
    ),
    update_interval = "daily",
    qc_manual = "OceanSITES User's Manual v1.2",
    last_date_observation = navigator_datetime(date_end),
    last_latitude_observation = station_lat,
    last_longitude_observation = station_lon
  )
}

navigator_datetime <- function(value) {
  format(
    as.POSIXct(value),
    "%Y-%m-%dT%H:%M:%OSZ",
    tz = "UTC",
    justify = "none"
  )
}
