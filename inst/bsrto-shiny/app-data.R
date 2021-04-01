
library(shiny)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(ncdf4)

# This is wrapped in a function so that it can be updated while the process
# is active. None of these loads take very long but could be made faster
# by a cache step that packs this data into an SQLite database. Set
# options(bsrto.data_refresh_interval = 15 * 60 * 1000) (e.g.) to
# have this refresh every 15 minutes (implemented in the app via
# reactiveTimer()).
data_last_refresh <- as.POSIXct("1900-01-01 00:00:00")

data_refresh <- function() {
  since_last_refresh <- as.numeric(Sys.time() - data_last_refresh, units = "secs")
  refresh_interval_ms <- getOption("bsrto.data_refresh_interval", NULL)

  if (is.null(refresh_interval_ms) || ((since_last_refresh * 1000) < refresh_interval_ms)) {
    return()
  }

  data_last_refresh <<- Sys.time()


  built_dir <- getOption("bsrto.built_dir", "build-cache")
  cat(sprintf("Loading data from '%s'\n", fs::path_abs(built_dir)))

  data_ctd <<- readr::read_csv(
    file.path(built_dir, "ctd.csv"),
    col_types = readr::cols(
      file = readr::col_character(),
      date_time = readr::col_datetime(),
      .default = readr::col_double()
    )
  )

  data_met <<- readr::read_csv(
    file.path(built_dir, "met.csv"),
    col_types = readr::cols(
      file = readr::col_character(),
      date_time = readr::col_datetime(),
      .default = readr::col_double()
    )
  )

  data_baro <<- readr::read_csv(
    file.path(built_dir, "baro.csv"),
    col_types = readr::cols(
      file = readr::col_character(),
      date_time = readr::col_datetime(),
      .default = readr::col_double()
    )
  )

  data_lgh <<- readr::read_csv(
    file.path(built_dir, "lgh.csv"),
    col_types = readr::cols(
      file = readr::col_character(),
      date_time = readr::col_datetime(),
      .default = readr::col_character()
    )
  )

  data_pcm <<- readr::read_csv(
    file.path(built_dir, "pcm_summary.csv"),
    col_types = readr::cols(
      file = readr::col_character(),
      date_time = readr::col_datetime(),
      .default = readr::col_double()
    )
  )

  # ncdf4 handles should be closed if they exist already
  if (exists("data_adp_nc", envir = .GlobalEnv)) {
    nc_close(data_adp_nc)
  }
  data_adp_nc <<- nc_open(file.path(built_dir, "adp.nc"))
  data_adp_nc_date_time <<- as.POSIXct(
    ncvar_get(data_adp_nc, "date_time"),
    origin = "1970-01-01 00:00:00",
    tz = "UTC"
  )

  if (exists("data_ips_nc", envir = .GlobalEnv)) {
    nc_close(data_ips_nc)
  }
  data_ips_nc <<- nc_open(file.path(built_dir, "ips.nc"))
  data_ips_nc_date_time <<- as.POSIXct(
    ncvar_get(data_ips_nc, "date_time"),
    origin = "1970-01-01 00:00:00",
    tz = "UTC"
  )

  if (exists("data_icl_nc", envir = .GlobalEnv)) {
    nc_close(data_icl_nc)
  }
  data_icl_nc <<- nc_open(file.path(built_dir, "icl.nc"))
  data_icl_nc_date_time <<- as.POSIXct(
    ncvar_get(data_icl_nc, "date_time"),
    origin = "1970-01-01 00:00:00",
    tz = "UTC"
  )
}

# call this at least once!
data_refresh()

# for pulling multiple 1-d variables along the date_time dimension
data_nc_tibble <- function(nc, dt_range, vars, index = as.POSIXct(
                             ncvar_get(nc, "date_time"),
                             origin = "1970-01-01 00:00:00",
                             tz = "UTC"
                           )) {
  dt_dim_values <- which(
    (index >= dt_range[1]) &
      (index < dt_range[2])
  )
  stopifnot(all(diff(dt_dim_values) == 1L))
  dim_min <- suppressWarnings(min(dt_dim_values))
  dim_count <- length(dt_dim_values)

  if (dim_count == 0) {
    vals <- lapply(vars, function(x) double(0))
    names(vals) <- vars
    tibble::tibble(date_time = index[integer(0)], !!! vals)
  } else {
    vals <- lapply(
      nc$var[vars],
      function(x) ncvar_get(
        nc, x,
        start = dim_min,
        count = dim_count
      )
    )

    file <- ncvar_get(
      nc, "file",
      start = c(1, dim_min),
      count = c(-1, dim_count)
    )

    tibble::new_tibble(
      c(
        list(file = file, date_time = index[dt_dim_values]),
        vals
      ),
      nrow = dim_count
    )
  }
}

# pick an aggregate level for the data that is at too high a resolution
data_agr_time <- function(dt_range) {
  if (isTRUE(diff(dt_range) > as.difftime(120, units = "days"))) {
    date_agr = "week"
    date_time_grid <- seq(
      lubridate::floor_date(dt_range[1], "week"),
      dt_range[2],
      by = "week"
    )
  } else if (isTRUE(diff(dt_range) > as.difftime(10, units = "days"))) {
    date_agr <- "day"
    date_time_grid <- seq(
      lubridate::floor_date(dt_range[1], "day"),
      dt_range[2],
      by = "day"
    )
  } else {
    date_agr <- "2 hour"
    date_time_grid <- seq(
      lubridate::floor_date(dt_range[1], "2 hour"),
      dt_range[2],
      by = "2 hour"
    )
  }

  list(
    date_agr = date_agr,
    date_time_grid = date_time_grid
  )
}

# ggplot2 components -------------

theme_set(theme_bw() + theme(strip.background = element_blank()))

scale_bsrto_datetime <- function(limits) {
  scale_x_datetime(
    limits = limits,
    expand = expansion(0, 0)
  )
}

# one-dimensional time-series plots
data_plot_datetime <- function(data, var, lab = var,
                               datetime_range = range(data$date_time, na.rm = TRUE),
                               lang = "en",
                               mapping = NULL,
                               extra = list()) {
  # occurs on initial load
  if (length(datetime_range) != 2) {
    return()
  }

  # there is no easy way to translate date labels without
  # explicit LC_TIME support for the other language
  # (not necessarily the case for my interactive Windows development)
  print(suppressWarnings(
    withr::with_locale(
      c(LC_TIME = paste0(lang, "_CA")),
      ggplot(data, aes(date_time, .data[[var]])) +
        geom_point(mapping = mapping, na.rm = TRUE) +
        scale_bsrto_datetime(datetime_range) +
        labs(x = NULL, y = i18n_t(lab, lang)) +
        extra
    )
  ))
}

dataUI <- function(id = "data") {
  tagList(
    div(
      # Global data filter options
      div(
        style = "padding-left: 10px; padding-right: 10px;",
        uiOutput(NS(id, "date_range"))
        # possible future filter for data flags?
      )
    )
  )
}

dataServer <- function(lang, id = "data") {
  moduleServer(id, function(input, output, session) {

    # this is a user-specific timer, so the worst-case refresh lag
    # would be the refresh interval * 2 if the user loads the app
    # just before a data refresh and lets it sit open for a while
    data_refresh_timer <- reactiveTimer(
      getOption(
        "bsrto.data_refresh_interval",
        # just pick some really big number if there is no data refresh
        # (probably because it's in development mode)
        1e9
      )
    )

    global_date_range <- reactive({
      data_refresh_timer()
      data_refresh()

      as.Date(range(data_ctd$date_time))
    })

    # Reactive on global_date_range() and lang(). When
    # re-rendering because of a language change or data update,
    # we'll want to keep the date range previously selected.
    # The exception is on data refresh when the user had the default
    # end date selected (in which case we should just update the end
    # date)
    output$date_range <- renderUI({
      date_range <- global_date_range()
      global_min <- date_range[1]
      global_max <- date_range[2] + 1L

      # get the current value without establishing a reactive dependency
      current_date_range <- isolate(input$date_range)

      if (length(current_date_range) != 2) {
        # occurs on first render
        render_start <- date_range[2] - 7L
        render_end <- date_range[2] + 1L
      } else if ((global_max - current_date_range[2]) <= 1L) {
        # if the last date is selected or has only increased by one,
        # update the end date
        render_start <- current_date_range[1]
        render_end <- date_range[2] + 1L
      } else {
        # keep the start and end dates
        render_start <- current_date_range[1]
        render_end <- current_date_range[2]
      }

      dateRangeInput(
        NS(id, "date_range"), NULL,
        start = render_start,
        end = render_end,
        min = global_min,
        max = global_max,
        separator = i18n_t("date_range_sep", lang()),
        language = lang()
      )
    })

    datetime_range <- reactive({
      data_refresh_timer()
      dt_range <- as.POSIXct(input$date_range)
      attr(dt_range, "tzone") <- "UTC"
      dt_range
    })

    # reactive values that return data frames based on user filter
    ctd <- reactive({
      dt_range <- datetime_range()

      data_ctd %>%
        filter(
          date_time >= !! dt_range[1],
          date_time < !! dt_range[2]
        )
    })

    met <- reactive({
      dt_range <- datetime_range()

      data_met %>%
        filter(
          date_time >= !! dt_range[1],
          date_time < !! dt_range[2]
        )
    })

    baro <- reactive({
      dt_range <- datetime_range()

      data_baro %>%
        filter(
          date_time >= !! dt_range[1],
          date_time < !! dt_range[2]
        )
    })

    lgh <- reactive({
      dt_range <- datetime_range()

      data_lgh %>%
        filter(
          date_time >= !! dt_range[1],
          date_time < !! dt_range[2]
        )
    })

    pcm <- reactive({
      dt_range <- datetime_range()

      data_pcm %>%
        filter(
          date_time >= !! dt_range[1],
          date_time < !! dt_range[2]
        )
    })

    adp_meta <- reactive({
      dt_range <- datetime_range()

      meta_vars <- c(
        # available in file but not reporting here
        # "n_data_types", "ensemble_number", "ensemble_number_msb", "bit_result",
        # "sound_speed",  "heading_std", "pitch_std", "roll_std",
        # "pressure_plus", "pressure_minus", "attitude_temp",
        # "transmit_current", "transmit_voltage", "pressure_std",
        "beam_heading_corrected", "bottom_error_velocity",
        "transducer_depth", "heading", "pitch", "roll",
        "salinity", "temperature", "ambient_temperature",
        "attitude",  "contamination_sensor", "pressure"
      )

      data_nc_tibble(
        data_adp_nc,
        dt_range = dt_range,
        vars = meta_vars,
        index = data_adp_nc_date_time
      )
    })

    adp_beam_meta <- reactive({
      dt_range <- datetime_range()

      beam_vars <- c(
        "bottom_range",
        "bottom_velocity_raw", "bottom_correlation",
        "bottom_amplitude", "bottom_pct_good"
      )

      index <- data_adp_nc_date_time
      n_beam <- data_adp_nc$dim$n_beam$vals

      dt_dim_values <- which(
        (index >= dt_range[1]) &
          (index < dt_range[2])
      )
      stopifnot(all(diff(dt_dim_values) == 1L))
      dim_min <- suppressWarnings(min(dt_dim_values))
      dim_count <- length(dt_dim_values)

      if (dim_count == 0) {
        vars0 <- lapply(beam_vars, function(x) double(0))
        names(vars0) <- beam_vars

        tibble::tibble(
          date_time = data_adp_nc_date_time[integer(0)],
          n_beam = integer(0),
          !!! vars0
        )
      } else {
        dims <- expand.grid(
          date_time = data_adp_nc_date_time[dt_dim_values],
          n_beam = n_beam
        )

        dims[beam_vars] <- lapply(
          beam_vars,
          function(x) {
            as.numeric(
              ncvar_get(
                data_adp_nc, x,
                start = c(dim_min, 1),
                count = c(dim_count, length(n_beam))
              )
            )
          }
        )

        tibble::as_tibble(dims)
      }
    })

    adp_cells <- reactive({
      dt_range <- datetime_range()

      cell_vars <- c(
        "velocity_raw", "correlation", "echo_intensity",
        "pct_good"
      )

      index <- data_adp_nc_date_time
      n_beam <- data_adp_nc$dim$n_beam$vals
      distance <- data_adp_nc$dim$distance$vals

      dt_dim_values <- which(
        (index >= dt_range[1]) &
          (index < dt_range[2])
      )
      stopifnot(all(diff(dt_dim_values) == 1L))
      dim_min <- suppressWarnings(min(dt_dim_values))
      dim_count <- length(dt_dim_values)

      if (dim_count == 0) {
        vars0 <- lapply(cell_vars, function(x) double(0))
        names(vars0) <- cell_vars

        tibble::tibble(
          date_time = data_adp_nc_date_time[integer(0)],
          n_beam = integer(0),
          distance = double(0),
          !!! vars0
        )
      } else {
        date_agr <- data_agr_time(dt_range)

        dims <- expand.grid(
          date_time = data_adp_nc_date_time[dt_dim_values],
          n_beam = n_beam,
          distance = distance
        )

        dims[cell_vars] <- lapply(
          cell_vars,
          function(x) {
            as.numeric(
              ncvar_get(
                data_adp_nc, x,
                start = c(dim_min, 1, 1),
                count = c(dim_count, length(n_beam), length(distance))
              )
            )
          }
        )

        agr <- dims %>%
          mutate(
            date_time = lubridate::floor_date(date_time, date_agr$date_agr)
          ) %>%
          group_by(date_time, n_beam, distance) %>%
          summarise(across(everything(), median, na.rm = TRUE), .groups = "drop")

        expand.grid(
          date_time = date_agr$date_time_grid,
          n_beam = n_beam,
          distance = distance
        ) %>%
          left_join(agr, by = c("date_time", "n_beam", "distance")) %>%
          tibble::as_tibble()
      }
    })

    adp_bottom_velocity <- reactive({
      dt_range <- datetime_range()

      index <- data_adp_nc_date_time
      distance <- data_adp_nc$dim$distance$vals

      dt_dim_values <- which(
        (index >= dt_range[1]) &
          (index < dt_range[2])
      )
      stopifnot(all(diff(dt_dim_values) == 1L))
      dim_min <- suppressWarnings(min(dt_dim_values))
      dim_count <- length(dt_dim_values)

      if (dim_count == 0) {
        tibble::tibble(
          date_time = data_adp_nc_date_time[integer(0)],
          bottom_velocity_east = double(0),
          bottom_velocity_north = double(0),
          bottom_velocity_up = double(0),
          bottom_velocity_total = double(0),
          bottom_velocity_direction = double(0)
        )
      } else {
        values <- ncvar_get(
          data_adp_nc,
          "bottom_velocity",
          start = c(dim_min, 1),
          count = c(dim_count, -1)
        )

        tibble::tibble(
          date_time = data_adp_nc_date_time[dt_dim_values],
          bottom_velocity_east = values[, 1, drop = TRUE],
          bottom_velocity_north = values[, 2, drop = TRUE],
          bottom_velocity_up = values[, 3, drop = TRUE]
        )  %>%
          mutate(
            bottom_velocity_total = sqrt(bottom_velocity_east ^ 2 + bottom_velocity_north ^ 2),
            bottom_velocity_direction = headings::hdg_from_uv(
              headings::uv(bottom_velocity_east, bottom_velocity_north)
            )
          )
      }
    })

    adp_velocity <- reactive({
      dt_range <- datetime_range()

      index <- data_adp_nc_date_time
      east_north_up <- data_adp_nc$dim$east_north_up$vals
      distance <- data_adp_nc$dim$distance$vals

      dt_dim_values <- which(
        (index >= dt_range[1]) &
          (index < dt_range[2])
      )
      stopifnot(all(diff(dt_dim_values) == 1L))
      dim_min <- suppressWarnings(min(dt_dim_values))
      dim_count <- length(dt_dim_values)

      if (dim_count == 0) {
        tibble::tibble(
          date_time = data_adp_nc_date_time[integer(0)],
          distance = double(0),
          east_north_up = integer(0),
          velocity = double(0)
        )
      } else {
        date_agr <- data_agr_time(dt_range)

        dims <- expand.grid(
          date_time = data_adp_nc_date_time[dt_dim_values],
          east_north_up = east_north_up,
          distance = distance
        )

        dims["velocity"] <- lapply(
          "velocity",
          function(x) {
            as.numeric(
              ncvar_get(
                data_adp_nc, x,
                start = c(dim_min, 1, 1),
                count = c(dim_count, length(east_north_up), length(distance))
              )
            )
          }
        )

        agr <- dims %>%
          mutate(
            date_time = lubridate::floor_date(date_time, date_agr$date_agr)
          ) %>%
          group_by(date_time, east_north_up, distance) %>%
          summarise(across(everything(), median, na.rm = TRUE), .groups = "drop")

        expand.grid(
          date_time = date_agr$date_time_grid,
          east_north_up = east_north_up,
          distance = distance
        ) %>%
          left_join(agr, by = c("date_time", "east_north_up", "distance")) %>%
          tibble::as_tibble()
      }
    })

    ips_meta <- reactive({
      dt_range <- datetime_range()

      meta_vars <- c(
        "draft_max", "draft_min", "draft_mean", "draft_sd",
        "n_ranges", "n_partial_ranges", "sound_speed", "density", "gravity",
        "pressure_max", "pressure_min", "temp_max", "temp_min", "max_pitch",
        "max_roll_pitch", "max_roll", "max_pitch_roll", "max_inclination"
      )

      data_nc_tibble(
        data_ips_nc,
        dt_range = dt_range,
        vars = meta_vars,
        index = data_ips_nc_date_time
      )
    })

    icl_meta <- reactive({
      dt_range <- datetime_range()

      meta_vars <- c("icl_temp", "icl_rel_hum")

      data_nc_tibble(
        data_icl_nc,
        dt_range = dt_range,
        vars = meta_vars,
        index = data_icl_nc_date_time
      )
    })

    icl_intensity <- reactive({
      dt_range <- datetime_range()
      index <- data_icl_nc_date_time
      frequency <- data_icl_nc$dim$frequency$vals

      dt_dim_values <- which(
        (index >= dt_range[1]) &
          (index < dt_range[2])
      )
      stopifnot(all(diff(dt_dim_values) == 1L))
      dim_min <- suppressWarnings(min(dt_dim_values))
      dim_count <- length(dt_dim_values)

      if (dim_count == 0) {
        tibble::tibble(
          frequency = double(0),
          date_time = data_icl_nc_date_time[integer(0)],
          intensity = integer(0)
        )
      } else {
        date_agr <- data_agr_time(dt_range)

        dims <- expand.grid(
          date_time = data_icl_nc_date_time[dt_dim_values],
          frequency = frequency
        )

        dims$intensity <- as.integer(
          ncvar_get(
            data_icl_nc,
            "icl_intensity",
            start = c(dim_min, 1),
            count = c(dim_count, -1)
          )
        )

        intensity_agr <- dims %>%
          mutate(
            date_time = lubridate::floor_date(date_time, date_agr$date_agr)
          ) %>%
          group_by(date_time, frequency) %>%
          summarise(intensity = median(intensity, na.rm = TRUE), .groups = "drop")

        expand.grid(
          date_time = date_agr$date_time_grid,
          frequency = frequency
        ) %>%
          left_join(intensity_agr, by = c("date_time", "frequency")) %>%
          tibble::as_tibble()
      }
    })

    # return a set of "exported" reactive values on which other modules
    # can depend
    reactiveValues(
      global_date_range = global_date_range,
      datetime_range = datetime_range,
      ctd = ctd,
      met = met,
      baro = baro,
      lgh = lgh,
      pcm = pcm,
      adp_meta = adp_meta,
      adp_beam_meta = adp_beam_meta,
      adp_bottom_velocity = adp_bottom_velocity,
      adp_velocity = adp_velocity,
      adp_cells = adp_cells,
      ips_meta = ips_meta,
      icl_meta = icl_meta,
      icl_intensity = icl_intensity
    )
  })
}
