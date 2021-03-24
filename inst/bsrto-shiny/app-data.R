
library(shiny)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(ncdf4)

built_dir <- getOption("bsrto.built_dir", "build-cache")

# eventually these should be SQLite tables or something else
# that can be lazily queried
data_ctd <- readr::read_csv(
  file.path(built_dir, "ctd.csv"),
  col_types = readr::cols(
    file = readr::col_character(),
    date_time = readr::col_datetime(),
    .default = readr::col_double()
  )
)

data_met <- readr::read_csv(
  file.path(built_dir, "met.csv"),
  col_types = readr::cols(
    file = readr::col_character(),
    date_time = readr::col_datetime(),
    .default = readr::col_double()
  )
)

data_baro <- readr::read_csv(
  file.path(built_dir, "baro.csv"),
  col_types = readr::cols(
    file = readr::col_character(),
    date_time = readr::col_datetime(),
    .default = readr::col_double()
  )
)

data_lgh <- readr::read_csv(
  file.path(built_dir, "lgh.csv"),
  col_types = readr::cols(
    file = readr::col_character(),
    date_time = readr::col_datetime(),
    .default = readr::col_character()
  )
)

data_pcm <- readr::read_csv(
  file.path(built_dir, "pcm_summary.csv"),
  col_types = readr::cols(
    file = readr::col_character(),
    date_time = readr::col_datetime(),
    .default = readr::col_double()
  )
)

data_adp_nc <- nc_open(file.path(built_dir, "adp.nc"))
data_adp_nc_date_time <- as.POSIXct(
  ncvar_get(data_adp_nc, "date_time"),
  origin = "1970-01-01 00:00:00",
  tz = "UTC"
)

data_ips_nc <- nc_open(file.path(built_dir, "ips.nc"))
data_ips_nc_date_time <- as.POSIXct(
  ncvar_get(data_ips_nc, "date_time"),
  origin = "1970-01-01 00:00:00",
  tz = "UTC"
)

data_icl_nc <- nc_open(file.path(built_dir, "icl.nc"))
data_icl_nc_date_time <- as.POSIXct(
  ncvar_get(data_icl_nc, "date_time"),
  origin = "1970-01-01 00:00:00",
  tz = "UTC"
)

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

# eventually this should be non-ggplot, but this works as a wrapper
# to get the data needed for the one-dimensional time-series plots
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
        scale_x_datetime(
          limits = datetime_range
        ) +
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

        div(
          style = "display:inline-block; width: 49%; vertical-align: middle; text-align: left;",
          uiOutput(NS(id, "date_range"))
        ),

        div(
          style = "display:inline-block; width: 49%; vertical-align: middle; text-align: right;",
          checkboxGroupInput(
            NS(id, "mooring_depths"), NULL,
            choices = c("40 m", "60 m", "160 m"),
            selected = c("40 m", "60 m", "160 m"),
            inline = TRUE
          )
        )
      ),

      # this div is a home for the JSON version of the (filtered) output
      div(id = NS(id, "json-data-container"))
    )
  )
}

dataServer <- function(lang, id = "data") {
  moduleServer(id, function(input, output, session) {

    global_date_range <- reactive({
      as.Date(range(data_ctd$date_time))
    })

    # reactive on global_date_range() and lang() and shouldn't
    # re-render when the user changes their desired date range
    # (but will re-render when the language is updated)
    output$date_range <- renderUI({
      date_range <- global_date_range()

      dateRangeInput(
        NS(id, "date_range"), NULL,
        start = date_range[2] - 7L,
        end = date_range[2] + 1L,
        min = date_range[1],
        max = date_range[2] + 1L,
        separator = i18n_t("date_range_sep", lang()),
        language = lang()
      )
    })

    datetime_range <- reactive({
      dt_range <- as.POSIXct(input$date_range)
      attr(dt_range, "tzone") <- "UTC"
      dt_range
    })

    # reactive values that return data frames based on user filter
    ctd <- reactive({
      dt_range <- datetime_range()
      mooring_depths <- as.numeric(gsub("\\s*m$", "", input$mooring_depths))

      data_ctd %>%
        filter(
          date_time >= !! dt_range[1],
          date_time < !! dt_range[2],
          depth_label %in% mooring_depths
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
        "beam_heading_corrected",
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
        list(
          frequency = frequency,
          date_time = data_icl_nc_date_time[integer(0)],
          intensity = array(data = numeric(), dim = c(0, length(frequency)))
        )
      } else {
        list(
          frequency = frequency,
          date_time = data_icl_nc_date_time[dt_dim_values],
          intensity = ncvar_get(
            data_icl_nc,
            "icl_intensity",
            start = c(dim_min, 1),
            count = c(dim_count, -1)
          )
        )
      }
    })

    reactiveValues(
      global_date_range = global_date_range,
      datetime_range = datetime_range,
      ctd = ctd,
      met = met,
      baro = baro,
      lgh = lgh,
      pcm = pcm,
      adp_meta = adp_meta,
      ips_meta = ips_meta,
      icl_meta = icl_meta,
      icl_intensity = icl_intensity
    )
  })
}
