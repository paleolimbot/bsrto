
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

data_adp_nc <- nc_open(file.path(built_dir, "adp.nc"))
data_adp_nc_date_time <- as.POSIXct(
  ncvar_get(data_adp_nc, "date_time"),
  origin = "1970-01-01 00:00:00",
  tz = "UTC"
)



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

    adp_meta <- reactive({
      dt_range <- datetime_range()

      dt_dim_values <- which(
        (data_adp_nc_date_time >= dt_range[1]) &
          (data_adp_nc_date_time < dt_range[2])
      )
      stopifnot(all(diff(dt_dim_values) == 1L))
      dim_min <- min(dt_dim_values)
      dim_count <- length(dt_dim_values)

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

      if (dim_count == 0) {
        vals <- lapply(meta_vars, function(x) double(0))
        names(vals) <- meta_vars
        tibble::tibble(date_time = data_adp_nc_date_time[integer(0)], !!! vals)
      } else {
        vals <- lapply(
          data_adp_nc$var[meta_vars],
          function(x) ncvar_get(
            data_adp_nc, x,
            start = dim_min,
            count = dim_count
          )
        )

        file <- ncvar_get(
          data_adp_nc, "file",
          start = c(1, dim_min),
          count = c(12, dim_count)
        )

        tibble::new_tibble(
          c(
            list(file = file, date_time = data_adp_nc_date_time[dt_dim_values]),
            vals
          ),
          nrow = dim_count
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
      adp_meta = adp_meta
    )
  })
}
