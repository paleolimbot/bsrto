
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

    reactiveValues(
      global_date_range = global_date_range,
      datetime_range = datetime_range,
      ctd = ctd,
      met = met,
      baro = baro,
      lgh = lgh
    )
  })
}