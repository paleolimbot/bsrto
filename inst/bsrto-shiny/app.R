
library(dplyr)
library(ggplot2)
library(shiny)

theme_set(theme_bw())

# Data -----------------

# eventually these should be SQLite tables or something else
# that can be lazily queried
ctd <- readr::read_csv(
    here::here("ctd.csv"),
    col_types = readr::cols(
        file = readr::col_character(),
        date_time = readr::col_datetime(),
        .default = readr::col_double()
    )
)

# Utility --------------

plot_ctd <- function(data, var, lab = var,
                     datetime_range = range(data$date_time, na.rm = TRUE),
                     reverse = FALSE, mooring_depth_guide = FALSE) {
    ggplot(data, aes(date_time, .data[[var]])) +
        geom_point(aes(col = factor(depth_label, levels = c("40", "60", "160")))) +
        scale_x_datetime(limits = datetime_range) +
        (if (reverse) scale_y_reverse()) +
        scale_color_brewer(
            type = "qual", palette = 1,
            limits = factor(c(40, 60, 160)),
            labels = paste(c(40, 60, 160), "m"),
            name = "Mooring Depth",
            guide = if (!mooring_depth_guide) "none" else "legend"
        ) +
        labs(x = NULL, y = lab, col = "Mooring Depth") +
        theme(legend.position = "top")
}

if (FALSE) {
    plot_ctd(ctd, "temperature", "Temperature [°C]", mooring_depth_guide = TRUE)
    plot_ctd(ctd, "conductivity", "Conductivity [S/m]")
    plot_ctd(ctd, "pressure", "Pressure [dbar]", reverse = TRUE)
    plot_ctd(ctd, "oxygen", "Oxygen [mg/L]")
    plot_ctd(ctd, "salinity_calc", "Salinity [psal]")
    plot_ctd(ctd, "sound_speed_calc", "Sound Speed [m/s]")
}

# Globals ---------------

globals <- list(
    first_date = as.Date(min(ctd$date_time)),
    last_date = as.Date(max(ctd$date_time))
)


# UI -----------------

ui <- fluidPage(

    sidebarLayout(
        sidebarPanel(
            dateRangeInput("date_range", "Date Range",
                           start = globals$last_date - 7L,
                           end = globals$last_date + 1L,
                           min = globals$first_date,
                           max = globals$last_date + 1L),
        ),

        mainPanel(
           plotOutput("ctd_temperature", height = 190),
           plotOutput("ctd_conductivity", height = 150),
           plotOutput("ctd_pressure", height = 150),
           plotOutput("ctd_oxygen", height = 150),
           plotOutput("ctd_salinity", height = 150),
           plotOutput("ctd_sound_speed", height = 150)
        )
    )
)


# Server ----------------

server <- function(input, output) {

    datetime_range <- reactive({
        dt_range <- as.POSIXct(input$date_range)
        attr(dt_range, "tzone") <- "UTC"
        dt_range
    })

    ctd_filter <- reactive({
        dt_range <- datetime_range()
        ctd %>%
            filter(
                date_time >= !! dt_range[1],
                date_time < !! dt_range[2]
            )
    })

    output$ctd_temperature <- renderPlot({
        plot_ctd(
            ctd_filter(),
            "temperature", "Temperature [°C]",
            datetime_range = datetime_range(),
            mooring_depth_guide = TRUE
        )
    })

    output$ctd_conductivity <- renderPlot({
        plot_ctd(
            ctd_filter(),
            "conductivity", "Conductivity [S/m]",
            datetime_range = datetime_range()
        )
    })

    output$ctd_pressure <- renderPlot({
        plot_ctd(
            ctd_filter(),
            "conductivity", "Conductivity [S/m]",
            datetime_range = datetime_range()
        )
    })

    output$ctd_oxygen <- renderPlot({
        plot_ctd(
            ctd_filter(),
            "oxygen", "Dissolved oxygen [mg/L]",
            datetime_range = datetime_range()
        )
    })

    output$ctd_salinity <- renderPlot({
        plot_ctd(
            ctd_filter(),
            "salinity_calc", "Salinity [psal]",
            datetime_range = datetime_range()
        )
    })

    output$ctd_sound_speed <- renderPlot({
        plot_ctd(
            ctd_filter(),
            "sound_speed_calc", "Sound speed [m/s]",
            datetime_range = datetime_range()
        )
    })
}

# Run ------------

shinyApp(ui = ui, server = server)
