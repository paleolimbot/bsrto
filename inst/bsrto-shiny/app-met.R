
library(shiny)


# eventually this should be non-ggplot
plot_met <- function(data, var, lab = var,
                     datetime_range = range(data$date_time, na.rm = TRUE),
                     reverse = FALSE,
                     lang = "en") {
  # occurs on initial load
  if (length(datetime_range) != 2) {
    return()
  }

  # there is no easy way to translate date labels without
  # explicit LC_TIME support for the other language
  # (not necessarily the case for my interactive Window development)
  print(suppressWarnings(
    withr::with_locale(
      c(LC_TIME = paste0(lang, "_CA")),
      ggplot(data, aes(date_time, .data[[var]])) +
        geom_line(na.rm = TRUE) +
        scale_x_datetime(
          limits = datetime_range
        ) +
        labs(x = NULL, y = i18n_t(lab, lang))
    )
  ))
}

if (FALSE) {
  plot_met(data_met, "temp", "Temperature [°C]")
}

metUI <- function(id = "met") {
  tagList(
    plotOutput(NS(id, "temp"), height = 150),
    plotOutput(NS(id, "dew_point_temp"), height = 150),
    plotOutput(NS(id, "rel_hum"), height = 150),
    plotOutput(NS(id, "wind_dir"), height = 150),
    plotOutput(NS(id, "wind_spd"), height = 150),
    plotOutput(NS(id, "stn_press"), height = 150),
    plotOutput(NS(id, "wind_chill"), height = 150)
  )
}

metServer <- function(lang, data, id = "met") {
  moduleServer(id, function(input, output, session) {

    output$temp <- renderPlot({
      plot_met(
        data$met(),
        "temp", "Temperature [°C]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$dew_point_temp <- renderPlot({
      plot_met(
        data$met(),
        "dew_point_temp", "Dew Point [°C]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$rel_hum <- renderPlot({
      plot_met(
        data$met(),
        "rel_hum", "Relative Humidity [%]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$wind_dir <- renderPlot({
      plot_met(
        data$met(),
        "wind_dir", "Wind Direction [°]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$wind_spd <- renderPlot({
      plot_met(
        data$met(),
        "wind_spd", "Wind Speed [km/h]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$stn_press <- renderPlot({
      plot_met(
        data$met(),
        "stn_press", "Pressure [dbar]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$wind_chill <- renderPlot({
      plot_met(
        data$met(),
        "wind_chill", "Wind Chill [°C]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

  })
}
