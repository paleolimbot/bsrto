
library(shiny)
library(ggplot2)

# eventually this should be non-ggplot
plot_ctd <- function(data, var, lab = var,
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
        geom_point(
          aes(col = factor(depth_label, levels = c("40", "60", "160"))),
          na.rm = TRUE
        ) +
        scale_x_datetime(
          limits = datetime_range,

        ) +
        (if (reverse) scale_y_reverse()) +
        scale_color_brewer(
          type = "qual", palette = 1,
          limits = factor(c(40, 60, 160)),
          labels = paste(c(40, 60, 160), "m"),
          guide = "none"
        ) +
        labs(x = NULL, y = i18n_t(lab, lang)) +
        theme(legend.position = "top")
    )
  ))
}

if (FALSE) {
  plot_ctd(data_ctd %>% slice(5000:6000), "temperature", "Temperature [°C]", lang = "fr")
  plot_ctd(ctd, "conductivity", "Conductivity [S/m]")
  plot_ctd(ctd, "pressure", "Pressure [dbar]", reverse = TRUE)
  plot_ctd(ctd, "oxygen", "Oxygen [mg/L]")
  plot_ctd(ctd, "salinity_calc", "Salinity [psal]")
  plot_ctd(ctd, "sound_speed_calc", "Sound Speed [m/s]")
}

ctdUI <- function(id = "ctd") {
  tagList(
    plotOutput(NS(id, "temperature"), height = 150),
    plotOutput(NS(id, "conductivity"), height = 150),
    plotOutput(NS(id, "pressure"), height = 150),
    plotOutput(NS(id, "oxygen"), height = 150),
    plotOutput(NS(id, "salinity"), height = 150),
    plotOutput(NS(id, "sound_speed"), height = 150)
  )
}

ctdServer <- function(lang, data, id = "ctd") {
  moduleServer(id, function(input, output, session) {

    output$temperature <- renderPlot({
      plot_ctd(
        data$ctd(),
        "temperature", "Temperature [°C]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$conductivity <- renderPlot({
      plot_ctd(
        data$ctd(),
        "conductivity", "Conductivity [S/m]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$pressure <- renderPlot({
      plot_ctd(
        data$ctd(),
        "pressure", "Pressure [dbar]",
        datetime_range = data$datetime_range(),
        reverse = TRUE,
        lang = lang()
      )
    })

    output$oxygen <- renderPlot({
      plot_ctd(
        data$ctd(),
        "oxygen", "Dissolved oxygen [mg/L]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$salinity <- renderPlot({
      plot_ctd(
        data$ctd(),
        "salinity_calc", "Salinity [psal]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$sound_speed <- renderPlot({
      plot_ctd(
        data$ctd(),
        "sound_speed_calc", "Sound speed [m/s]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })
  })
}
