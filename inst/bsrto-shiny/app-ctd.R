
library(shiny)
library(ggplot2)

# eventually this should be non-ggplot
plot_ctd <- function(data, var, lab = var,
                     datetime_range = range(data$date_time, na.rm = TRUE),
                     reverse = FALSE,
                     lang = "en") {
 data_plot_datetime(
   data, var, lab, datetime_range, lang,
   mapping = aes(col = factor(depth_label, levels = c("40", "60", "160"))),
   extra = list(
     (if (reverse) scale_y_reverse()),
     scale_color_brewer(
       type = "qual", palette = 1,
       limits = factor(c(40, 60, 160)),
       labels = paste(c(40, 60, 160), "m"),
       guide = "none"
     )
   )
 )
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
