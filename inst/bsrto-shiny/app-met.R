
library(shiny)

metUI <- function(id = "met") {
  tagList(
    dataBsrtoPlotOutput(NS(id, "temp"), height = 150),
    dataBsrtoPlotOutput(NS(id, "dew_point_temp"), height = 150),
    dataBsrtoPlotOutput(NS(id, "rel_hum"), height = 150),
    dataBsrtoPlotOutput(NS(id, "wind_dir"), height = 150),
    dataBsrtoPlotOutput(NS(id, "wind_spd"), height = 150),
    dataBsrtoPlotOutput(NS(id, "stn_press"), height = 150),
    dataBsrtoPlotOutput(NS(id, "wind_chill"), height = 150)
  )
}

metServer <- function(lang, data, id = "met") {
  moduleServer(id, function(input, output, session) {

    output$temp <- renderPlot({
      data_plot_datetime(
        data$met(),
        "temp", "Temperature [°C]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$dew_point_temp <- renderPlot({
      data_plot_datetime(
        data$met(),
        "dew_point_temp", "Dew Point [°C]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$rel_hum <- renderPlot({
      data_plot_datetime(
        data$met(),
        "rel_hum", "Relative Humidity [%]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$wind_dir <- renderPlot({
      data_plot_datetime(
        data$met(),
        "wind_dir", "Wind Direction [°]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$wind_spd <- renderPlot({
      data_plot_datetime(
        data$met(),
        "wind_spd", "Wind Speed [km/h]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$stn_press <- renderPlot({
      data_plot_datetime(
        data$met(),
        "stn_press", "Pressure [dbar]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$wind_chill <- renderPlot({
      data_plot_datetime(
        data$met(),
        "wind_chill", "Wind Chill [°C]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

  })
}
