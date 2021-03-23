
library(shiny)

adpUI <- function(id = "adp") {
  tagList(
    plotOutput(NS(id, "transducer_depth"), height = 150),
    plotOutput(NS(id, "beam_heading_corrected"), height = 150),
    plotOutput(NS(id, "pitch"), height = 150),
    plotOutput(NS(id, "roll"), height = 150),
    plotOutput(NS(id, "pressure"), height = 150),
    plotOutput(NS(id, "temperature"), height = 150),
  )
}

adpServer <- function(lang, data, id = "adp") {
  moduleServer(id, function(input, output, session) {

    output$transducer_depth <- renderPlot({
      data_plot_datetime(
        data$adp_meta(),
        "transducer_depth", "Tranducer Depth [m]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$beam_heading_corrected <- renderPlot({
      data_plot_datetime(
        data$adp_meta(),
        "beam_heading_corrected", "Beam Heading (corrected) [°]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$pitch <- renderPlot({
      data_plot_datetime(
        data$adp_meta(),
        "pitch", "Pitch [°]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$roll <- renderPlot({
      data_plot_datetime(
        data$adp_meta(),
        "roll", "Roll [°]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$salinity <- renderPlot({
      data_plot_datetime(
        data$adp_meta(),
        "salinity", "Salinity [psal]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$pressure <- renderPlot({
      data_plot_datetime(
        data$adp_meta(),
        "pressure", "Pressure [dbar]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$temperature <- renderPlot({
      data_plot_datetime(
        data$adp_meta(),
        "temperature", "Temperature [°C]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$attitude <- renderPlot({
      data_plot_datetime(
        data$adp_meta(),
        "attitude", "Attitude [??]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$contamination_sensor <- renderPlot({
      data_plot_datetime(
        data$adp_meta(),
        "contamination_sensor", "Contamination Sensor",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

  })
}
