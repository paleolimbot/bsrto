
library(shiny)

baroUI <- function(id = "baro") {
  tagList(
    plotOutput(NS(id, "shore_temp"), height = 150),
    plotOutput(NS(id, "shore_press"), height = 150)
  )
}

baroServer <- function(lang, data, id = "baro") {
  moduleServer(id, function(input, output, session) {

    output$shore_temp <- renderPlot({
      data_plot_datetime(
        data$baro(),
        "shore_temp", "Temperature [°C]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$shore_press <- renderPlot({
      data_plot_datetime(
        data$baro(),
        "shore_press", "Pressure [dbar]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

  })
}
