
library(shiny)

# eventually this should be non-ggplot
plot_baro <- function(data, var, lab = var,
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
        geom_point(na.rm = TRUE) +
        scale_x_datetime(
          limits = datetime_range
        ) +
        labs(x = NULL, y = i18n_t(lab, lang))
    )
  ))
}

baroUI <- function(id = "baro") {
  tagList(
    plotOutput(NS(id, "shore_temp"), height = 150),
    plotOutput(NS(id, "shore_press"), height = 150)
  )
}

baroServer <- function(lang, data, id = "baro") {
  moduleServer(id, function(input, output, session) {

    output$shore_temp <- renderPlot({
      plot_baro(
        data$baro(),
        "shore_temp", "Temperature [°C]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$shore_press <- renderPlot({
      plot_baro(
        data$baro(),
        "shore_press", "Pressure [dbar]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

  })
}
