
library(shiny)

iclUI <- function(id = "icl") {
  tagList(
    plotOutput(NS(id, "intensity"))
  )
}

iclServer <- function(lang, data, id = "icl") {
  moduleServer(id, function(input, output, session) {

    output$intensity <- renderPlot({
      dt_range <- data$datetime_range()

      p <- ggplot(data$icl_intensity(), aes(date_time, frequency)) +
        geom_raster(aes(fill = intensity)) +
        scale_fill_viridis_c(limits = c(0, 70), oob = scales::squish, guide = "none") +
        scale_x_datetime(limits = dt_range) +
        scale_y_continuous(expand = expansion(0, 0)) +
        labs(x = NULL, y = i18n_t("Frequency [Hz]", lang()))

      suppressWarnings(
        withr::with_locale(
          c(LC_TIME = paste0(lang(), "_CA")),
          print(p)
        )
      )
    })

  })
}
