
library(shiny)

iclUI <- function(id = "icl") {
  tagList(
    dataBsrtoPlotOutput(NS(id, "intensity"))
  )
}

iclServer <- function(lang, data, id = "icl") {
  moduleServer(id, function(input, output, session) {

    output$intensity <- renderPlot({
      dt_range <- data$datetime_range()

      render_with_lang(lang(), {
        ggplot(data$icl_intensity(), aes(date_time, frequency)) +
          geom_raster(aes(fill = intensity)) +
          scale_fill_viridis_c(limits = c(0, 70), oob = scales::squish, guide = "none") +
          scale_bsrto_datetime(limits = dt_range) +
          scale_y_continuous(expand = expansion(0, 0)) +
          labs(x = NULL, y = i18n_t("Frequency [Hz]", lang())) +
          guides(y = guide_axis_fixed_width()) +
          theme_bsrto_margins(pad_right = TRUE)
      })
    })

  })
}
