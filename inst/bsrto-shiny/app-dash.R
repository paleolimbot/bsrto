
library(shiny)


dashUI <- function(id = "dash") {
  tagList(
    plotOutput(NS(id, "ctd_temperature"), height = 200),
    plotOutput(NS(id, "met_temp"), height = 150),
    plotOutput(NS(id, "ips_draft"))
  )
}

dashServer <- function(lang, data, id = "dash") {
  moduleServer(id, function(input, output, session) {

    output$ctd_temperature <- renderPlot({
      data_plot_datetime(
        data$ctd(),
        "temperature", "Water temperature [°C]",
        data$datetime_range(),
        lang(),
        mapping = aes(col = factor(depth_label, levels = c("40", "60", "160"))),
        extra = list(
          scale_color_brewer(
            type = "qual", palette = 1,
            limits = factor(c(40, 60, 160)),
            labels = paste(c(40, 60, 160), "m"),
            name = i18n_t("Mooring Depth", lang())
          ),
          theme(legend.position = "bottom")
        )
      )
    })

    output$met_temp <- renderPlot({
      data_plot_datetime(
        data$met(),
        "temp", "Air temperature [°C]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$ips_draft <- renderPlot({
      # occurs on initial load
      if (length(data$datetime_range()) != 2) {
        return()
      }

      # trick to insert gaps when the distance between measurements
      # is too large
      ips_meta <- data$ips_meta()

      if (nrow(ips_meta) > 0) {
        ips_meta$.group <- c(0, cumsum(
          as.numeric(diff(ips_meta$date_time), units = "hours") > 12
        ))
      } else {
        ips_meta$.group <- double(0)
      }

      # these should get flagged upstream instead of here in the future
      ips_meta$draft_min[ips_meta$draft_min < 0] <- NA_real_

      print(suppressWarnings(
        withr::with_locale(
          c(LC_TIME = paste0(lang(), "_CA")),

          ggplot(ips_meta, aes(x = date_time, group = .group)) +
            geom_ribbon(
              aes(ymin = draft_min, ymax = draft_max),
              fill = "grey60",
              alpha = 0.3
            ) +
            geom_line(aes(y = draft_mean), lty = 2) +
            scale_x_datetime(
              limits = data$datetime_range()
            ) +
            labs(x = NULL, y = i18n_t("Ice draft [m]", lang()))
        )
      ))
    })


  })
}
