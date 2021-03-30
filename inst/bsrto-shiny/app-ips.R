
library(shiny)
library(ggplot2)

ipsUI <- function(id = "ips") {
  tagList(
    plotOutput(NS(id, "draft"), height = 400)
  )
}

ipsServer <- function(lang, data, id = "ips") {
  moduleServer(id, function(input, output, session) {

    output$draft <- renderPlot({
      # trick to insert gaps when the distance between measurements
      # is too large
      ips_meta <- data$ips_meta()
      ips_meta$.group <- c(0, cumsum(
        as.numeric(diff(ips_meta$date_time), units = "hours") > 12
      ))

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