
library(shiny)

iclUI <- function(id = "icl") {
  tagList(
    plotOutput(NS(id, "intensity"))
  )
}

iclServer <- function(lang, data, id = "icl") {
  moduleServer(id, function(input, output, session) {

    output$intensity <- renderPlot({
      intensity <- data$icl_intensity()
      dt_range <- data$datetime_range()

      # Incoming data is at a resolution up to one per second
      # which takes a while to render. Depending on the datetime range,
      # set a different level of aggregation.
      if (isTRUE(diff(dt_range) > as.difftime(120, units = "days"))) {
        date_agr = "week"
        date_time_grid <- seq(
          lubridate::floor_date(dt_range[1], "week"),
          dt_range[2],
          by = "week"
        )
      } else if (isTRUE(diff(dt_range) > as.difftime(10, units = "days"))) {
        date_agr <- "day"
        date_time_grid <- seq(
          lubridate::floor_date(dt_range[1], "day"),
          dt_range[2],
          by = "day"
        )
      } else {
        date_agr <- "hour"
        date_time_grid <- seq(
          lubridate::floor_date(dt_range[1], "hour"),
          dt_range[2],
          by = "hour"
        )
      }

      dims <- expand.grid(
        date_time = intensity$date_time,
        frequency = intensity$frequency
      )

      dims$intensity <- as.integer(intensity$intensity)

      intensity_agr <- dims %>%
        mutate(
          date_time = lubridate::floor_date(date_time, date_agr)
        ) %>%
        group_by(date_time, frequency) %>%
        summarise(intensity = median(intensity, na.rm = TRUE), .groups = "drop")

      p <- expand.grid(
        date_time = date_time_grid,
        frequency = intensity$frequency
      ) %>%
        left_join(intensity_agr, by = c("date_time", "frequency")) %>%
        ggplot(aes(date_time, frequency)) +
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
