
library(shiny)
library(bsrto)
library(dplyr)
library(tidyr)


dashUI <- function(id = "dash") {
  tagList(
    plotOutput(NS(id, "adp_average_velocity"), height = 200),
    plotOutput(NS(id, "adp_bottom_velocity"), height = 200),
    plotOutput(NS(id, "ctd_temperature"), height = 200),
    plotOutput(NS(id, "met_temp"), height = 150),
    plotOutput(NS(id, "ips_draft"))
  )
}

dashServer <- function(lang, data, id = "dash") {
  moduleServer(id, function(input, output, session) {

    output$adp_average_velocity <- renderPlot({
      df <- data$adp_bottom_velocity()

      if (nrow(df) > 0) {
        df <- data$adp_bottom_velocity() %>%
          mutate(
            east_north_up = c("east", "north", "up")[east_north_up] %>%
              factor(levels = c("east", "north", "up"))
          ) %>%
          select(date_time, east_north_up, average_velocity) %>%
          pivot_wider(
            names_from = east_north_up,
            values_from = average_velocity,
          ) %>%
          mutate(
            velocity_total = sqrt(east ^ 2 + north ^ 2),
            velocity_direction = headings::hdg_from_uv(headings::uv(east, north))
          )
      } else {
        df <- tibble::tibble(
          date_time = as.Date(integer(0)),
          velocity_total = double(0),
          velocity_direction = double(0)
        )
      }

      data_plot_datetime(
        df,
        "velocity_total", "Depth-averaged velocity [m/s]",
        datetime_range = data$datetime_range(),
        lang = lang(),
        extra = list(
          if ((nrow(df) < 100) && (nrow(df) > 0)) {
            metR::geom_arrow(
              aes(
                mag = 1,
                angle = velocity_direction + 180
              ),
              direction = "cw",
              start = -90
            )
          }
        )
      )
    })

    output$adp_bottom_velocity <- renderPlot({
      df <- data$adp_bottom_velocity()

      if (nrow(df) > 0) {
      df <- data$adp_bottom_velocity() %>%
        filter(
          bottom_velocity_flag == bs_flag("probably good data")
        ) %>%
        mutate(
          east_north_up = c("east", "north", "up")[east_north_up] %>%
            factor(levels = c("east", "north", "up"))
        ) %>%
        select(date_time, east_north_up, bottom_velocity) %>%
        pivot_wider(
          names_from = east_north_up,
          values_from = bottom_velocity,
        ) %>%
        mutate(
          bottom_velocity_total = sqrt(east ^ 2 + north ^ 2),
          bottom_velocity_direction = headings::hdg_from_uv(headings::uv(east, north))
        )
      } else {
        df <- tibble::tibble(
          date_time = as.Date(integer(0)),
          bottom_velocity_total = double(0),
          bottom_velocity_direction = double(0)
        )
      }

      data_plot_datetime(
        df,
        "bottom_velocity_total", "Ice velocity [m/s]",
        datetime_range = data$datetime_range(),
        lang = lang(),
        extra = list(
          if ((nrow(df) < 100) && (nrow(df) > 0)) {
            metR::geom_arrow(
              aes(
                mag = 1,
                angle = bottom_velocity_direction + 180
              ),
              direction = "cw",
              start = -90
            )
          }
        )
      )
    })

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
            name = i18n_t("Mooring depth", lang())
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

      ips_meta <- data$ips_meta()

      # trick to insert gaps when the distance between measurements
      # is too large
      if (nrow(ips_meta) > 0) {
        ips_meta$.group <- c(0, cumsum(
          as.numeric(diff(ips_meta$date_time), units = "hours") > 12
        ))
      } else {
        ips_meta$.group <- double(0)
      }

      # these should get flagged upstream instead of here in the future
      ips_meta$draft_min[ips_meta$draft_min < 0] <- NA_real_

      render_with_lang(lang(), {
        ggplot(ips_meta, aes(x = date_time, group = .group)) +
          geom_ribbon(
            aes(ymin = draft_min, ymax = draft_max),
            fill = "grey60",
            alpha = 0.3
          ) +
          geom_line(aes(y = draft_mean), lty = 2) +
          scale_bsrto_datetime(
            limits = data$datetime_range()
          ) +
          labs(x = NULL, y = i18n_t("Ice draft [m]", lang())) +
          guides(y = guide_axis_fixed_width()) +
          theme_bsrto_margins(pad_right = TRUE)
      })
    })


  })
}

