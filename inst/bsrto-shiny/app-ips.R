
library(shiny)
library(ggplot2)

ipsUI <- function(id = "ips") {
  tagList(
    dataBsrtoPlotOutput(
      NS(id, "draft"),
      height = 400,
      dblclick = dblclickOpts(id = NS(id, "draft_click"))
    ),
    plotOutput(NS(id, "bins"), height = 300)
  )
}

ipsServer <- function(lang, data, id = "ips") {
  moduleServer(id, function(input, output, session) {

    output$draft <- renderPlot({
      # trick to insert gaps when the distance between measurements
      # is too large
      ips_meta <- data$ips_meta()

      names(ips_meta) <- gsub("_corrected$", "", names(ips_meta))

      ips_meta$.group <- c(0, cumsum(
        as.numeric(diff(ips_meta$date_time), units = "hours") > 12
      ))

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

    output$bins <- renderPlot({
      click <- input$draft_click
      geom <- NULL

      if (!is.null(click)) {
        current_range <- data$datetime_range()
        current_diff <- diff(current_range)
        datetime_click <- current_range[1] + click$x * current_diff

        # find a relevant datetime that has data
        datetime_diff <- abs(as.numeric(datetime_click - data_ips_nc_date_time, units = "hours"))
        datetime_which <- which.min(datetime_diff)

        if (datetime_diff[datetime_which] < 12) {
          bins <- ncdf4::ncvar_get(
            data_ips_nc,
            "ips_count_corrected",
            start = c(1, datetime_which), count = c(-1, 1)
          )

          distance <- ncdf4::ncvar_get(data_ips_nc, "distance_corrected", start = 1, count = -1)

          geom <- list(
            geom_col(
              aes(distance, count),
              data = tibble(distance = distance, count = bins)
            ),
            labs(
              subtitle = as.character(data_ips_nc_date_time[datetime_which])
            )
          )
        }
      }

      render_with_lang(lang(), {
        ggplot() +
          geom +
          theme_bsrto_margins(TRUE) +
          guides(y = guide_axis_fixed_width()) +
          labs(
            y = "Number of measurements",
            x = i18n_t("Ice draft [m]", lang()),
            caption = i18n_t("ice_draft_histogram_instructions", lang())
          )
      })
    })

  })
}
