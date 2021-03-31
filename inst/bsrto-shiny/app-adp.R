
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)


plot_adp_beam <- function(data, var, lab = var,
                          datetime_range = range(data$date_time, na.rm = TRUE),
                          lang = "en") {
  data_plot_datetime(
    data, var, lab, datetime_range, lang,
    mapping = aes(col = factor(n_beam, levels = c("1", "2", "3", "4"))),
    extra = list(
      scale_color_brewer(
        type = "qual", palette = 2,
        limits = factor(1:4),
        labels = sprintf("%s %d", i18n_t("Beam", lang), 1:4),
        guide = "none"
      )
    )
  )
}

plot_adp_cell <- function(data, var, lab = var,
                          datetime_range = range(data$date_time, na.rm = TRUE),
                          lang = "en") {
  facet <- if (nrow(data) > 0) {
    facet_grid(
      vars(n_beam),
      labeller = labeller(
        n_beam = function(x) sprintf("%s %s", i18n_t("Beam", lang), x)
      )
    )
  }

  p <- ggplot(data, aes(date_time, distance)) +
    geom_raster(aes(fill = .data[[var]])) +
    scale_fill_viridis_c(oob = scales::squish) +
    scale_x_datetime(limits = datetime_range) +
    scale_y_continuous(expand = expansion(0, 0)) +
    facet +
    labs(
      x = NULL,
      y = i18n_t("Distance [m]", lang),
      fill = i18n_t(lab, lang)
    ) +
    theme(legend.position = "top")

  suppressWarnings(
    withr::with_locale(
      c(LC_TIME = paste0(lang, "_CA")),
      print(p)
    )
  )
}

adpUI <- function(id = "adp") {
  tagList(
    uiOutput(NS(id, "beam_input")),

    plotOutput(NS(id, "bottom_velocity"), height = 300),

    plotOutput(NS(id, "velocity_raw"), height = 150 * 4),
    plotOutput(NS(id, "correlation"), height = 150 * 4),
    plotOutput(NS(id, "echo_intensity"), height = 150 * 4),
    plotOutput(NS(id, "pct_good"), height = 150 * 4),

    plotOutput(NS(id, "bottom_range"), height = 150),
    plotOutput(NS(id, "bottom_velocity_raw"), height = 150),
    plotOutput(NS(id, "bottom_correlation"), height = 150),
    plotOutput(NS(id, "bottom_amplitude"), height = 150),
    plotOutput(NS(id, "bottom_pct_good"), height = 150),

    plotOutput(NS(id, "transducer_depth"), height = 150),
    plotOutput(NS(id, "beam_heading_corrected"), height = 150),
    plotOutput(NS(id, "pitch"), height = 150),
    plotOutput(NS(id, "roll"), height = 150),
    plotOutput(NS(id, "pressure"), height = 150),
    plotOutput(NS(id, "temperature"), height = 150),
    plotOutput(NS(id, "pc_heading"), height = 150),
  )
}

adpServer <- function(lang, data, id = "adp") {
  moduleServer(id, function(input, output, session) {

    output$beam_input <- renderUI({
      checkboxGroupInput(
        NS(id, "beams"), NULL,
        choices = sprintf("%s %d", i18n_t("Beam", lang()), 1:4),
        selected = sprintf("%s %d", i18n_t("Beam", lang()), 1:4),
        inline = TRUE
      )
    })

    adp_beams <- reactive({
      beam_vals <- as.numeric(gsub("[^0-9]", "", input$beams))
      data$adp_beam_meta() %>%
        filter(n_beam %in% !! beam_vals)
    })

    adp_cells <- reactive({
      beam_vals <- as.numeric(gsub("[^0-9]", "", input$beams))
      data$adp_cells() %>%
        filter(n_beam %in% !! beam_vals)
    })

    output$bottom_velocity_east <- renderPlot({
      data_plot_datetime(
        data$adp_bottom_velocity(),
        "bottom_velocity_east", "Bottom velocity (east) [m/s]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$bottom_velocity <- renderPlot({
      df <- data$adp_bottom_velocity() %>%
        select(
          date_time,
          bottom_velocity_east,
          bottom_velocity_north,
          bottom_velocity_up
        ) %>%
        pivot_longer(-date_time) %>%
        rbind(
          data$adp_meta() %>%
            transmute(
              date_time,
              name = "bottom_velocity_error",
              value = bottom_error_velocity
            )
        ) %>%
        mutate(
          name = gsub("^bottom_velocity_", "", name)
        )

      data_plot_datetime(
        df,
        "value", "Bottom velocity [m/s]",
        mapping = aes(col = name),
        datetime_range = data$datetime_range(),
        lang = lang(),
        extra = list(
          scale_color_discrete(
            limits = c("east", "north", "up", "error"),
            labels = i18n_t(
              c("East", "North", "Up", "Error"),
              lang()
            ),
            name = NULL
          ),
          theme(legend.position = "bottom")
        )
      )
    })

    output$velocity_raw <- renderPlot({
      plot_adp_cell(
        adp_cells(),
        "velocity_raw", "Velocity [m/s]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$correlation <- renderPlot({
      plot_adp_cell(
        adp_cells(),
        "correlation", "Correlation",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$echo_intensity <- renderPlot({
      plot_adp_cell(
        adp_cells(),
        "echo_intensity", "Echo intensity [relative units]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$pct_good <- renderPlot({
      plot_adp_cell(
        adp_cells(),
        "pct_good", "Percent made good [%]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$bottom_range <- renderPlot({
      plot_adp_beam(
        adp_beams(),
        "bottom_range", "Bottom range [m]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$bottom_velocity_raw <- renderPlot({
      plot_adp_beam(
        adp_beams(),
        "bottom_velocity_raw", "Bottom velocity [m/s]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$bottom_correlation <- renderPlot({
      plot_adp_beam(
        adp_beams(),
        "bottom_correlation", "bottom_correlation",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$bottom_amplitude <- renderPlot({
      plot_adp_beam(
        adp_beams(),
        "bottom_amplitude", "bottom_amplitude",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$bottom_pct_good <- renderPlot({
      plot_adp_beam(
        adp_beams(),
        "bottom_pct_good", "bottom_pct_good",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$transducer_depth <- renderPlot({
      data_plot_datetime(
        data$adp_meta(),
        "transducer_depth", "Depth [m]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$beam_heading_corrected <- renderPlot({
      data_plot_datetime(
        data$adp_meta(),
        "beam_heading_corrected", "Heading [°]",
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

    output$pc_heading <- renderPlot({
      data_plot_datetime(
        data$pcm(),
        "pc_heading", "Heading (pole compass) [°]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

  })
}
