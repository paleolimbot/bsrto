
library(shiny)
library(ggplot2)


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

adpUI <- function(id = "adp") {
  tagList(
    uiOutput(NS(id, "beam_input")),

    plotOutput(NS(id, "velocity"), height = 150 * 4),
    plotOutput(NS(id, "correlation"), height = 150 * 4),
    plotOutput(NS(id, "echo_intensity"), height = 150 * 4),
    plotOutput(NS(id, "pct_good"), height = 150 * 4),

    plotOutput(NS(id, "range_lsb"), height = 150),
    plotOutput(NS(id, "range_msb"), height = 150),
    plotOutput(NS(id, "bottom_track_velocity"), height = 150),
    plotOutput(NS(id, "bc"), height = 150),
    plotOutput(NS(id, "ba"), height = 150),
    plotOutput(NS(id, "bg"), height = 150),

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

    output$velocity <- renderPlot({
      dt_range <- data$datetime_range()
      cells <- adp_cells()

      facet <- if (nrow(cells) > 0) {
        facet_grid(
          vars(n_beam),
          labeller = labeller(
            n_beam = function(x) sprintf("%s %s", i18n_t("Beam", lang()), x)
          )
        )
      }

      p <- ggplot(cells, aes(date_time, distance)) +
        geom_raster(aes(fill = velocity)) +
        scale_fill_viridis_c(oob = scales::squish, guide = "none") +
        scale_x_datetime(limits = dt_range) +
        scale_y_continuous(expand = expansion(0, 0)) +
        facet +
        labs(x = NULL, y = i18n_t("Distance [m]", lang()))

      suppressWarnings(
        withr::with_locale(
          c(LC_TIME = paste0(lang(), "_CA")),
          print(p)
        )
      )
    })

    output$range_lsb <- renderPlot({
      plot_adp_beam(
        adp_beams(),
        "range_lsb", "range_lsb",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$range_msb <- renderPlot({
      plot_adp_beam(
        adp_beams(),
        "range_msb", "range_msb",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$bottom_track_velocity <- renderPlot({
      plot_adp_beam(
        adp_beams(),
        "bottom_track_velocity", "bottom_track_velocity",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$bc <- renderPlot({
      plot_adp_beam(
        adp_beams(),
        "bc", "bc",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$ba <- renderPlot({
      plot_adp_beam(
        adp_beams(),
        "ba", "ba",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$bg <- renderPlot({
      plot_adp_beam(
        adp_beams(),
        "bg", "bg",
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
