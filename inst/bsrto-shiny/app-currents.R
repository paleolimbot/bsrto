
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

plot_adp_cell_enu <- function(data, var, lab = var,
                              datetime_range = range(data$date_time, na.rm = TRUE),
                              lang = "en") {
  facet <- if (nrow(data) > 0) {
    list(
      facet_grid(
        vars(east_north_up),
        labeller = labeller(
          east_north_up = function(x) {
            i18n_t(c("vEast", "vNorth", "vUp")[as.integer(x)], lang)
          }
        )
      ),
      theme_bsrto_margins(pad_right = FALSE)
    )
  }

  render_with_lang(lang, {
    ggplot(data, aes(date_time, distance)) +
      geom_raster(aes(fill = .data[[var]])) +
      scale_fill_gradient2(
        limits = function(x) {
          if (is.null(x)) return(c(-1, 1))

          max_mag <- max(abs(x))
          c(-max_mag, max_mag)
        }
      ) +
      scale_bsrto_datetime(limits = datetime_range) +
      scale_y_continuous(expand = expansion(0, 0)) +
      theme_bsrto_margins(pad_right = TRUE) +
      facet +
      labs(
        x = NULL,
        y = i18n_t("Distance [m]", lang),
        fill = i18n_t(lab, lang)
      ) +
      guides(y = guide_axis_fixed_width()) +
      theme(legend.position = "top")
  })
}

currentsUI <- function(id = "currents") {
  tagList(
    uiOutput(NS(id, "enu_input")),

    plotOutput(NS(id, "velocity"), height = 400),
    plotOutput(NS(id, "average_velocity"), height = 300),
    plotOutput(NS(id, "bottom_velocity"), height = 300),
  )
}

currentsServer <- function(lang, data, id = "currents") {
  moduleServer(id, function(input, output, session) {

    output$enu_input <- renderUI({
      checkboxGroupInput(
        NS(id, "enu"), NULL,
        choiceNames = i18n_t(c("vEast", "vNorth", "vUp"), lang()),
        choiceValues = c("east", "north", "up"),
        selected = c("east", "north"),
        inline = TRUE
      )
    })

    output$velocity <- renderPlot({
      which_enu <- match(input$enu, c("east", "north", "up"))

      plot_adp_cell_enu(
        data$adp_velocity() %>%
          filter(east_north_up %in% !! which_enu),
        "velocity", "Velocity [m/s]",
        datetime_range = data$datetime_range(),
        lang = lang()
      )
    })

    output$average_velocity <- renderPlot({
      which_enu <- match(input$enu, c("east", "north", "up"))

      data_plot_datetime(
        data$adp_bottom_velocity() %>%
          filter(
            east_north_up %in% !! which_enu
          ),
        "average_velocity", "Depth-averaged velocity [m/s]",
        mapping = aes(col = c("east", "north", "up")[east_north_up]),
        datetime_range = data$datetime_range(),
        lang = lang(),
        extra = list(
          scale_color_discrete(
            limits = c("east", "north", "up"),
            labels = i18n_t(
              c("vEast", "vNorth", "vUp", "vTotal"),
              lang()
            ),
            name = NULL
          ),
          theme(legend.position = "bottom")
        )
      )
    })

    output$bottom_velocity <- renderPlot({
      which_enu <- match(input$enu, c("east", "north", "up"))

      data_plot_datetime(
        data$adp_bottom_velocity() %>%
          filter(
            east_north_up %in% !! which_enu,
            bottom_velocity_flag == bs_flag("probably good data")
          ),
        "bottom_velocity", "Bottom velocity [m/s]",
        mapping = aes(col = c("east", "north", "up")[east_north_up]),
        datetime_range = data$datetime_range(),
        lang = lang(),
        extra = list(
          scale_color_discrete(
            limits = c("east", "north", "up"),
            labels = i18n_t(
              c("vEast", "vNorth", "vUp", "vTotal"),
              lang()
            ),
            name = NULL
          ),
          theme(legend.position = "bottom")
        )
      )
    })

  })
}
