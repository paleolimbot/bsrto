
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

plot_adp_cell_enu <- function(data, var, lab = var,
                              datetime_range = range(data$date_time, na.rm = TRUE),
                              lang = "en") {
  facet <- if (nrow(data) > 0) {
    facet_grid(
      vars(east_north_up),
      labeller = labeller(
        east_north_up = function(x) {
          i18n_t(c("vEast", "vNorth", "vUp")[as.integer(x)], lang)
        }
      )
    )
  }

  p <- ggplot(data, aes(date_time, distance)) +
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

currentsUI <- function(id = "currents") {
  tagList(
    uiOutput(NS(id, "enu_input")),

    plotOutput(NS(id, "velocity"), height = 400),
    plotOutput(NS(id, "bottom_velocity"), height = 300),
  )
}

currentsServer <- function(lang, data, id = "currents") {
  moduleServer(id, function(input, output, session) {

    output$enu_input <- renderUI({
      checkboxGroupInput(
        NS(id, "enu"), NULL,
        choiceNames = i18n_t(c("vEast", "vNorth", "vUp", "vTotal"), lang()),
        choiceValues = c("east", "north", "up", "total"),
        selected = c("east", "north", "up", "total"),
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

    output$bottom_velocity <- renderPlot({
      which_enu <- input$enu

      df <- data$adp_bottom_velocity() %>%
        select(
          date_time,
          bottom_velocity_east,
          bottom_velocity_north,
          bottom_velocity_up,
          bottom_velocity_total
        ) %>%
        pivot_longer(-date_time) %>%
        mutate(
          name = gsub("^bottom_velocity_", "", name)
        ) %>%
        filter(name %in% !! which_enu)

      data_plot_datetime(
        df,
        "value", "Bottom velocity [m/s]",
        mapping = aes(col = name),
        datetime_range = data$datetime_range(),
        lang = lang(),
        extra = list(
          scale_color_discrete(
            limits = c("east", "north", "up", "total"),
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
