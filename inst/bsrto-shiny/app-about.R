
library(shiny)
library(leaflet)

aboutUI <- function(id = "about") {
  tagList(
    p(i18n_t_js("about_text_1")),
    p(i18n_t_js("about_text_2")),
    leafletOutput(NS(id, "map"))
  )
}

aboutServer <- function(lang, id = "about") {
  moduleServer(id, function(input, output, session) {

    output$map <- renderLeaflet({
      provider <- switch(
        lang(),
        fr = providers$OpenStreetMap.France,
        providers$OpenStreetMap
      )

      leaflet() %>%
        addProviderTiles(provider) %>%
        addScaleBar() %>%
        addMarkers(lng = -91.25105, lat = 74.60635) %>%
        setView(lng = -91.25105, lat = 74.60635, zoom = 6)
    })

  })
}
