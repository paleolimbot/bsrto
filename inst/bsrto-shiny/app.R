
library(shiny)

# modules
source("app-i18n.R")

# https://www.bio.gc.ca/science/newtech-technouvelles/observatory-observatoire-en.php
# https://www.bio.gc.ca/science/newtech-technouvelles/observatory-observatoire-fr.php

# A hack to include some right-aligned elements in the navbar page
navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}

ui <- tags$div(
  i18nStartBody(),

  navbarPageWithInputs(
    i18n$t("bsrto_full"),
    navbarMenu(
      "Data",
      tabPanel(i18n$t("Water Properties")),
      tabPanel(i18n$t("Currents")),
      tabPanel(i18n$t("Shore-Station Barometer")),
      tabPanel(i18n$t("Sound")),
      tabPanel(i18n$t("Ice Thickness")),
      tabPanel(i18n$t("Resolute Weather"))
    ),
    tabPanel(i18n$t("Logs")),
    tabPanel(i18n$t("About")),
    inputs = tags$div(style = "float: right;", i18nUI())
  )
)

server <- function(input, output, session) {
  i18nServer()
}

shinyApp(ui, server)
