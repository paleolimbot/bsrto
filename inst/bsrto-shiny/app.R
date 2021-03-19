
library(shiny)

# modules
source("app-i18n.R")
source("app-about.R")

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
      i18n$t("Data"),
      tabPanel(i18n$t("Water properties")),
      tabPanel(i18n$t("Currents")),
      tabPanel(i18n$t("Barometric pressure")),
      tabPanel(i18n$t("Sound")),
      tabPanel(i18n$t("Ice thickness")),
      tabPanel(i18n$t("Current conditions at Resolute Airport")),
      tabPanel(i18n$t("Log files"))
    ),
    tabPanel(i18n$t("About"), aboutUI()),
    inputs = tags$div(style = "float: right;", i18nUI())
  )
)

server <- function(input, output, session) {
  i18nServer()

  # There might be a more clean way to code this, but lang() as
  # a reactive value that modules can take as a dependency is probably the
  # best way to go about this.
  lang <- reactive({ input$`i18n-lang` })

  aboutServer(lang)
}

shinyApp(ui, server)
