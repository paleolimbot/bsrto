
library(shiny)
ggplot2::theme_set(ggplot2::theme_bw())

# modules
source("app-i18n.R")
source("app-about.R")
source("app-data.R")
source("app-ctd.R", encoding = "UTF-8")

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
      tabPanel(i18n$t("Water properties"), ctdUI()),
      tabPanel(i18n$t("Currents")),
      tabPanel(i18n$t("Barometric pressure")),
      tabPanel(i18n$t("Sound")),
      tabPanel(i18n$t("Ice thickness")),
      tabPanel(i18n$t("Current conditions at Resolute Airport")),
      tabPanel(i18n$t("Log files"))
    ),
    tabPanel(i18n$t("About"), aboutUI()),
    inputs = tags$div(style = "float: right;", i18nUI()),

    header = div(
      dataUI()
    ),

    # tabPanel content goes here

    footer = div()
  )
)

server <- function(input, output, session) {
  lang <- i18nServer()
  aboutServer(lang)
  data <- dataServer(lang)
  ctdServer(lang, data)
}

shinyApp(ui, server)
