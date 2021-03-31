
# packages <- c("shiny", "dplyr", "ggplot2", "shiny.i18n", "lubridate", "DT", "ncdf4", "fs", "leaflet", "metR", "tidyr")
# install.packages(packages)

library(shiny)
ggplot2::theme_set(ggplot2::theme_bw())

# important: data refresh! (milliseconds)
options(bsrto.data_refresh_interval = 15 * 60 * 1000)

# modules (the encoding bit is for interactive
# development on Windows, where not specifying this
# results in mangled non-ASCII text)
source("app-i18n.R", encoding = "UTF-8")
source("app-data.R", encoding = "UTF-8")

source("app-dash.R", encoding= "UTF-8")

source("app-ctd.R", encoding = "UTF-8")
source("app-met.R", encoding = "UTF-8")
source("app-baro.R", encoding = "UTF-8")
source("app-lgh.R", encoding = "UTF-8")
source("app-adp.R", encoding = "UTF-8")
source("app-about.R", encoding = "UTF-8")
source("app-ips.R", encoding = "UTF-8")
source("app-icl.R", encoding = "UTF-8")

# A hack to include some right-aligned elements in the navbar page
navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)

  form <- tags$div(
    style = "text-align: right; padding: 15px 10px 0; position: absolute; top: 0; right: 0;",
    inputs
  )
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)

  navbar
}

ui <- tags$div(
  i18nStartBody(),

  navbarPageWithInputs(
    i18n$t("Barrow Strait Real-Time Observatory"),
    tabPanel(i18n$t("Dashboard"), dashUI()),
    navbarMenu(
      i18n$t("Data"),
      tabPanel(i18n$t("Water properties"), ctdUI()),
      tabPanel(i18n$t("Currents"), adpUI()),
      tabPanel(i18n$t("Barometric pressure"), baroUI()),
      tabPanel(i18n$t("Sound"), iclUI()),
      tabPanel(i18n$t("Ice thickness"), ipsUI()),
      tabPanel(i18n$t("Current conditions at Resolute Airport"), metUI()),
      tabPanel(i18n$t("Log files"), lghUI())
    ),
    tabPanel(i18n$t("About"), aboutUI()),
    inputs = tags$div(
      style = "text-align: right; vertical-align: middle;",
      i18nUI()
    ),

    header = div(
      # hide the data filters when on the "About" panel
      conditionalPanel("!(jQuery('#about-map').is(':visible'))", dataUI())
    ),

    # tabPanel content goes here

    footer = div()
  )
)

server <- function(input, output, session) {
  lang <- i18nServer()
  data <- dataServer(lang)

  dashServer(lang, data)

  ctdServer(lang, data)
  adpServer(lang, data)
  baroServer(lang, data)
  iclServer(lang, data)
  ipsServer(lang, data)
  metServer(lang, data)
  lghServer(lang, data)

  aboutServer(lang)
}

shinyApp(ui, server)
