
library(shiny)
ggplot2::theme_set(ggplot2::theme_bw())

# modules
source("app-i18n.R", encoding = "UTF-8")
source("app-data.R", encoding = "UTF-8")

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
      tabPanel(i18n$t("Currents"), adpUI()),
      tabPanel(i18n$t("Barometric pressure"), baroUI()),
      tabPanel(i18n$t("Sound"), iclUI()),
      tabPanel(i18n$t("Ice thickness"), ipsUI()),
      tabPanel(i18n$t("Current conditions at Resolute Airport"), metUI()),
      tabPanel(i18n$t("Log files"), lghUI())
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
  data <- dataServer(lang)

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
