
library(shiny)

# Setting the default language to fr because there is auto-detection of
# language based on the browser. This allows this to be bested since
# development is taking place in english
i18n <- shiny.i18n::Translator$new(translation_json_path = "translation.json")
i18n$set_translation_language("fr")


navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}

ui <- tags$div(
  shinyjs::useShinyjs(),
  shiny.i18n::usei18n(i18n),
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
    inputs = tags$div(
      style = "float: right;",
      actionButton("lang_toggle", label = i18n$t("other_lang")),
      verbatimTextOutput("lang_dummy"),
      tags$script(HTML('Shiny.addCustomMessageHandler("changetitle", function(x) {document.title=x});'))
    )
  )
)

server <- function(input, output, session) {

  # Fancy way to get the browser's idea of what the language is
  # this won't set the input value right away; code needs to treat this like
  # an input value that could be set by the user at any time.
  shinyjs::runjs("
    var usr_lang_initial_auto =  window.navigator.userLanguage || window.navigator.language;
    Shiny.setInputValue('lang_initial_auto', usr_lang_initial_auto);
  ")

  # An empty output that is rendered initially and when 'lang_initial_auto'
  # is changed (on page load)
  output$lang_dummy <- renderText({
    query <- parseQueryString(session$clientData$url_search)
    new_lang <- NULL
    has_initial_lang <- exists("lang_initial", session$userData)

    if (!has_initial_lang && !is.null(query$lang)) {
      new_lang <- query$lang
      session$userData$lang_initial <- query$lang
    } else if (!has_initial_lang && !is.null(input$lang_initial_auto)) {
      if (is.character(input$lang_initial_auto) && startsWith(input$lang_initial_auto, "fr")) {
        new_lang <- "fr"
      } else {
        new_lang <- "en"
      }
      session$userData$lang_initial <- input$initial_lang_auto
    } else if (!exists("lang", session$userData)) {
      new_lang <- "fr"
    }

    if (!is.null(new_lang)) {
      session$userData$lang <- new_lang
      shiny.i18n::update_lang(session, new_lang)
      updateQueryString(paste0("?lang=", new_lang), mode = "replace")

      # window title doesn't quite work with i18n$t()
      session$sendCustomMessage(
        "changetitle",
        i18n$get_translations()["window_title", new_lang]
      )
    }
  })

  # Observe click of the "Francais" or "English" button and toggle the language
  observeEvent(input$lang_toggle, {
    new_lang <- if (session$userData$lang == "en") "fr" else "en"
    shiny.i18n::update_lang(session, new_lang)
    updateQueryString(paste0("?lang=", new_lang), mode = "replace")
    session$userData$lang <- new_lang

    # window title doesn't quite work with i18n$t()
    session$sendCustomMessage(
      "changetitle",
      i18n$get_translations()["window_title", new_lang]
    )
  })
}

shinyApp(ui, server)
