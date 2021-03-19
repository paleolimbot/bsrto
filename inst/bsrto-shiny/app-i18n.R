
library(shiny)
loadNamespace("shinyjs")
loadNamespace("shiny.i18n")

# This is namespaced as 'i18n' because allowing more than one of these
# makes it less useful. This module requires that there is a language
# 'key' in addition to at least one other language. You must define
# a 'window_title' translation but everything else is up to you.

# Translations are defined in translation.json. I'm using "key" as the
# key language, but you could omit this an use "en" or "fr" as the key
# language as well. I like the ability to abbreviate the key because there
# are some longer bits (like the text of an "about" page) where using the
# english version as a "key" is problematic.
i18n <- shiny.i18n::Translator$new(translation_json_path = "translation.json")
i18n_languages <- setdiff(i18n$get_languages(), "key")
i18n_default_language <- i18n_languages[1]

# must be before any translated elements (so, start of body)
i18nStartBody <- function() {
  tagList(
    shinyjs::useShinyjs(),
    shiny.i18n::usei18n(i18n)
  )
}

i18nUI <- function() {
  id <- "i18n"

  lang_button <- function(lang) {
    tags$button(
      stringr::str_to_title(lang),
      class = paste("btn btn-default", NS(id, "btn-lang")),
      id = NS(id, paste0("btn-lang-", lang))
    )
  }

  tagList(
    !!! lapply(i18n_languages, lang_button),
    verbatimTextOutput(NS(id, "lang_dummy"))
  )
}

i18nServer <- function() {
  id <- "i18n"

  moduleServer(id, function(input, output, session) {
    shinyjs::runjs("
      jQuery(function() {
        var usr_lang_initial_auto =  window.navigator.userLanguage || window.navigator.language;
        Shiny.setInputValue('i18n-lang_initial_auto', usr_lang_initial_auto);

        jQuery('.i18n-btn-lang').on('click', function() {
          var elId = this.id;
          var lang_id = elId.substring(elId.length - 2, elId.length);
          Shiny.setInputValue('i18n-lang', lang_id);
          jQuery(this).blur();
        });
      });

      Shiny.addCustomMessageHandler(
        'i18nChangeTitle',
        function(x) { document.title = x }
      );

      Shiny.addCustomMessageHandler(
        'i18nUpdateLang',
        function(x) {
          jQuery('.i18n-btn-lang').removeClass('i18n-btn-lang-current');
          jQuery('#i18n-btn-lang-' + x).addClass('i18n-btn-lang-current');
        }
      );
    ")

    # An empty output that is rendered initially and when 'lang_initial_auto'
    # is changed (on page load)
    output$lang_dummy <- renderText({
      query <- parseQueryString(session$clientData$url_search)
      new_lang <- NULL
      has_initial_lang <- exists("i18nlang_initial", session$userData)

      if (!has_initial_lang && !is.null(query$lang)) {
        new_lang <- query$lang
        session$userData$i18nlang_initial <- query$lang

      } else if (!has_initial_lang && !is.null(input$lang_initial_auto)) {
        # input value will be something like en-CA
        new_lang <- substr(input$lang_initial_auto, 1, 2)

        # if the user's language isn't in the translation key, use the first
        # non-key language
        if (!(new_lang %in% i18n_languages)) {
          new_lang <- i18n_default_language
        }

        session$userData$i18nlang_initial <- new_lang
      } else if (!exists("i18nlang", session$userData)) {
        new_lang <- i18n_default_language
      }

      if (!is.null(new_lang)) {
        updateQueryString(paste0("?lang=", new_lang), mode = "replace")
        session$userData$i18nlang <- new_lang

        session$sendInputMessage("state", list(lang = new_lang))
        session$sendCustomMessage("i18nUpdateLang", new_lang)
        session$sendCustomMessage(
          "i18nChangeTitle",
          i18n$get_translations()["window_title", new_lang]
        )
      }
    })

    # Observe language change from updated Shiny input
    observeEvent(input$lang, {
      new_lang <- input$lang
      updateQueryString(paste0("?lang=", new_lang), mode = "replace")
      session$userData$i18nlang <- new_lang

      session$sendInputMessage("state", list(lang = new_lang))
      session$sendCustomMessage("i18nUpdateLang", new_lang)
      session$sendCustomMessage(
        "i18nChangeTitle",
        i18n$get_translations()["window_title", new_lang]
      )
    })
  })
}
