
library(shiny)

aboutUI <- function(id = "about") {
  tagList(
    p(i18n$t("about_text_1")),
    p(i18n$t("about_text_2"))
  )
}

aboutServer <- function(lang, id = "about") {
  moduleServer(id, function(input, output, session) {
    # not currently anything here, but keep as a placeholder
  })
}
