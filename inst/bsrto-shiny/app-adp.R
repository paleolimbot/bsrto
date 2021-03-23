
library(shiny)

adpUI <- function(id = "adp") {
  tagList(
    plotOutput(NS(id, "transducer_depth"))
  )
}

adpServer <- function(lang, data, id = "adp") {
  moduleServer(id, function(input, output, session) {

  })
}
