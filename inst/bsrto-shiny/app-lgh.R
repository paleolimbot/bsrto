
library(shiny)

lghUI <- function(id = "lgh") {
  tagList(
    DT::DTOutput(NS(id, "log_list")),
    div(style = "height: 10px;"),
    verbatimTextOutput(NS(id, "log_text"))
  )
}

lghServer <- function(lang, data, id = "lgh") {
  moduleServer(id, function(input, output, session) {

    lgh_sorted <- reactive({
      data$lgh() %>%
        arrange(desc(date_time))
    })

    output$log_list <- DT::renderDT({

        lgh_sorted() %>%
          transmute(
            # Not using a locale-specific format here, but in theory
            # any strftime calls should be wrapped in locale code
            date_time = suppressWarnings(
              withr::with_locale(
                c(LC_TIME = paste0(lang(), "_CA")),
                strftime(date_time, "%Y-%m-%d %H:%M", tz = "UTC")
              )
            ),
            file,
            log_text = substr(log_text, 1, 50)
          )
      },

      # https://datatables.net/reference/option/
      options = list(
        searching = FALSE,
        lengthChange = FALSE,
        ordering = FALSE
      ),

      # ?DT::datatable
      selection = "single",
      rownames = FALSE,
      colnames = i18n_t(
        c("Date", "File name", "File content"),
        lang()
      )
    )

    output$log_text <- renderText({
      rows <- input$log_list_rows_selected
      if (!is.null(rows)) {
        lgh_sorted() %>%
          slice(rows) %>%
          pull(log_text)
      } else {
        i18n_t("<No items selected>", lang())
      }
    })

  })
}
