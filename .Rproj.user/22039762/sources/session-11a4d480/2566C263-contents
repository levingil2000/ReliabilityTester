likertUI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "likert",
    fluidRow(
      box(
        title = "Likert Scale Configuration", status = "primary", solidHeader = TRUE, width = 6,
        numericInput(ns("likert_levels"), "Number of Likert Scale Levels:", value = 5, min = 2, max = 10),
        br(),
        uiOutput(ns("likert_labels")),
        br(),
        actionButton(ns("apply_likert"), "Apply Likert Conversion", class = "btn-warning")
      ),
      box(
        title = "Converted Data Preview", status = "info", solidHeader = TRUE, width = 6,
        DTOutput(ns("converted_data_preview"))
      )
    )
  )
}

likertServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$likert_labels <- renderUI({
      req(input$likert_levels)
      lapply(1:input$likert_levels, function(i) {
        div(style = "display: inline-block; margin-right: 10px;",
            textInput(ns(paste0("label_", i)), paste("Label", i, ":"), value = paste("Level", i), width = "120px"),
            numericInput(ns(paste0("value_", i)), paste("Value", i, ":"), value = i, width = "80px")
        )
      })
    })

    observeEvent(input$apply_likert, {
      req(values$selected_data, input$likert_levels)
      label_mapping <- list()

      for (i in 1:input$likert_levels) {
        label <- input[[paste0("label_", i)]]
        value <- input[[paste0("value_", i)]]
        if (!is.null(label) && !is.null(value)) {
          label_mapping[[label]] <- value
        }
      }

      converted_data <- values$selected_data
      for (col in colnames(converted_data)) {
        if (is.character(converted_data[[col]]) || is.factor(converted_data[[col]])) {
          converted_data[[col]] <- as.character(converted_data[[col]])
          for (label in names(label_mapping)) {
            converted_data[[col]][converted_data[[col]] == label] <- as.character(label_mapping[[label]])
          }
          converted_data[[col]] <- as.numeric(converted_data[[col]])
        }
      }

      values$converted_data <- converted_data
      showNotification("Likert scale conversion applied successfully!", type = "message")
    })

    output$converted_data_preview <- renderDT({
      req(values$converted_data)
      datatable(values$converted_data, options = list(scrollX = TRUE, pageLength = 10))
    })
  })
}
