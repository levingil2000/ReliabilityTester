selectionUI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "selection",
    fluidRow(
      box(
        title = "Select Columns for Analysis", status = "primary", solidHeader = TRUE, width = 12,
        helpText("Select the columns you want to include in the reliability analysis (exclude demographics):"),
        br(),
        uiOutput(ns("column_selector")),
        br(),
        actionButton(ns("confirm_columns"), "Confirm Selection", class = "btn-success"),
        br(), br(),
        DTOutput(ns("selected_data_preview"))
      )
    )
  )
}

selectionServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$column_selector <- renderUI({
      req(values$raw_data)
      checkboxGroupInput(ns("selected_columns"), "Select Columns:",
                         choices = colnames(values$raw_data),
                         selected = colnames(values$raw_data))
    })

    observeEvent(input$confirm_columns, {
      req(input$selected_columns, values$raw_data)
      values$selected_data <- values$raw_data[, input$selected_columns, drop = FALSE]
      showNotification("Columns selected successfully!", type = "message")
    })

    output$selected_data_preview <- renderDT({
      req(values$selected_data)
      datatable(values$selected_data, options = list(scrollX = TRUE, pageLength = 10))
    })
  })
}
