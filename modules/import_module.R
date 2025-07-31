importUI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "import",
    fluidRow(
      box(
        title = "Import Google Sheets Data", status = "primary", solidHeader = TRUE, width = 12,
        textInput(ns("sheet_url"), "Google Sheets URL:", placeholder = "https://docs.google.com/..."),
        actionButton(ns("load_data"), "Load Data", class = "btn-primary"),
        br(), br(),
        DTOutput(ns("raw_data_preview"))
      )
    )
  )
}

importServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$load_data, {
      req(input$sheet_url)
      tryCatch({
        sheet_id <- gsub(".*spreadsheets/d/([a-zA-Z0-9_-]+).*", "\\1", input$sheet_url)
        csv_url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/export?format=csv")
        values$raw_data <- read_csv(csv_url, show_col_types = FALSE)
        showNotification("Data loaded successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error loading data:", e$message), type = "error")
      })
    })

    output$raw_data_preview <- renderDT({
      req(values$raw_data)
      datatable(values$raw_data, options = list(scrollX = TRUE, pageLength = 10))
    })
  })
}
