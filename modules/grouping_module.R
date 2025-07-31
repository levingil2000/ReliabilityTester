groupingUI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "grouping",
    fluidRow(
      box(
        title = "Group Columns by Objectives", status = "primary", solidHeader = TRUE, width = 12,
        helpText("Assign each selected column to an objective group for separate reliability analysis:"),
        br(),
        uiOutput(ns("objective_assignment")),
        br(),
        actionButton(ns("confirm_objectives"), "Confirm Grouping", class = "btn-success"),
        br(), br(),
        verbatimTextOutput(ns("objective_summary"))
      )
    )
  )
}

groupingServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$objective_assignment <- renderUI({
      req(values$selected_data)
      cols <- colnames(values$selected_data)
      lapply(cols, function(col) {
        textInput(ns(paste0("obj_", col)), paste("Objective for", col, ":"),
                  placeholder = "e.g., Satisfaction, Quality, etc.")
      })
    })

    observeEvent(input$confirm_objectives, {
      req(values$selected_data)
      cols <- colnames(values$selected_data)
      objective_list <- list()

      for (col in cols) {
        obj_name <- input[[paste0("obj_", col)]]
        if (!is.null(obj_name) && obj_name != "") {
          if (is.null(objective_list[[obj_name]])) {
            objective_list[[obj_name]] <- c()
          }
          objective_list[[obj_name]] <- c(objective_list[[obj_name]], col)
        }
      }

      values$objective_groups <- objective_list
      showNotification("Objectives grouped successfully!", type = "message")
    })

    output$objective_summary <- renderText({
      req(values$objective_groups)
      summary_text <- "Objective Groups:\n"
      for (obj in names(values$objective_groups)) {
        summary_text <- paste0(summary_text, obj, ": ", paste(values$objective_groups[[obj]], collapse = ", "), "\n")
      }
      summary_text
    })
  })
}
