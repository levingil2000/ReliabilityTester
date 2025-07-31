analysisUI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "analysis",
    fluidRow(
      box(
        title = "Reliability Analysis Results", status = "primary", solidHeader = TRUE, width = 12,
        actionButton(ns("run_analysis"), "Run Cronbach's Alpha Analysis", class = "btn-danger"),
        br(), br(),
        uiOutput(ns("analysis_results"))
      )
    )
  )
}

analysisServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$run_analysis, {
      req(values$converted_data, values$objective_groups)

      results <- list()
      for (obj_name in names(values$objective_groups)) {
        obj_cols <- values$objective_groups[[obj_name]]
        obj_data <- values$converted_data[, obj_cols, drop = FALSE]
        obj_data <- obj_data[complete.cases(obj_data), , drop = FALSE]

        if (nrow(obj_data) > 0 && ncol(obj_data) > 1) {
          results[[obj_name]] <- optimize_cronbach(obj_data, obj_name)
        }
      }

      values$analysis_results <- results
      showNotification("Reliability analysis completed!", type = "message")
    })

    output$analysis_results <- renderUI({
      req(values$analysis_results)

      lapply(names(values$analysis_results), function(obj_name) {
        result <- values$analysis_results[[obj_name]]

        box(
          title = paste("Objective:", obj_name), status = "info", solidHeader = TRUE, width = 12,
          h4("Optimization Steps:"),
          renderDT({
            datatable(result$optimization_steps, 
                      options = list(pageLength = 10, searching = FALSE),
                      rownames = FALSE) %>%
              formatRound(columns = "Cronbach_Alpha", digits = 4)
          }),

          br(),
          h4("Summary:"),
          p(paste("Initial Cronbach's Alpha:", round(result$optimization_steps$Cronbach_Alpha[1], 4))),
          p(paste("Final Cronbach's Alpha:", round(result$final_alpha, 4))),
          p(paste("Final Items:", paste(result$final_items, collapse = ", "))),

          br(),
          h4("Interpretation:"),
          p(case_when(
            result$final_alpha >= 0.9 ~ "Excellent reliability (\u03b1 \u2265 0.9)",
            result$final_alpha >= 0.8 ~ "Good reliability (0.8 \u2264 \u03b1 < 0.9)",
            result$final_alpha >= 0.7 ~ "Acceptable reliability (0.7 \u2264 \u03b1 < 0.8)",
            result$final_alpha >= 0.6 ~ "Questionable reliability (0.6 \u2264 \u03b1 < 0.7)",
            TRUE ~ "Poor reliability (\u03b1 < 0.6)"
          ))
        )
      }) %>% tagList()
    })
  })
}
