# CFA Module UI
cfaUI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "cfa",
          fluidRow(
            box(
              title = "Confirmatory Factor Analysis", status = "primary", solidHeader = TRUE, width = 12,
              helpText("CFA analysis will be performed on initial items and final optimized items for each objective."),
              br(),
              actionButton(ns("run_cfa"), "Run CFA Analysis", class = "btn-info"),
              br(), br(),
              uiOutput(ns("cfa_results_display"))
            )
          )
  )
}

# CFA Module Server
cfaServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Run CFA analysis
    observeEvent(input$run_cfa, {
      req(values$analysis_results, values$converted_data, values$objective_groups)
      
      tryCatch({
        cfa_results <- list()
        
        for (obj_name in names(values$analysis_results)) {
          obj_cols <- values$objective_groups[[obj_name]]
          obj_data <- values$converted_data[, obj_cols, drop = FALSE]
          obj_data <- obj_data[complete.cases(obj_data), , drop = FALSE]
          
          if (nrow(obj_data) > 0 && ncol(obj_data) > 2) {
            # Initial CFA (all items)
            initial_model <- create_cfa_model(obj_cols, obj_name)
            initial_fit <- run_cfa_safely(initial_model, obj_data)
            
            # Final CFA (optimized items)
            final_items <- values$analysis_results[[obj_name]]$final_items
            if (length(final_items) > 2) {
              final_model <- create_cfa_model(final_items, obj_name)
              final_data <- obj_data[, final_items, drop = FALSE]
              final_fit <- run_cfa_safely(final_model, final_data)
            } else {
              final_fit <- NULL
            }
            
            cfa_results[[obj_name]] <- list(
              initial_fit = initial_fit,
              final_fit = final_fit,
              initial_items = obj_cols,
              final_items = final_items
            )
          }
        }
        
        values$cfa_results <- cfa_results
        showNotification("CFA analysis completed!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("CFA Error:", e$message), type = "error")
      })
    })
    
    # Display CFA results
    output$cfa_results_display <- renderUI({
      req(values$cfa_results)
      
      result_boxes <- lapply(names(values$cfa_results), function(obj_name) {
        result <- values$cfa_results[[obj_name]]
        
        box(
          title = paste("CFA Results:", obj_name), 
          status = "info", 
          solidHeader = TRUE, 
          width = 12,
          collapsible = TRUE,
          
          # Initial Model Results
          div(
            style = "border-left: 4px solid #3c8dbc; padding-left: 15px; margin-bottom: 20px;",
            h4("Initial Model (All Items):", style = "color: #3c8dbc;"),
            p(paste("Items (", length(result$initial_items), "):", 
                    paste(result$initial_items, collapse = ", ")), 
              style = "font-style: italic; margin-bottom: 10px;"),
            if (!is.null(result$initial_fit)) {
              create_detailed_fit_summary(result$initial_fit)
            } else {
              div(
                p("Initial model could not be fitted", style = "color: red; font-weight: bold;"),
                p("This usually indicates multicollinearity or insufficient data.", style = "color: #666; font-size: 0.9em;")
              )
            }
          ),
          
          # Final Model Results
          div(
            style = "border-left: 4px solid #00a65a; padding-left: 15px; margin-bottom: 20px;",
            h4("Final Model (Optimized Items):", style = "color: #00a65a;"),
            p(paste("Items (", length(result$final_items), "):", 
                    paste(result$final_items, collapse = ", ")), 
              style = "font-style: italic; margin-bottom: 10px;"),
            if (!is.null(result$final_fit)) {
              create_detailed_fit_summary(result$final_fit)
            } else {
              div(
                p("Final model could not be fitted", style = "color: orange; font-weight: bold;"),
                p("This may be due to too few items remaining after optimization.", style = "color: #666; font-size: 0.9em;")
              )
            }
          ),
          
          # Comparison
          if (!is.null(result$initial_fit) && !is.null(result$final_fit)) {
            div(
              style = "border-left: 4px solid #f39c12; padding-left: 15px; background-color: #fefefe;",
              create_model_comparison(result$initial_fit, result$final_fit)
            )
          }
        )
      })
      
      do.call(tagList, result_boxes)
    })
  })
}