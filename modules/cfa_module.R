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
          title = paste("CFA Results:", obj_name), status = "info", solidHeader = TRUE, width = 12,
          
          # Initial Model Results
          h4("Initial Model (All Items):"),
          if (!is.null(result$initial_fit)) {
            div(
              p(paste("Items:", paste(result$initial_items, collapse = ", "))),
              create_fit_summary(result$initial_fit)
            )
          } else {
            p("Initial model could not be fitted", style = "color: red;")
          },
          
          br(),
          
          # Final Model Results
          h4("Final Model (Optimized Items):"),
          if (!is.null(result$final_fit)) {
            div(
              p(paste("Items:", paste(result$final_items, collapse = ", "))),
              create_fit_summary(result$final_fit)
            )
          } else {
            p("Final model could not be fitted (too few items)", style = "color: orange;")
          },
          
          br(),
          
          # Comparison
          if (!is.null(result$initial_fit) && !is.null(result$final_fit)) {
            div(
              h4("Model Comparison:"),
              create_model_comparison(result$initial_fit, result$final_fit)
            )
          }
        )
      })
      
      do.call(tagList, result_boxes)
    })
  })
}

# Helper function to create CFA model syntax
create_cfa_model <- function(items, factor_name) {
  # Clean factor name for lavaan syntax
  clean_name <- gsub("[^A-Za-z0-9_]", "_", factor_name)
  paste0(clean_name, " =~ ", paste(items, collapse = " + "))
}

# Helper function to safely run CFA
run_cfa_safely <- function(model, data) {
  tryCatch({
    fit <- cfa(model, data = data, estimator = "MLR")
    return(fit)
  }, error = function(e) {
    return(NULL)
  })
}

# Helper function to create fit summary
create_fit_summary <- function(fit) {
  if (is.null(fit)) return(p("Model could not be fitted"))
  
  fit_measures <- fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
  
  div(
    p(paste("χ² =", round(fit_measures["chisq"], 3), 
            ", df =", fit_measures["df"], 
            ", p =", round(fit_measures["pvalue"], 3))),
    p(paste("CFI =", round(fit_measures["cfi"], 3), 
            ", TLI =", round(fit_measures["tli"], 3))),
    p(paste("RMSEA =", round(fit_measures["rmsea"], 3), 
            ", SRMR =", round(fit_measures["srmr"], 3))),
    p(create_fit_interpretation(fit_measures), style = "font-weight: bold;")
  )
}

# Helper function to interpret fit
create_fit_interpretation <- function(fit_measures) {
  cfi <- fit_measures["cfi"]
  rmsea <- fit_measures["rmsea"]
  srmr <- fit_measures["srmr"]
  
  if (cfi >= 0.95 && rmsea <= 0.06 && srmr <= 0.08) {
    return("Excellent fit")
  } else if (cfi >= 0.90 && rmsea <= 0.08 && srmr <= 0.10) {
    return("Acceptable fit")
  } else {
    return("Poor fit")
  }
}

# Helper function to compare models
create_model_comparison <- function(initial_fit, final_fit) {
  if (is.null(initial_fit) || is.null(final_fit)) return(p("Cannot compare models"))
  
  initial_measures <- fitMeasures(initial_fit, c("cfi", "rmsea", "srmr"))
  final_measures <- fitMeasures(final_fit, c("cfi", "rmsea", "srmr"))
  
  div(
    p("Fit Improvement:"),
    p(paste("CFI:", round(initial_measures["cfi"], 3), "→", round(final_measures["cfi"], 3),
            "(", ifelse(final_measures["cfi"] > initial_measures["cfi"], "+", ""), 
            round(final_measures["cfi"] - initial_measures["cfi"], 3), ")")),
    p(paste("RMSEA:", round(initial_measures["rmsea"], 3), "→", round(final_measures["rmsea"], 3),
            "(", ifelse(final_measures["rmsea"] < initial_measures["rmsea"], "", "+"), 
            round(final_measures["rmsea"] - initial_measures["rmsea"], 3), ")")),
    p(paste("SRMR:", round(initial_measures["srmr"], 3), "→", round(final_measures["srmr"], 3),
            "(", ifelse(final_measures["srmr"] < initial_measures["srmr"], "", "+"), 
            round(final_measures["srmr"] - initial_measures["srmr"], 3), ")"))
  )
}