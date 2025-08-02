# CFA Helper Functions

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

# Helper function to extract factor loadings
get_factor_loadings <- function(fit) {
  if (is.null(fit)) return(NULL)
  
  tryCatch({
    loadings <- parameterEstimates(fit)
    loadings <- loadings[loadings$op == "=~", c("rhs", "est", "se", "pvalue")]
    names(loadings) <- c("Item", "Loading", "SE", "P_Value")
    loadings$Loading <- round(loadings$Loading, 3)
    loadings$SE <- round(loadings$SE, 3)
    loadings$P_Value <- round(loadings$P_Value, 3)
    return(loadings)
  }, error = function(e) {
    return(NULL)
  })
}

# Helper function to create individual fit interpretations
interpret_fit_indices <- function(fit_measures) {
  interpretations <- list()
  
  # Chi-square
  chisq_p <- fit_measures["pvalue"]
  if (chisq_p > 0.05) {
    interpretations$chisq <- "Good (non-significant, p > .05)"
  } else {
    interpretations$chisq <- "Poor (significant, p < .05)"
  }
  
  # CFI
  cfi <- fit_measures["cfi"]
  if (cfi >= 0.95) {
    interpretations$cfi <- "Excellent (≥ .95)"
  } else if (cfi >= 0.90) {
    interpretations$cfi <- "Acceptable (.90 - .94)"
  } else {
    interpretations$cfi <- "Poor (< .90)"
  }
  
  # TLI
  tli <- fit_measures["tli"]
  if (tli >= 0.95) {
    interpretations$tli <- "Excellent (≥ .95)"
  } else if (tli >= 0.90) {
    interpretations$tli <- "Acceptable (.90 - .94)"
  } else {
    interpretations$tli <- "Poor (< .90)"
  }
  
  # RMSEA
  rmsea <- fit_measures["rmsea"]
  if (rmsea <= 0.06) {
    interpretations$rmsea <- "Excellent (≤ .06)"
  } else if (rmsea <= 0.08) {
    interpretations$rmsea <- "Acceptable (.06 - .08)"
  } else if (rmsea <= 0.10) {
    interpretations$rmsea <- "Mediocre (.08 - .10)"
  } else {
    interpretations$rmsea <- "Poor (> .10)"
  }
  
  # SRMR
  srmr <- fit_measures["srmr"]
  if (srmr <= 0.08) {
    interpretations$srmr <- "Good (≤ .08)"
  } else if (srmr <= 0.10) {
    interpretations$srmr <- "Acceptable (.08 - .10)"
  } else {
    interpretations$srmr <- "Poor (> .10)"
  }
  
  return(interpretations)
}

# Helper function to create detailed fit summary with individual interpretations
create_detailed_fit_summary <- function(fit) {
  if (is.null(fit)) return(div(p("Model could not be fitted", style = "color: red;")))
  
  fit_measures <- fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
  interpretations <- interpret_fit_indices(fit_measures)
  
  # Get factor loadings
  loadings <- get_factor_loadings(fit)
  
  div(
    # Fit indices with individual interpretations
    h5("Fit Indices:", style = "margin-top: 10px;"),
    div(
      style = "margin-left: 15px;",
      p(paste("χ² =", round(fit_measures["chisq"], 3), 
              "(df =", fit_measures["df"], 
              ", p =", round(fit_measures["pvalue"], 3), ") -", 
              interpretations$chisq)),
      p(paste("CFI =", round(fit_measures["cfi"], 3), "-", interpretations$cfi)),
      p(paste("TLI =", round(fit_measures["tli"], 3), "-", interpretations$tli)),
      p(paste("RMSEA =", round(fit_measures["rmsea"], 3), "-", interpretations$rmsea)),
      p(paste("SRMR =", round(fit_measures["srmr"], 3), "-", interpretations$srmr))
    ),
    
    # Factor loadings
    if (!is.null(loadings)) {
      div(
        h5("Factor Loadings:", style = "margin-top: 15px;"),
        div(
          style = "margin-left: 15px;",
          DT::renderDataTable({
            DT::datatable(loadings, 
                          options = list(pageLength = 10, searching = FALSE, dom = 't'),
                          rownames = FALSE) %>%
              DT::formatStyle(columns = "Loading", 
                              backgroundColor = DT::styleInterval(c(0.3, 0.5, 0.7), 
                                                                  c("#ffcccc", "#ffffcc", "#ccffcc", "#ccffff")))
          }, outputArgs = list())
        )
      )
    } else {
      div(
        h5("Factor Loadings:", style = "margin-top: 15px;"),
        p("Could not extract factor loadings", style = "color: orange; margin-left: 15px;")
      )
    }
  )
}

# Helper function to create overall model interpretation
create_overall_fit_interpretation <- function(fit_measures) {
  cfi <- fit_measures["cfi"]
  rmsea <- fit_measures["rmsea"]
  srmr <- fit_measures["srmr"]
  
  if (cfi >= 0.95 && rmsea <= 0.06 && srmr <= 0.08) {
    return("Overall: Excellent fit")
  } else if (cfi >= 0.90 && rmsea <= 0.08 && srmr <= 0.10) {
    return("Overall: Acceptable fit")
  } else {
    return("Overall: Poor fit")
  }
}

# Helper function to compare models
create_model_comparison <- function(initial_fit, final_fit) {
  if (is.null(initial_fit) || is.null(final_fit)) return(p("Cannot compare models"))
  
  initial_measures <- fitMeasures(initial_fit, c("cfi", "rmsea", "srmr"))
  final_measures <- fitMeasures(final_fit, c("cfi", "rmsea", "srmr"))
  
  div(
    h5("Model Comparison:"),
    div(
      style = "margin-left: 15px;",
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
  )
}