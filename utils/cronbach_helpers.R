calculate_cronbach <- function(data) {
  if (ncol(data) < 2) return(NA)
  tryCatch({
    alpha_result <- psych::alpha(data, check.keys = TRUE)
    return(alpha_result$total$raw_alpha)
  }, error = function(e) {
    return(NA)
  })
}

optimize_cronbach <- function(data, objective_name) {
  if (ncol(data) < 2) {
    return(list(
      optimization_steps = data.frame(Step = "Initial", Items_Remaining = ncol(data),
                                      Cronbach_Alpha = NA, Removed_Item = "None"),
      final_alpha = NA,
      final_items = colnames(data)
    ))
  }

  steps <- data.frame(Step = "Initial", Items_Remaining = ncol(data),
                      Cronbach_Alpha = calculate_cronbach(data), Removed_Item = "None")

  current_data <- data
  step_count <- 1

  while (ncol(current_data) > 2) {
    best_alpha <- calculate_cronbach(current_data)
    best_removal <- NULL

    for (col in colnames(current_data)) {
      temp_data <- current_data[, !colnames(current_data) %in% col, drop = FALSE]
      temp_alpha <- calculate_cronbach(temp_data)
      if (!is.na(temp_alpha) && temp_alpha > best_alpha) {
        best_alpha <- temp_alpha
        best_removal <- col
      }
    }

    if (is.null(best_removal)) break

    current_data <- current_data[, !colnames(current_data) %in% best_removal, drop = FALSE]
    steps <- rbind(steps, data.frame(
      Step = paste("Step", step_count),
      Items_Remaining = ncol(current_data),
      Cronbach_Alpha = best_alpha,
      Removed_Item = best_removal
    ))

    step_count <- step_count + 1
  }

  return(list(
    optimization_steps = steps,
    final_alpha = calculate_cronbach(current_data),
    final_items = colnames(current_data)
  ))
}
