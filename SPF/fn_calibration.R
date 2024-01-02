# Two functions: (1) calibration table; (2) smooth (binary) truth for calibration plot

# # Create a new variable for clustering
# data$cluster_id_event <- with(data, paste(id, event, sep = "_"))
# 
# model1 <- lm_robust(
#   truth ~ p,
#   se_type = "stata",
#   clusters = cluster_id_event,
#   data = data
# )
# 
# # View the summary of the model
# print(summary(model1))
# 
# # Perform hypothesis tests
# test_p <- linearHypothesis(model1, "p = 1", white.adjust = TRUE)
# test_cons <- linearHypothesis(model1, "(Intercept) = 0", white.adjust = TRUE)
# 
# # Print hypothesis test results
# print(test_p)
# print(test_cons)

fn_analyze_model <- function(data, cluster_by_event_only = FALSE) {
  # Fit the linear model using lm (not lm_robust here, since we will calculate clustered robust SE separately)
  lm_model <- lm(truth ~ p, data = data)
  
  # Calculate robust standard errors, clustered by both 'id' and 'event'
  # In other words, calculates the variance-covariance matrix of the model coefficients, taking into account the
  # clustering of data by both id and event. This approach is used to account for potential correlations within clusters.
  if (cluster_by_event_only) {
    # Cluster by event only
    robust_se <- sqrt(diag(cluster.vcov(lm_model, ~ event)))
  } else {
    robust_se <- sqrt(diag(cluster.vcov(lm_model, ~ id + event)))
    
    # Calculate the number of clusters for each clustering variable
    num_clusters_id <- length(unique(data$id))
    num_clusters_event <- length(unique(data$event))
    
    # Apply a subtle degrees of freedom adjustment
    # This adjustment tries to align with STATA's output
    df_correction_id <- sqrt((num_clusters_id - 1) / num_clusters_id)
    df_correction_event <- sqrt((num_clusters_event - 1) / num_clusters_event)
    combined_df_correction <- (df_correction_id + df_correction_event) / 2  # Averaging the adjustments
    
    robust_se <- robust_se * combined_df_correction
  }
  
  # Extract coefficients
  coefficients <- coef(lm_model)
  
  # Display the model summary with robust standard errors
  cat("Coefficients:\n")
  print(coefficients)
  cat("\nRobust Standard Errors:\n")
  print(robust_se)
  
  # Calculate Wald test statistic for p = 1
  p_coef <- coefficients["p"]
  p_se <- robust_se["p"]
  # Calculates the Wald test statistic to test if the coefficient of p is equal to 1.
  # It uses the formula (Coefficient − Hypothesized Value) ^ 2 / Standard Error ^ 2
  wald_statistic_p <- (p_coef - 1)^2 / p_se^2
  
  # Similarly, this part tests if the intercept of the model is equal to 0.
  intercept_coef <- coefficients["(Intercept)"]
  intercept_se <- robust_se["(Intercept)"]
  wald_statistic_intercept <- intercept_coef^2 / intercept_se^2
  
  # Calculate p-values for Chi-squared distribution with 1 degree of freedom
  p_value_p <- pchisq(wald_statistic_p, df = 1, lower.tail = FALSE)
  p_value_intercept <- pchisq(wald_statistic_intercept, df = 1, lower.tail = FALSE)
  
  # Print Wald test results
  cat("\nWald test for p = 1: Chi-squared statistic =", wald_statistic_p, ", p-value =", p_value_p, "\n")
  cat("Wald test for Intercept = 0: Chi-squared statistic =", wald_statistic_intercept, ", p-value =", p_value_intercept, "\n")
  
  # Return a list containing the model, Wald test p-values, and other relevant information
  return(list(
    model = lm_model,
    p_value_p = p_value_p,
    p_value_intercept = p_value_intercept,
    df = nrow(data),
    coefficients = coefficients,
    robust_se = robust_se
  ))
}


fn_smooth <- function(df, p_var, smoothed_name, spar_value) {
  # Fit a curve through the data points represented by (p_var, truth). The smooth.spline function in R creates a 
  # spline that passes through or near these points in a way that minimizes the overall curvature of the line, 
  # resulting in a smooth representation of the data.
  df %>%
    mutate(!!smoothed_name := {
      spline_fit <- smooth.spline(get(p_var), truth, spar = spar_value)  # Fit the spline
      predict(spline_fit, x = get(p_var))$y  # Predict using the original probs
    }) %>%
    select(!!sym(p_var), !!sym(smoothed_name)) %>%  # Keep only p_var and the smoothed truth
    distinct()  # Remove duplicate rows
}