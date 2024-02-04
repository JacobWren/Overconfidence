# Regression and hypothesis tests for variance/error on disagreement for SPF and
# partitioned variance regression for experiment.

# Check if in "SPF"
if (grepl("SPF$", getwd())) {
  # If it ends with "SPF", source the file from one directory up
  source("../Helpers/fn_reg_variable_names.R")
} else {
  # Otherwise, source the file from two directories up
  source("../../Helpers/fn_reg_variable_names.R")
}


fn_regress_and_test <-
  function(data,
           normed = FALSE,
           cluster = TRUE,
           include_intercept = TRUE,
           simple_reg = FALSE,
           case = "SPF",
           wrong_mean = FALSE,
           error_as_dv = TRUE,
           disagreement = FALSE) {
    # Choose the right formula based on the flags.
    if (error_as_dv) {
      # Error term as the dependent variable.
      if (normed) {
        formula_str <-
          ifelse(
            include_intercept,
            "errorNorm ~ disagreement_EV_minus_forecaster_i",
            "errorNorm ~ disagreement_EV_minus_forecaster_i - 1"
          )
      } else {
        if (case == "SPF") {
          formula_str <-
            ifelse(
              include_intercept,
              "error ~ disagreement_EV_minus_forecaster_i",
              "error ~ disagreement_EV_minus_forecaster_i - 1"
            )
        } else {
          if (disagreement) {
            formula_str <-
              ifelse(
                wrong_mean,
                "error_in_mse ~ avg_var_of_mean_less_ith_id",
                "error_in_var ~ avg_var_of_mean_less_ith_id"
              )
          } else {
            formula_str <-
              ifelse(
                wrong_mean,
                "error_in_mse ~ var_true_model + error_in_est",
                "error_in_var ~ var_true_model"
              )
          }
        }
      }
    } else {
      # Variance on LHS.
      if (case == "Experiment") {
        formula_str <-
          ifelse(
            wrong_mean,
            "var ~ var_true_within + var_true_model + var_true_added_from_wrong",
            "var ~ var_true_within + var_true_model"
          )
      }
    }
    
    # Convert the string to a formula
    lm_formula <- as.formula(formula_str)
    
    # Fit the linear model using lm (not lm_robust here, since we will calculate clustered robust SE separately).
    lm_model <- lm(lm_formula, data = data)
    
    # If simple_reg is TRUE, return just the model.
    if (simple_reg) {
      fn_reg_variable_names(lm_model, data)
      print("Averaged over dataSet/time_to_resolution")
      print(summary(lm_model))
      print(
        "________________________________________________________________________________________________"
      )
      return(lm_model)
    }
    
    # Calculate robust standard errors, clustered by both 'id' and 'event' or no clustering.
    # In other words, if clustering, calculates the variance-covariance matrix of the model coefficients, taking into account the
    # clustering of data by both id and event. This approach is used to account for potential correlations within clusters.
    if (cluster) {
      vcov_matrix <- cluster.vcov(lm_model, ~ id + event)
      
      # Calculate the number of clusters for each clustering variable.
      num_clusters_id <- length(unique(data$id))
      num_clusters_event <- length(unique(data$event))
      
      # Apply a subtle degrees of freedom adjustment.
      # This adjustment tries to align with STATA's output.
      df_correction_id <-
        sqrt((num_clusters_id - 1) / num_clusters_id)
      df_correction_event <-
        sqrt((num_clusters_event - 1) / num_clusters_event)
      df_correction <-
        (df_correction_id + df_correction_event) / 2  # Averaging the adjustments
      
      vcov_matrix_adjusted <- vcov_matrix * df_correction
      robust_se <- sqrt(diag(vcov_matrix_adjusted))
    } else {
      # No cluster.
      robust_se <- sqrt(diag(vcovHC(lm_model, type = "HC1")))
    }
    
    # Extract coefficients
    coefficients <- coef(lm_model)
    
    # Display the model summary with robust standard errors
    fn_reg_variable_names(lm_model, data)
    cat("Coefficients:\n")
    print(coefficients)
    cat("\nRobust Standard Errors:\n")
    print(robust_se)
    
    if (case == "Experiment") {
      return(list(
        model = lm_model,
        coefficients = coefficients,
        robust_se = robust_se
      ))
    }
    
    # Calculate Wald test statistic for disagreement_EV_minus_forecaster_i = 0
    disag_coef <- coefficients["disagreement_EV_minus_forecaster_i"]
    disag_se <- robust_se["disagreement_EV_minus_forecaster_i"]
    # Calculates the Wald test statistic to test if the coefficient of disagreement_EV_minus_forecaster_i is equal to 0.
    # It uses the formula (Coefficient − Hypothesized Value) ^ 2 / Standard Error ^ 2
    wald_statistic_disag <- (disag_coef - 0) ^ 2 / disag_se ^ 2
    
    # Calculate p-values for Chi-squared distribution with 1 degree of freedom.
    p_value <-
      pchisq(wald_statistic_disag, df = 1, lower.tail = FALSE)
    
    # Print Wald test results
    cat(
      "\nWald test for disagreement_EV_minus_forecaster_i = 0: Chi-squared statistic =", wald_statistic_disag,
      ", p-value =", p_value, "\n"
    )
    print(
      "________________________________________________________________________________________________"
    )
    
    # Return a list containing the model, Wald test p-value, and other relevant information.
    return(
      list(
        model = lm_model,
        p_value = p_value,
        coefficients = coefficients,
        robust_se = robust_se
      )
    )
  }
