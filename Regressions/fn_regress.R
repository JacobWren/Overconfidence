source("../Helpers/fn_help.R")
source("../Regressions/fn_wald_test.R")

# Regression for variance/error on disagreement for SPF,
# partitioned variance for experiment, and for calibration.


fn_regress <-
  function(data,
           normed = FALSE,
           cluster = "double",
           include_intercept = TRUE,
           simple_reg = FALSE,
           case = "SPF",
           wrong_mean = FALSE,
           error_as_dv = TRUE,
           disagreement = FALSE,
           calibration = FALSE,
           use_dummies = FALSE) {
    if (calibration) {
      if (case == "SPF") {
        dep_var <- "p_empirical"
        ind_var <- "p"
        # if (cluster == "single") { # => in the aggregate
        #   dep_var <- "p_agg_empirical"
        #   ind_var <- "p_agg"
        # }
      } else {
        names(data)[names(data) == "pic"] <- "event"
        # Initialize variables
        dep_var <- "p_true"  
        if (cluster == "single") { # => in the aggregate
          dep_var <- "p_agg_true"
          ind_var <- "p_agg"
        }
      }
      
      if (use_dummies) {
        # Include all prob_cut_dum* variables without an intercept.
        dummy_vars <- names(data)[grepl("prob_cut_dum_", names(data))]
        formula_str <- paste(dep_var, "~", paste(dummy_vars, collapse = " + "), "- 1")
      } else {
        formula_str <- paste(dep_var, "~", ind_var)
      }
    }
    else {
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
      }
    # Convert the string to a formula
    lm_formula <- as.formula(formula_str)
    
    # Fit the linear model using lm (not lm_robust here, since we will calculate clustered robust SE separately).
    lm_model <- lm(lm_formula, data = data)
    
    # Return just the model.
    if (simple_reg) {
      fn_help(lm_model, data)
      print("Averaged over dataSet/time_to_resolution")
      print(summary(lm_model))
      print(
        "________________________________________________________________________________________________"
      )
      return(lm_model)
    }
    
    # Calculate robust standard errors, clustered by both 'id' and 'event', just 'event', or no clustering.
    # In other words, if clustering, calculates the variance-covariance matrix of the model coefficients, taking into account the
    # clustering of data by both id and event. This approach is used to account for potential correlations within clusters.
    if (cluster == "double") {
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
    } else if (cluster == "single") {
      vcov_matrix <- cluster.vcov(lm_model, ~ event)
      robust_se <- sqrt(diag(vcov_matrix))
    } else {
      # No cluster.
      robust_se <- sqrt(diag(vcovHC(lm_model, type = "HC1")))
    }
    
    # Extract coefficients
    coefficients <- coef(lm_model)
    
    # Display the model summary with robust standard errors
    if (!use_dummies) { 
      # We already see this in the calibration binned scatter plot.
      fn_help(lm_model, data)
      cat("Coefficients:\n")
      print(coefficients)
      cat("\nRobust Standard Errors:\n")
      print(robust_se)
    }
      
    if (use_dummies) {
      # Return results needed for traditional calibration bin scatter plot.
      return(list(beta = coefficients,
                  se = robust_se))
      
    } else if (case == "Experiment") { 
      return(list(
        model = lm_model,
        coefficients = coefficients,
        robust_se = robust_se
      ))
      
    } else { # SPF: calibration?
      # Initialize.
      result <- list(
        model = lm_model,
        coefficients = coefficients,
        robust_se = robust_se
      )
        if (calibration) {
          result$p_value_p <- fn_wald_test(coefficients, robust_se, "p", hyp_value = 1)
          result$p_value_intercept <- fn_wald_test(coefficients, robust_se, "(Intercept)")
        }
        else {
          result$p_value <- fn_wald_test(coefficients, robust_se, "disagreement_EV_minus_forecaster_i")
        }
    }
    # Return a list containing the model, Wald test p-value, and other relevant information.
    return(result)
}
