source("fn_reg_variable_names.R")

# Calibration functions: (1) calibration regressions; (2) smooth (binary) true_bin for calibration plot;
# (3) Traditional calibration bin scatter plot.


fn_calibration_regs <-
  # Regression and hypothesis tests for calibration tables.
  function(data,
           use_dummies = FALSE,
           cluster_by_event_only = FALSE) {
    # Determine which variables to include in the model.
    if (use_dummies) {
      # Include all prob_cut_dum* variables without an intercept.
      dummy_vars <- names(data)[grepl("prob_cut_dum_", names(data))]
      formula_str <-
        paste("true_bin ~", paste(dummy_vars, collapse = " + "), "- 1")
    } else {
      formula_str <- "true_bin ~ p"
    }
    lm_formula <- as.formula(formula_str)
    # Fit the linear model using lm (not lm_robust here, since we will calculate clustered robust SE separately).
    lm_model <- lm(lm_formula, data = data)
    
    # Calculate robust standard errors, clustered by both 'id' and 'event'.
    # In other words, calculates the variance-covariance matrix of the model coefficients, taking into account the
    # clustering of data by both id and event. This approach is used to account for potential correlations within clusters.
    if (cluster_by_event_only) {
      vcov_matrix <- cluster.vcov(lm_model, ~ event)
      robust_se <- sqrt(diag(vcov_matrix))
    } else {
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
    }
    
    # Extract coefficients
    coefficients <- coef(lm_model)
    
    if (!use_dummies) {
      # We see this in the calibration binned scatter plot.
      # Display the model summary with robust standard errors
      fn_reg_variable_names(lm_model, data)
      cat("Coefficients:\n")
      print(coefficients)
      cat("\nRobust Standard Errors:\n")
      print(robust_se)
    }
    
    if (use_dummies) {
      # Return results needed for traditional calibration bin scatter plot.
      return(list(beta = coefficients,
                  se = robust_se))
    } else {
      # Calculate Wald test statistic for prob = 1
      p_coef <- coefficients["p"]
      p_se <- robust_se["p"]
      # Calculates the Wald test statistic to test if the coefficient of p is equal to 1.
      # It uses the formula (Coefficient − Hypothesized Value) ^ 2 / Standard Error ^ 2
      wald_statistic_p <- (p_coef - 1) ^ 2 / p_se ^ 2
      
      # Similarly, this part tests if the intercept of the model is equal to 0.
      intercept_coef <- coefficients["(Intercept)"]
      intercept_se <- robust_se["(Intercept)"]
      wald_statistic_intercept <-
        (intercept_coef - 0) ^ 2 / intercept_se ^ 2
      
      # Calculate p-values for Chi-squared distribution with 1 degree of freedom.
      p_value_p <-
        pchisq(wald_statistic_p, df = 1, lower.tail = FALSE)
      p_value_intercept <-
        pchisq(wald_statistic_intercept,
               df = 1,
               lower.tail = FALSE)
      
      # Print Wald test results
      cat(
        "\nWald test for p = 1: Chi-squared statistic =",
        wald_statistic_p,
        ", p-value =",
        p_value_p,
        "\n"
      )
      cat(
        "Wald test for Intercept = 0: Chi-squared statistic =",
        wald_statistic_intercept,
        ", p-value =",
        p_value_intercept,
        "\n"
      )
      print(
        "________________________________________________________________________________________________"
      )
      
      # Return a list containing the model, Wald test p-values, and other relevant information.
      return(
        list(
          model = lm_model,
          p_value_p = p_value_p,
          p_value_intercept = p_value_intercept,
          coefficients = coefficients,
          robust_se = robust_se
        )
      )
    }
  }


# Helper function.
fn_generate_true_bin_var <- function(df) {
  # What bin captures the realized outcome?
  df %>%
    mutate(true_bin = ifelse(realization > bin_l &
                               realization <= bin_h, TRUE, FALSE))
}


fn_smooth <- function(df, p_var, smoothed_name, spar_value) {
  # Fit a curve through the data points represented by (prob_var, true_bin). The smooth.spline function in R creates a
  # spline that passes through or near these points in a way that minimizes the overall curvature of the line,
  # resulting in a smooth representation of the data.
  df %>%
    mutate(!!smoothed_name := {
      spline_fit <-
        smooth.spline(get(p_var), true_bin, spar = spar_value)  # Fit the spline.
      predict(spline_fit, x = get(p_var))$y  # Predict using the original probs.
    }) %>%
    select(!!sym(p_var), !!sym(smoothed_name)) %>%  # Keep only p_var and the smoothed true_bin.
    distinct()  # Remove duplicate rows.
}


fn_calibration_bin_scatter <-
  function(df,
           prob_var,
           title,
           cluster_by_event_only = FALSE) {
    # Traditional calibration bin scatter plot.
    epsilon <-
      1e-10  # To match STATA's cut() - However, it drops rows with p=1 (Question: Is this intentional?)
    df <- df %>%
      mutate(
        # Dividing the prob variable into bins based on the specified breakpoints.
        prob_cut = cut(
          !!sym(prob_var),
          breaks = c(
            -.00001,
            0.000001,
            .025 - epsilon,
            .075 - epsilon,
            .125  - epsilon,
            seq(.2, .5, by = .1) - epsilon,
            1
          ),
          right = TRUE,
          include.lowest = FALSE,
          labels = FALSE
        ),
        prob_cut_codes = as.integer(prob_cut) + 1
      )
    # print(table(df$prob_cut))
    # Creating dummy variables using model.matrix()
    dummy_vars <-
      # The - 1 in the formula means that we're not including the intercept.
      model.matrix(~ as.factor(prob_cut_codes) - 1, data = df)
    dummy_vars <- as.data.frame(dummy_vars)
    # Renaming the dummy variables.
    names(dummy_vars) <-
      paste0("prob_cut_dum_", seq_along(dummy_vars))
    # Binding the dummy variables to the original data frame.
    df <- cbind(df, dummy_vars)
    
    # Regression for error bars in the scatter plot.
    idvl_bin_scatter <-
      fn_calibration_regs(df,
                          use_dummies = TRUE,
                          cluster_by_event_only = cluster_by_event_only)
    idvl_coefs <- idvl_bin_scatter$beta
    idvl_robt_se <- idvl_bin_scatter$se
    
    df$prob_cut <- as.factor(df$prob_cut)
    
    # Calculate the statistics for calibration scatter plot by bin.
    df <- df %>%
      group_by(prob_cut) %>%
      mutate(
        p_mean = mean(!!sym(prob_var), na.rm = TRUE),
        p_est = idvl_coefs[paste0("prob_cut_dum_", prob_cut)],
        p_est_lower = idvl_coefs[paste0("prob_cut_dum_", prob_cut)] - 1.96 * idvl_robt_se[paste0("prob_cut_dum_", prob_cut)],
        p_est_upper = idvl_coefs[paste0("prob_cut_dum_", prob_cut)] + 1.96 * idvl_robt_se[paste0("prob_cut_dum_", prob_cut)]
      )
    
    # Append the point (0.75, 0.75) to extend the line -- just for cosmetic purposes.
    end_point <- 0.75
    line_data <-
      rbind(df, data.frame(p_mean = end_point, p_est = end_point))
    
    # Create the plot
    bin_scatter <- ggplot() +
      geom_line(
        data = line_data,
        aes(x = p_mean, y = p_mean),
        color = "gray",
        linetype = "dashed",
        linewidth = 0.75
      ) +
      geom_point(
        data = df,
        aes(x = p_mean, y = p_est),
        color = "red",
        size = 1.7
      ) +
      geom_errorbar(
        data = df,
        aes(x = p_mean, ymin = p_est_lower, ymax = p_est_upper),
        color = "gray50",
        width = 0.028,
        size = .41
      ) +
      labs(title = title, x = "Belief", y = "Empirical Probability") +
      scale_x_continuous("Belief",
                         labels = percent_format(),
                         breaks = seq(0, end_point, by = 0.25)) +
      scale_y_continuous(
        "Empirical Probability",
        labels = percent_format(),
        breaks = seq(0, end_point, by = 0.25)
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 12),
        # Center the title and adjust size if needed
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill = "white", colour = NA),
        # White background
        panel.border = element_blank(),
        # Remove border around the panel
        axis.line.x = element_line(color = "black"),
        # Keep bottom axis line
        axis.line.y = element_line(color = "black"),
        # Keep left axis line
        plot.background = element_rect(fill = "white", color = NA) # White background for the entire plot area
      )
    
    # Generate file name based on the title string
    file_name <-
      paste0("Graphs/calibrationBin", toupper(substr(title, 1, 3)), ".png")
    
    # Save the plot
    ggsave(
      file_name,
      bin_scatter,
      device = "png",
      width = 6,
      height = 4
    )
    return(bin_scatter)
  }
