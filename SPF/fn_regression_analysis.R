source("../Regressions/fn_regress.R")

# Variance/Error Regressions at the individual and aggregate levels.


fn_regression_analysis <-
  function(df_indvl,
           df_agg,
           df_collapsed_bin_all_vars_indvl) {
    # Individual level:
    # df_indvl => every forecaster and every bin
    # df_collapsed_bin_all_vars_indvl => compressed over bins + has statistics like sq error, disagreement
    
    # Agg level:
    # df_agg => every bin (agg beliefs have been compressed over forecasters)
    
    # Error On Disagreement Regressions.
    var_regs_int <- fn_regress(df_collapsed_bin_all_vars_indvl)
    var_regs_no_int <-
      fn_regress(df_collapsed_bin_all_vars_indvl, include_intercept = FALSE)
    
    names(df_collapsed_bin_all_vars_indvl)[names(df_collapsed_bin_all_vars_indvl) == "dataSet"] <-
      "forecast_var"
    # Combining groups ==> that is, taking averages over forecasted variables/timeToResolution.
    df_collapsed_bin_all_vars_indvl <-
      df_collapsed_bin_all_vars_indvl %>%
      # Modify 'distance_from_resolution'.
      mutate(
        distance_from_resolution_trucate = if_else(
          distance_from_resolution > 2,
          1.875,
          distance_from_resolution
        )
      ) %>%
      group_by(forecast_var, distance_from_resolution_trucate) %>%
      mutate(grpd_forecast_var_distance = cur_group_id()) %>%
      ungroup()
    
    df_collapsed <- df_collapsed_bin_all_vars_indvl %>%
      group_by(grpd_forecast_var_distance) %>%
      # Take averages.
      summarise(
        error = mean(error, na.rm = TRUE),
        sq_dev_from_realization = mean(sq_dev_from_realization, na.rm = TRUE),
        var_predicted = mean(var_predicted, na.rm = TRUE),
        disagreement_EV_minus_forecaster_i = mean(disagreement_EV_minus_forecaster_i, na.rm = TRUE),
        forecast_var = first(forecast_var)
      )
    
    # Regressions now at the aggregate level.
    var_regs_int_avg <-
      fn_regress(df_collapsed, simple_reg = TRUE)
    var_regs_no_int_avg <-
      fn_regress(df_collapsed,
                 simple_reg = TRUE,
                 include_intercept = FALSE)
    
    # Completely compressed (i.e., no regression).
    
    # Collapse the Data to Overall Means
    df_fully_collapsed <- df_collapsed_bin_all_vars_indvl %>%
      summarise(
        error = mean(error, na.rm = TRUE),
        disagreement_EV_minus_forecaster_i = mean(disagreement_EV_minus_forecaster_i, na.rm = TRUE)
      )
    
    # Duplicate the single row of means, otherwise won't run the "regession".
    df_fully_collapsed <- df_fully_collapsed[rep(1, 2), ]
    var_regs_no_int_avg_only <-
      fn_regress(df_fully_collapsed,
                 simple_reg = TRUE,
                 include_intercept = FALSE)
    
    # Creating the disagreement regression table.
    # Models list
    mods <- list(
      var_regs_int$model,
      var_regs_no_int$model,
      var_regs_int_avg,
      var_regs_no_int_avg,
      var_regs_no_int_avg_only
    )
    
    se = list(
      var_regs_int$robust_se,
      var_regs_no_int$robust_se,
      summary(var_regs_int_avg)$coefficients[, "Std. Error"],
      summary(var_regs_no_int_avg)$coefficients[, "Std. Error"],
      summary(var_regs_no_int_avg_only)$coefficients[, "Std. Error"]
    )
    
    # Extract P-Values and Format
    p_values <- c(
      var_regs_int$p_value,
      var_regs_no_int$p_value,
      summary(var_regs_int_avg)$coefficients["disagreement_EV_minus_forecaster_i", "Pr(>|t|)"],
      summary(var_regs_no_int_avg)$coefficients["disagreement_EV_minus_forecaster_i", "Pr(>|t|)"],
      NA  # For silly last case where we have rolled-up to a scalar.
    )
    
    p_values <-
      unlist(lapply(p_values, function(p)
        if (is.na(p))
          ""
        else
          sprintf("%.4f", p))) # Format.
    
    add_lines <- list(c("P(Beta=0)", p_values))
    
    disagreement_table <- stargazer(
      mods,
      type = "latex",
      header = FALSE,
      se = se,
      add.lines = add_lines,
      covariate.labels = c("Disagreement", "Constant"),
      omit.stat = c("f", "ser", "adj.rsq"),
      column.labels = c("Individual", "est2", "est3", "est4", "est5"),
      model.numbers = FALSE,
      star.cutoffs = NA,
      dep.var.labels.include = FALSE,
      dep.var.caption = "",
      digits = 4
    )
    cat(disagreement_table, file = "Results/disagreement.tex")
  }
