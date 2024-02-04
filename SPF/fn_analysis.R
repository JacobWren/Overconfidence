source("../Calibration/fn_calibration.R")
source("../Regressions/fn_regress_and_test.R")


fn_analysis <-
  function(df_indvl,
           df_agg,
           df_collapsed_bin_all_vars_indvl) {
    # Individual level:
    # df_indvl => every forecaster and every bin
    # df_collapsed_bin_all_vars_indvl => compressed over bins + has statistics like sq error, disagreement
    
    # Agg level:
    # df_agg => every bin (agg beliefs have been compressed over forecasters)
    
    df_indvl <- fn_generate_true_bin(df_indvl)
    
    # INDIVIDUAL
    idvl_calibration_regs <- fn_calibration_regs(df_indvl)
    
    # AGGREGATE
    df_agg <- fn_generate_true_bin(df_agg)
    
    names(df_agg)[names(df_agg) == "p_agg"] <- "p" # For table
    agg_calibration_regs <-
      fn_calibration_regs(df_agg, cluster_by_event_only = TRUE)
    
    # Create the Calibration Table
    stargazer_table_calib <-
      stargazer(
        idvl_calibration_regs$model,
        agg_calibration_regs$model,
        type = "latex",
        header = FALSE,
        se = list(
          idvl_calibration_regs$robust_se,
          agg_calibration_regs$robust_se
        ),
        add.lines = list(
          c(
            "P(Beta=1)",
            sprintf("%.4f", idvl_calibration_regs$p_value_p),
            sprintf("%.4f", agg_calibration_regs$p_value_p)
          ),
          c(
            "P(Cons=0)",
            sprintf("%.4f", idvl_calibration_regs$p_value_intercept),
            sprintf("%.4f", agg_calibration_regs$p_value_intercept)
          )
        ),
        covariate.labels = c("Prob", "Constant"),
        omit.stat = c("f", "ser", "adj.rsq"),
        # Drop
        column.labels = c("Individual", "Average"),
        model.numbers = FALSE,
        star.cutoffs = NA,
        # Disable significance stars
        dep.var.labels.include = FALSE,
        # Remove "Dependent variable" label
        dep.var.caption = ""  # Remove the dependent variable caption
      )
    
    # Write the table to a .tex file
    cat(stargazer_table_calib, file = "Results/calibration.tex")
    
    # Calibration: Plots
    names(df_agg)[names(df_agg) == "p"] <- "p_agg" # Command z.
    # Smoothing operation on the "trueBin" variable against prob.
    # Individual level
    smoothing_param <-
      1.475 # Used for smoothing scatterplot via a spline.
    data_idvl_smoothed <-
      fn_smooth(df_indvl, "p", "true_bin_smoothed", smoothing_param)
    # Aggregate level
    data_agg_smoothed <-
      fn_smooth(df_agg, "p_agg", "true_bin_smoothed_agg", smoothing_param)
    
    # Append the dfs.
    combined_smoothed <-
      bind_rows(data_idvl_smoothed, data_agg_smoothed)
    
    # Calibration plot
    calibration_smoothed_fig <- ggplot() +
      geom_line(
        data = combined_smoothed,
        aes(x = p, y = p, color = "Bayesian"),
        size = 0.90,
        linetype = "dashed"
      ) +
      geom_line(
        data = combined_smoothed,
        aes(x = p, y = true_bin_smoothed, color = "Individual"),
        size = 0.85,
        alpha = .85
      ) +
      geom_line(
        data = combined_smoothed,
        aes(x = p_agg, y = true_bin_smoothed_agg, color = "Average"),
        size = 0.85,
        alpha = .90
      ) +
      scale_x_continuous("Belief",
                         labels = percent_format(),
                         breaks = seq(0, 1, by = 0.25)) +
      scale_y_continuous("Empirical Probability",
                         labels = percent_format(),
                         breaks = seq(0, 1, by = 0.25)) +
      scale_color_manual(
        values = c(
          "Bayesian" = "darkgrey",
          "Individual" = "navy",
          "Average" = "red"
        ),
        name = "Legend"
      ) +
      scale_linetype_manual(values = c(
        "Bayesian" = "5",
        "Individual" = "solid",
        "Average" = "solid"
      )) + # Adjust linetype pattern
      theme_minimal() +
      theme(
        legend.position = c(0.18, 0.77),
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        legend.key = element_blank(),
        # Remove boxes around legend colors
        legend.key.size = unit(.01, "lines"),
        legend.spacing = unit(0.001, "cm"),
        legend.margin = margin(.001, 3, .001, .001),
        legend.box.margin = margin(.01, .3, .01, .3),
        # Reduce legend box margin
        legend.spacing.y = unit(.01, "lines"),
        # Reduce vertical spacing between legend items
        legend.background = element_rect(fill = "white", color = "black"),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        # Set plot background to white
        plot.margin = unit(c(1, 1, 1, 1), "lines")
      ) + # Adjust plot margins
      guides(color = guide_legend(override.aes = list(
        size = 11,
        linetype = c("solid", "dashed", "solid")
      )))
    
    # Save the plot to a PDF file
    ggsave(
      "Graphs/calibration.png",
      plot = calibration_smoothed_fig,
      device = "png",
      width = 6.75,
      height = 4.75
    )
    
    # Traditional calibration bin scatter plots.
    # Individual level
    idvl_bin_scatter <-
      fn_calibration_bin_scatter(df_indvl, "p", "Individual")
    # Aggregate level
    agg_bin_scatter <-
      fn_calibration_bin_scatter(df_agg, "p_agg", "Aggregate", cluster_by_event_only =
                                   TRUE)
    # Combine the plots
    combined_plot <-
      plot_grid(
        idvl_bin_scatter,
        agg_bin_scatter,
        ncol = 2,
        align = 'v',
        axis = 'tb',
        rel_widths = c(2, 2)
      )
    # Export the combined plot
    ggsave(
      "Graphs/calibrationBin.png",
      combined_plot,
      device = "png",
      width = 7,
      height = 3
    )
    
    # Variance Regressions
    # Error On Disagreement Regressions.
    var_regs_int <- fn_regress_and_test(df_collapsed_bin_all_vars_indvl)
    var_regs_no_int <-
      fn_regress_and_test(df_collapsed_bin_all_vars_indvl, include_intercept = FALSE)
    
    names(df_collapsed_bin_all_vars_indvl)[names(df_collapsed_bin_all_vars_indvl) == "dataSet"] <-
      "forecast_var"
    # Combining groups ==> that is, taking averages over forecasted variables/timeToResolution.
    df_collapsed_bin_all_vars_indvl <-
      df_collapsed_bin_all_vars_indvl %>%
      # Create and modify 'distance_from_resolution_trucate'.
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
    # Regressions
    var_regs_int_avg <-
      fn_regress_and_test(df_collapsed, simple_reg = TRUE)
    var_regs_no_int_avg <-
      fn_regress_and_test(df_collapsed,
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
    df_fully_collapsed <- df_fully_collapsed[rep(1, 2),]
    var_regs_no_int_avg_only <-
      fn_regress_and_test(df_fully_collapsed,
                       simple_reg = TRUE,
                       include_intercept = FALSE)
    
    # Creating the disagreement regression table.
    mods <-
      list(
        var_regs_int$model,
        var_regs_no_int$model,
        var_regs_int_avg,
        var_regs_no_int_avg,
        var_regs_no_int_avg_only
      )
    stargazer_table_disagg <- stargazer(
      mods,
      type = "latex",
      header = FALSE,
      se = list(
        var_regs_int$robust_se,
        var_regs_no_int$robust_se,
        summary(var_regs_int_avg)$coefficients[, "Std. Error"],
        summary(var_regs_no_int_avg)$coefficients[, "Std. Error"],
        summary(var_regs_no_int_avg_only)$coefficients[, "Std. Error"]
      ),
      add.lines = list(c(
        "P(Beta=0)",
        sprintf("%.4f", var_regs_int$p_value),
        sprintf("%.4f", var_regs_no_int$p_value),
        sprintf("%.4f", summary(var_regs_int_avg)$coefficients["disagreement_EV_minus_forecaster_i", "Pr(>|t|)"]),
        sprintf("%.4f", summary(var_regs_no_int_avg)$coefficients["disagreement_EV_minus_forecaster_i", "Pr(>|t|)"]),
        ""
      )),
      covariate.labels = c("Disagreement", "Constant"),
      omit.stat = c("f", "ser", "adj.rsq"),
      # Drop
      column.labels = c("Individual", "est2", "est3", "est4", "est5"),
      model.numbers = FALSE,
      star.cutoffs = NA,
      # Disable significance stars
      dep.var.labels.include = FALSE,
      # Remove "Dependent variable" label
      dep.var.caption = "",
      # Remove the dependent variable caption
      digits = 4
    )
    
    # Write the table to a .tex file
    cat(stargazer_table_disagg, file = "Results/disagreement.tex")
  }