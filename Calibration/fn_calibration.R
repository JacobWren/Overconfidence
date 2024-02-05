source("../Regressions/fn_regress.R")


# Calibration functions: Traditional calibration bin scatter plot and a couple of helpers.


# Helper function.
fn_generate_p_empirical <- function(df) {
  # What bin captures the realized outcome?
  df %>%
    mutate(p_empirical = ifelse(realization > bin_l &
                                  realization <= bin_h, TRUE, FALSE))
}

# Smooth (binary) empirical probability for calibration plot (for SPF).
fn_smooth <- function(df, p_var, smoothed_name, spar_value) {
  # Fit a curve through the data points represented by (prob_var, p_empirical). The smooth.spline function in R creates a
  # spline that passes through or near these points in a way that minimizes the overall curvature of the line,
  # resulting in a smooth representation of the data.
  df %>%
    mutate(!!smoothed_name := {
      spline_fit <-
        smooth.spline(get(p_var), p_empirical, spar = spar_value)  # Fit the spline.
      predict(spline_fit, x = get(p_var))$y  # Predict using the original probs.
    }) %>%
    select(!!sym(p_var),!!sym(smoothed_name)) %>%  # Keep only p_var and the smoothed p_empirical.
    distinct()  # Remove duplicate rows.
}


fn_calibration_bin_scatter <-
  function(df,
           prob_var,
           title,
           cluster = "double",
           case = "SPF") {
    # Traditional calibration bin scatter plot.
    epsilon <-
      1e-10  # To match STATA's cut()
    df <- df %>%
      mutate(
        # Dividing the perceived probability into bins based on the specified breakpoints.
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
    if (!(sum(table(df$prob_cut)) == nrow(df))) {
      stop("Your calibration bins are leaving some observations out.")
    }
    
    # Creating dummy variables using model.matrix()
    dummy_vars <-
      # The - 1 in the formula means that we're not including the intercept.
      model.matrix( ~ as.factor(prob_cut_codes) - 1, data = df)
    dummy_vars <- as.data.frame(dummy_vars)
    # Renaming the dummy variables.
    names(dummy_vars) <-
      paste0("prob_cut_dum_", seq_along(dummy_vars))
    # Binding the dummy variables to the original data frame.
    df <- cbind(df, dummy_vars)
    # Regression for error bars in the scatter plot.
    bin_scatter <-
      fn_regress(
        df,
        use_dummies = TRUE,
        cluster = cluster,
        case = case,
        calibration = TRUE
      )
    
    coefs <- bin_scatter$beta
    robust_se <- bin_scatter$se
    
    df$prob_cut <- as.factor(df$prob_cut)
    
    # Calculate the statistics for calibration scatter plot by bin.
    df <- df %>%
      group_by(prob_cut) %>%
      mutate(
        p_mean = mean(!!sym(prob_var), na.rm = TRUE),
        p_est = coefs[paste0("prob_cut_dum_", prob_cut)],
        p_est_lower = coefs[paste0("prob_cut_dum_", prob_cut)] - 1.96 * robust_se[paste0("prob_cut_dum_", prob_cut)],
        p_est_upper = coefs[paste0("prob_cut_dum_", prob_cut)] + 1.96 * robust_se[paste0("prob_cut_dum_", prob_cut)]
      )
    # Append the point (0.75, 0.75) to extend the line -- just for cosmetic purposes.
    end_point <- 0.75
    
    # Are we playing god?
    y_title <- ifelse(case == "SPF", "Empirical", "True")
    
    bin_scatter <- ggplot(df, aes(x = p_mean, y = p_est)) +
      geom_line(
        aes(x = p_mean, y = p_mean),
        data = data.frame(p_mean = c(0, end_point)),
        color = "gray",
        linetype = "dashed",
        linewidth = 0.75
      ) +
      geom_point(color = "red", size = 1.7) +
      geom_errorbar(
        aes(ymin = p_est_lower, ymax = p_est_upper),
        color = "gray50",
        width = 0.028,
        size = .41
      ) +
      labs(title = title,
           x = "Belief",
           y = paste(y_title, "Probability")) +
      scale_x_continuous("Belief",
                         labels = percent_format(),
                         breaks = seq(0, end_point, by = 0.25)) +
      scale_y_continuous(
        paste(y_title, "Probability"),
        labels = percent_format(),
        breaks = seq(0, end_point, by = 0.25)
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.border = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        plot.background = element_rect(fill = "white", color = NA)
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
