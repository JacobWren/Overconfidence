# Match any figures used in paper/slides.


fn_summary_figs <- function(df) {
  print(paste0("Avg true variance is: ", round(mean(df$var_true), 2), 
              ". People’s estimate is: ", round(mean(df$var), 2), " (", percent(mean(df$var) / mean(df$var_true)),")"))
  print(paste("Avg true MSE is:", round(mean(df$mse_true), 2)))
  
  print(paste0("Account for ~", percent(mean(df$var) / mean(df$mse_true)), " of MSE on average"))

  mean_abs_error_in_avg_mean_est <- df %>%
    filter(first == 1) %>%
    summarise(mean_abs_error = mean(abs(avg_mean - mean_true) / mean_true, na.rm = TRUE)) %>%
    pull(mean_abs_error) %>%
    {
      . * 100
    } # Convert to percentage
  
  print(
    paste(
      "Wisdom of Crowds: Absolute deviation of average of mean estimates from truth averages ∼",
      sprintf("%.0f%%", mean_abs_error_in_avg_mean_est)
    )
  )
  
  df <- df %>%
    mutate(
      var_of_collective_estimate = if_else(first == 1 &
                                             with_line == 0, avg_var + avg_var_of_mean, NA_real_)
    )
  
  mean_var_of_collective_estimate <-
    mean(df$var_of_collective_estimate, na.rm = TRUE)
  mean_var_true <- mean(df$var_true, na.rm = TRUE)
  wisdom <- (mean_var_of_collective_estimate / mean_var_true) * 100
  
  print(
    paste(
      "Wisdom of Crowds Aggressive: Predicted Var of average distribution averages",
      sprintf("%.0f%%", wisdom),
      "true distribution"
    )
  )
  
  df_no_model <- df %>%
    filter(first == 1, with_line == 0)
  
  # Correlation test
  test_result <- cor.test(df_no_model$avg_var_of_mean, df_no_model$var_true_model)
  correl <- test_result$estimate
  p_value <- test_result$p.value
  print(paste0("Prediction: dispersion in means connected to model uncertainty (corr=",
               round(correl, 2), " (p=", round(p_value, 3), "))"))
}
