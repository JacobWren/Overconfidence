# Match any figures used in paper/slides.


fn_summary_figs <- function(df) {
  print(paste("Avg true variance is:", round(mean(df$var_true), 2)))
  print(paste("Avg true MSE is:", round(mean(df$mse_true), 2)))
  
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
      sprintf("%.2f%%", mean_abs_error_in_avg_mean_est)
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
      sprintf("%.2f%%", wisdom),
      "true distribution"
    )
  )
}
