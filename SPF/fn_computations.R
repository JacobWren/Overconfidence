# Compute statistics.


fn_calculate_EV_and_var <- function(df, grpd_level, is_agg = FALSE) {
  # Individual forecaster or average forecaster?
  suffix <- if (is_agg)
    "_agg"
  else
    ""
  
  prob <- paste0("p", suffix)
  
  # Calculate Expected Value (EV)
  # For a given event-time, what is the expected value?
  df <- df %>%
    mutate(t_component_EV = !!sym(prob) * bin_value) %>%
    # Group by.
    group_by(!!sym(grpd_level)) %>%
    # Sort by bin within an event-time.
    arrange(bin, .by_group = TRUE) %>%
    mutate(EV = sum(t_component_EV, na.rm = TRUE)) %>%
    ungroup()
  
  # Across bin variance
  df <- df %>%
    mutate(t_component_var = !!sym(prob) * (bin_value - EV) ^ 2) %>%
    group_by(!!sym(grpd_level)) %>%
    mutate(var_predicted_across_bins = sum(t_component_var, na.rm = TRUE)) %>%
    ungroup()
  
  # Within bin variance
  df <- df %>%
    mutate(t_within_bin_var = ((bin_h - bin_l) ^ 2) / 12) %>% # Assuming a uniform distribution within each bin.
    mutate(t_weighted_within_bin_var = !!sym(prob) * t_within_bin_var) %>%
    group_by(!!sym(grpd_level)) %>%
    mutate(var_predicted_within_bins = sum(t_weighted_within_bin_var, na.rm = TRUE)) %>%
    ungroup()
  
  total_var <- paste0("var_predicted", suffix)
  # Get total predicted variance
  df <- df %>%
    mutate(!!sym(total_var) := var_predicted_across_bins + var_predicted_within_bins)
  
  return(df)
}


calculate_error_metrics <- function(df, is_agg = FALSE) {
  # Define the common suffix
  suffix <- if (is_agg)
    "_agg"
  else
    ""
  
  # Create column names by concatenating the suffix
  sq_dev_from_reality <- paste0("sq_dev_from_realization", suffix)
  var_pred <- paste0("var_predicted", suffix)
  error <- paste0("error", suffix)
  perc_error <- paste0("perc_error", suffix)
  
  # Add a column for squared deviation from the realization.
  df <- df %>%
    mutate(!!sym(sq_dev_from_reality) := (EV - realization) ^ 2)
  
  # Add columns for error and percentage error.
  df[[error]] <- df[[sq_dev_from_reality]] - df[[var_pred]]
  df[[perc_error]] <- df[[error]] / df[[var_pred]]
  
  return(df)
}
