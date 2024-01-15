source("fn_reg_variable_names.R")

fn_construct_agg_data <- function(spf_micro_ind, pred_vars, spf_collapsed_bin_disagreement) {
  # Want a dataset that looks like:
  # event bin numBins prob timeToEnd
  
  # Initialize empty lists to store data frames by forecasting variable.
  spf_ind_agg <- list()
  spf_ind_collapsed_bin_agg <- list()

  for (pred_var in pred_vars) {
    spf_micro_ind_var <- spf_micro_ind[[pred_var]] # One forecasting variable at a time.
    
    spf_agg_data <- spf_micro_ind_var %>%
      group_by(event, time, bin) %>%
      mutate(grpd_event_time_bin = cur_group_id()) %>%
      # Get average probabilities over event/time/bin.
      mutate(p_agg = mean(p, na.rm = TRUE)) %>%
      ungroup() %>%
      # Drop individual-level stuff.
      select(-id, -industry, -p) %>%
      distinct() %>% # Remove duplicate rows.
      arrange(event, time, bin) %>%
      mutate(data_set = pred_var)
    
    spf_ind_agg[[pred_var]] <- spf_agg_data
    
    # Sanity check: probability adds up to 1.
    spf_agg_data <- spf_agg_data %>%
      group_by(time, event) %>%
      mutate(should_be_1 = sum(p_agg, na.rm = TRUE)) %>%
      ungroup()
    tolerance = 0.00199
    prob_check <- all(abs(spf_agg_data$should_be_1 - 1) < tolerance)
    if (!prob_check) {
      print(pred_var)
      print("Probabilities don't add to 1")
    }
    # stopifnot(all(abs(spf_agg_data$should_be_1 - 1) < tolerance))

    spf_agg_data <- spf_agg_data %>%
      # For a given event-time, what is the expected value?
      mutate(t_component_EV = p_agg * bin_value) %>%
      # Group by time and event
      group_by(time, event) %>%
      mutate(grpd_time_event = cur_group_id()) %>%
      # Sort by bin within an event-time.
      arrange(bin, .by_group = TRUE) %>%
      mutate(EV = sum(t_component_EV, na.rm = TRUE)) %>%
      # For a given event-time, what is the variance of the aggregate?
      mutate(t_component_var = p_agg * (bin_value - EV) ^ 2) %>%
      mutate(var_predicted_agg = sum(t_component_var, na.rm = TRUE)) %>%
      ungroup()
    
    # Now, what is our observation of this "aggregate" forecasters squared error from reality.
    spf_agg_data <- spf_agg_data %>%
      mutate(sq_dev_from_realization_agg = (EV - realization) ^ 2) %>%
      # Drop columns that start with t_co
      select(-starts_with("t_co"), -should_be_1)

    spf_agg_data <- spf_agg_data %>%
      mutate(error_agg = sq_dev_from_realization_agg - var_predicted_agg) %>%
      mutate(perc_error_agg = error_agg / var_predicted_agg) %>%
      # One row per event-time (they're repeated across bins).
      distinct(grpd_time_event, .keep_all = TRUE) %>% # ".keep_all = TRUE" ensures that all other columns are retained
      # Drop columns that start with bin and p_agg
      select(-starts_with("bin"), -starts_with("p_agg"))
    
    # Let's get the measure of disagreement -> this is only accessible from the individual level 
    # data.
    df_spf_agg <- spf_collapsed_bin_disagreement[[pred_var]]
    # Merge the datasets on event-time appending disagreement only.
    spf_agg_data <- spf_agg_data %>%
      left_join(
        df_spf_agg %>% select(event, time, disagreement_EV),
        by = c("event", "time")
      ) %>%
      arrange(event, time)
    
    spf_ind_collapsed_bin_agg[[pred_var]] <- spf_agg_data
    
    # Run the regressions:
    # Regression of error on a constant, with standard errors clustered by event.
    model1 <- lm_robust(error_agg ~ 1,
                        se_type = "stata",
                        clusters = event,
                        data = spf_agg_data)
    # Regression of error on expected disagreement without an intercept, again with standard errors clustered by event.
    model2 <- lm_robust(
      error_agg ~ disagreement_EV - 1,
      se_type = "stata",
      clusters = event,
      data = spf_agg_data
    )
    # View model results (clustered robust standard errors)
    cat(
      "************************************",
      pred_var,
      "************************************",
      "\n"
    )
    options(digits = 7)
    fn_reg_variable_names(model1, spf_agg_data)
    print(model1)
    cat("\n")
    fn_reg_variable_names(model2, spf_agg_data)
    print(model2)
  }
  
  # Combine forecasting variables (PRGDP PRUNEMP PRCPCE PRCCPI PRPGDP)
  # Initialize empty data frames
  all_vars_agg <- data.frame(temp = numeric(0))
  all_vars_agg_with_bins <- data.frame(temp = numeric(0))
  
  # Iterate over each variable and append data.
  for (pred_var in pred_vars) {
    all_vars_agg_with_bins <- rbind(all_vars_agg_with_bins, spf_ind_agg[[pred_var]])
    all_vars_agg <- rbind(all_vars_agg, spf_ind_collapsed_bin_agg[[pred_var]])
  }
  return(list("all_vars_agg_with_bins" = all_vars_agg_with_bins, 
               "all_vars_agg" = all_vars_agg))
}