# Get average probability and 'slice' to the aggregate level.


fn_aggregate <-
  function(data,
           pred_var = NULL,
           case = "SPF",
           to_drop = c()) {
    group_by_cols <-
      if (case == "SPF")
        c("event", "time", "bin")
    else
      c("pic", "bin")
    
    df_agg <- data %>%
      group_by(across(all_of(group_by_cols))) %>%
      mutate(aggregate_forecaster = cur_group_id()) %>%
      # Get average probabilities over event/time/bin (pic/bin) -> collapse out forecasters (participants)
      mutate(p_agg = mean(p, na.rm = TRUE)) 
    
    if (case != "SPF") {
      df_agg <- df_agg %>%
        mutate(p_agg_true = mean(p_true, na.rm = TRUE))
    }

    df_agg <- df_agg %>%
      ungroup() %>%
      select(-id,-p,-all_of(to_drop)) 
    
    if (case != "SPF") {
      df_agg <- df_agg %>%
        select(-matches(paste(c("^sd", "^var", "^mean", "^mse", "less"), collapse = "|")))
    }
    df_agg <- df_agg %>%
      distinct() %>% 
      arrange(across(all_of(group_by_cols))) 
      # group_by(across(all_of(setdiff(group_by_cols, "bin")))) %>%
      # # For an "average" forecaster @ a point in time forecasting an event (for SPF).
      # mutate(current_event = cur_group_id()) %>%
      # ungroup()
    
    if (case == "SPF") {
      df_agg <- df_agg %>%
        mutate(data_set = pred_var)
    }
    
    return(df_agg)
  }



















