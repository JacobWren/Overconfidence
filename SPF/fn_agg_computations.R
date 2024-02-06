source("../Helpers/fn_help.R")
source("../Helpers/fn_aggregate.R")
source("fn_computations.R")


fn_agg_computations <-
  function(spf_micro_ind,
           pred_vars,
           spf_collapsed_bin_disagreement) {
    # Initialize empty lists to store data frames by forecasting variable.
    spf_ind_agg <- list()
    spf_ind_collapsed_bin_agg <- list()
    
    for (pred_var in pred_vars) {
      df_spf_ind_var <-
        spf_micro_ind[[pred_var]] # One forecasting variable at a time.
      
      # Get average probabilities over event/time/bin -> collapse out forecasters.
      df_spf_agg_var <- fn_aggregate(df_spf_ind_var, pred_var = pred_var, to_drop = c("industry"))
      
      spf_ind_agg[[pred_var]] <- df_spf_agg_var
      
      # Sanity check: probability adds up to 1.
      df_spf_agg_var <- df_spf_agg_var %>%
        group_by(time, event) %>%
        mutate(should_be_1 = sum(p_agg, na.rm = TRUE)) %>%
        ungroup()
      tolerance = 0.00199
      prob_check <-
        all(abs(df_spf_agg_var$should_be_1 - 1) < tolerance)
      if (!prob_check) {
        print(pred_var)
        stop("Probabilities don't add to 1 in the SPF aggregate.")
      }
      
      df_spf_agg_var <- df_spf_agg_var %>%
        group_by(time, event) %>% # For an "average" forecaster @ a point in time forecasting an event.
        mutate(grpd_time_event = cur_group_id()) %>%
        ungroup()

      # For a given event-time, what is the expected value and the variance of the aggregate?
      df_spf_agg_var <-
        calculate_EV_and_var(df_spf_agg_var, "grpd_time_event", is_agg = TRUE)
      
      # What is our observation of this "aggregate" forecasters squared error from reality.
      # And how does that deviate from the predicted aggregate variance?
      df_spf_agg_var <-
        calculate_error_metrics(df_spf_agg_var, is_agg = TRUE)
      
      df_spf_agg_var <- df_spf_agg_var %>%
        # Drop columns that start with t_co
        select(-starts_with("t_co"),-should_be_1)
      
      df_spf_agg_var <- df_spf_agg_var %>%
        # One row per event-time (they're repeated across bins).
        distinct(grpd_time_event, .keep_all = TRUE) %>% # ".keep_all = TRUE" ensures that all other columns are retained
        # Drop columns that start with bin and p_agg
        select(-starts_with("bin"),-starts_with("p_agg"))
      
      # Let's get the measure of disagreement -> this is only accessible from the individual level
      # data.
      df_spf_agg <- spf_collapsed_bin_disagreement[[pred_var]]
      # Merge the datasets on event-time appending disagreement only.
      df_spf_agg_var <- df_spf_agg_var %>%
        left_join(df_spf_agg %>% select(event, time, disagreement_EV),
                  by = c("event", "time")) %>%
        arrange(event, time)
      
      spf_ind_collapsed_bin_agg[[pred_var]] <- df_spf_agg_var
      
      # Run the regressions:
      # Regression of error on a constant, with standard errors clustered by event.
      model1 <- lm_robust(
        error_agg ~ 1,
        se_type = "stata",
        clusters = event,
        data = df_spf_agg_var
      )
      # Regression of error on expected disagreement without an intercept, again with standard errors clustered by event.
      model2 <- lm_robust(
        error_agg ~ disagreement_EV - 1,
        se_type = "stata",
        clusters = event,
        data = df_spf_agg_var
      )
      # View model results (clustered robust standard errors)
      cat(
        "************************************",
        pred_var,
        "************************************",
        "\n"
      )
      options(digits = 7)
      fn_help(model1, df_spf_agg_var)
      print(model1)
      cat("\n")
      fn_help(model2, df_spf_agg_var)
      print(model2)
    }
    
    # Combine forecasting variables (PRGDP PRUNEMP PRCPCE PRCCPI PRPGDP)
    # Initialize empty data frames
    all_vars_agg <- data.frame(temp = numeric(0))
    all_vars_agg_with_bins <- data.frame(temp = numeric(0))
    
    # Iterate over each variable and append data.
    for (pred_var in pred_vars) {
      all_vars_agg_with_bins <-
        rbind(all_vars_agg_with_bins, spf_ind_agg[[pred_var]])
      all_vars_agg <-
        rbind(all_vars_agg, spf_ind_collapsed_bin_agg[[pred_var]])
    }
    return(
      list(
        "all_vars_agg_with_bins" = all_vars_agg_with_bins,
        "all_vars_agg" = all_vars_agg
      )
    )
  }
