# "bin_diff_stat": for a forecaster-event @ a point in time, we need a single bin size.
# Options are: mode (default) or mean. This pertains to the non-smooth case.
# 'smooth' is a flag -> smooth out forecaster's predictions => not so discrete.
fn_disagreement_mse <- function(spf_micro_ind, pred_vars, bin_diff_stat, smooth) {
  # Initialize empty lists to store data frames by forecasting variable.
  spf_ind_collapsed_bin <- list() # Aggregate over bins
  spf_ind_collapsed_bin_agg <- list()
  
  for (pred_var in pred_vars) {
    spf_micro_ind_var <- spf_micro_ind[[pred_var]] # One forecasting variable at a time.
    
    spf_micro_ind_var <- spf_micro_ind_var %>%
      group_by(id, time, event) %>% # For an individual @ a point in time forecasting a given event.
      mutate(grpd_id_time_event = cur_group_id()) %>%
      ungroup()
    
    # Shrinking wide bins.
    spf_micro_ind_var <- spf_micro_ind_var %>%
      mutate(
        bin_h = ifelse(bin_h > 100, bin_l + 1, bin_h),
        bin_l = ifelse(bin_l < -100, bin_h - 1, bin_l)
      )
    
    if(smooth)
    {
      # Initialize an empty dataframe to store the results.
      results <- data.frame(grpd_id_time_event = numeric(), EV = numeric(), var_predicted = numeric())
      
      unique_grpd_id_time_event <- unique(spf_micro_ind_var$grpd_id_time_event)
      for (id_time_event in unique_grpd_id_time_event) {
        # Subset the data.
        spf_ind_data_sub <- spf_micro_ind_var[spf_micro_ind_var$grpd_id_time_event == id_time_event, ]
        
        # Partition each bin, while putting more weight in p (higher prob -> more weight).
        spf_ind_data_sub <- spf_ind_data_sub %>%
          rowwise() %>%
          mutate( # Need at least 10 points for numerical integration.
            n_bin_partitions = ifelse(p == 0, 0, max((bin_h - bin_l) * (500 * p), 10))
          ) %>%
          ungroup()
  
        # Need an integer for the weights.
        spf_ind_data_sub$n_bin_partitions <- as.integer(spf_ind_data_sub$n_bin_partitions)
        
        # Then expand the rows accordingly.
        spf_ind_data_sub <- spf_ind_data_sub %>%
          uncount(weights = n_bin_partitions)
        
        # Calculate partition size again after uncount().
        spf_ind_data_sub <- spf_ind_data_sub %>%
          rowwise() %>%
          mutate(
            n_bin_partitions = ifelse(p == 0, 0, max((bin_h - bin_l) * (500 * p), 10))
          ) %>%
          ungroup()
        
        spf_ind_data_sub <- spf_ind_data_sub %>%
          # Then interpolation between bin_l and bin_h. 
          group_by(bin) %>%
          mutate(binValue_fine = bin_l + (row_number() / n_bin_partitions) * (bin_h - bin_l)) %>%
          ungroup()
  
        density_values <- density(spf_ind_data_sub$binValue_fine) # , adjust = 1.5
        
        # Calculate the expected value by integrating over the density curve.
        EV <- integrate(function(x) x * approxfun(density_values)(x), 
                                    min(density_values$x), max(density_values$x))$value
        # Calculate the variance by integrating over the squared differences.
        var_predicted <- integrate(function(x) (x - EV)^2 * approxfun(density_values)(x),
                              min(density_values$x), max(density_values$x), subdivisions = 200)$value
        
        # Create a new row for the results dataframe.
        results_row <- data.frame(grpd_id_time_event = id_time_event, EV = EV, var_predicted = var_predicted)
        # Then append the results.
        results <- bind_rows(results, results_row)
      }
      
      # Merge the results dataframe back to the original dataframe
      spf_micro_ind_var <- merge(spf_micro_ind_var, results, by = "grpd_id_time_event")
    }
    else # Discrete predictions.
    {
      # For a given time + event, what is the expected value / variance of a given forecaster?
      spf_micro_ind_var <- spf_micro_ind_var %>%
        mutate(t_component_EV = p * bin_value) %>% # EV
        group_by(grpd_id_time_event) %>%
        arrange(bin, .by_group = TRUE) %>%
        mutate(EV = sum(t_component_EV, na.rm = TRUE)) %>%
        ungroup()
      
      spf_micro_ind_var <- spf_micro_ind_var %>%
        mutate(t_component_var = p * (bin_value - EV) ^ 2) %>% # Var
        group_by(grpd_id_time_event) %>%
        mutate(var_predicted_across_bins = sum(t_component_var, na.rm = TRUE)) %>%
        ungroup()
      
      # Mode func.
      get_mode <- function(v) {
        freq <- table(na.omit(v))
        as.numeric(names(freq)[which.max(freq)])
      }
      
      # Function to calculate mode or mean based on 'bin_diff_stat' argument.
      get_bin_diff_stat <- function(v, stat = "mode") {
        if (stat == "mean") {
          return(mean(na.omit(v)))
        } else {  # Default to mode
          return(get_mode(v))
        }
      }
      
      # Calculate bin_diff Mode.
      spf_micro_ind_var <- spf_micro_ind_var %>%
        mutate(bin_diff = bin_h - bin_l) %>%
        group_by(grpd_id_time_event) %>%
        mutate(common_bin_diff = get_bin_diff_stat(bin_diff, bin_diff_stat)) %>%
        ungroup()
      
      # Calculate Var Predicted
      # Within bin variance => all bins have the same value because uniform across them.
      spf_micro_ind_var <- spf_micro_ind_var %>%
        mutate(var_predicted = var_predicted_across_bins + ((1 / 12) * common_bin_diff ^ 2))
    }
    
    # Now, what is our observation of this forecasters squared error FROM REALITY
    spf_micro_ind_var$sq_dev_from_realization <- (spf_micro_ind_var$EV - spf_micro_ind_var$realization) ^ 2

    # Drop variables starting with t_co
    spf_micro_ind_var <- spf_micro_ind_var[,!grepl("^t_co", names(spf_micro_ind_var))]

    # Now, for a given time/event, what is the disagreement (i.e., variance) between forecasters in terms of EVs?
    spf_micro_ind_var <- spf_micro_ind_var %>%
      arrange(id, event, time, bin) %>%
      group_by(time, event) %>% # Subsumes forecasters.
      mutate(grpd_time_event = cur_group_id(),
             forecasters_EV = mean(EV, na.rm = TRUE)) %>%  # EV across forecasters.
      ungroup() %>%
      # Calculate the squared deviation.
      mutate(sq_dev_from_mean_EV = (EV - forecasters_EV) ^ 2) %>%
      group_by(grpd_time_event) %>%
      # Compute disagreement.
      mutate(disagreement_EV = mean(sq_dev_from_mean_EV, na.rm = TRUE)) %>%
      ungroup()

    # For a given event @ a point in time, how many forecasters are there?
    # This may differ by event and across time (e.g., one quarter to the next, etc.)
    # This is a running count.
    spf_micro_ind_var <- spf_micro_ind_var %>%
      arrange(grpd_id_time_event, bin) %>%
      group_by(grpd_id_time_event) %>%
      mutate(t_first = as.integer(row_number() == 1)) %>%
      ungroup() %>%
      arrange(grpd_time_event, id, bin) %>%
      group_by(grpd_time_event) %>%
      mutate(within_id = cumsum(t_first)) %>%
      ungroup()

    # Initialize disagreement for all forecasters.
    spf_micro_ind_var$disagreement_EV_minus_person_i <- 0
    # Largest number of forecasters for any event.
    max_withinId <- max(spf_micro_ind_var$within_id, na.rm = TRUE)

    # Let's cycle through each forecaster, excluding one observation at a time, calculating the disagreement.
    for (i in 1:max_withinId) {
      data_grouped <- spf_micro_ind_var %>%
        group_by(grpd_time_event) %>%
        filter(within_id != i) %>% # Exclude the current observation.
        summarise(
          t_mean_EV = mean(EV, na.rm = TRUE),
          t_disagreement_EV = mean((EV - t_mean_EV) ^ 2, na.rm = TRUE)
        ) %>%
        ungroup()

      # Assign the calculated disagreement value.
      for (grp in unique(spf_micro_ind_var$grpd_time_event)) {
        spf_micro_ind_var$disagreement_EV_minus_person_i[spf_micro_ind_var$grpd_time_event == grp &
                                          spf_micro_ind_var$within_id == i] <-
          data_grouped$t_disagreement_EV[data_grouped$grpd_time_event == grp]
      }

      # Remove temporary variables.
      spf_micro_ind_var <- spf_micro_ind_var %>% select(-which(grepl("^t_", names(spf_micro_ind_var))))
    }

    spf_micro_ind_var <- spf_micro_ind_var %>%
      mutate(# Time till event "resolves".
        distance_from_resolution = event + 1 - time,
        dataSet = pred_var)

    spf_micro_ind_var <- spf_micro_ind_var %>%
      mutate(error = sq_dev_from_realization - var_predicted,
             perc_error = error / var_predicted)

    # One row per forecaster event-time (they're repeated across bins).
    spf_micro_ind_var <- spf_micro_ind_var %>%
      distinct(grpd_id_time_event, .keep_all = TRUE)

    cols_to_drop <- grep("^bin|p$", names(spf_micro_ind_var), value = TRUE)
    spf_micro_ind_var <- spf_micro_ind_var %>%
      select(-all_of(cols_to_drop))
    # Collapsed across the bins.
    spf_ind_collapsed_bin[[pred_var]] <- spf_micro_ind_var

    # Keep specific columns only.
    spf_ind_sub_data <- spf_micro_ind_var %>%
      select(
        sq_dev_from_realization,
        var_predicted,
        disagreement_EV,
        time,
        event,
        grpd_time_event,
        dataSet,
        distance_from_resolution,
        forecasters_EV,
        realization,
        disagreement_EV_minus_person_i,
        error
      )

    # Collapse data by event-time and average across forecasters.
    spf_ind_collapsed <- spf_ind_sub_data %>%
      group_by(grpd_time_event) %>%
      summarise(
        sq_dev_from_realization = mean(sq_dev_from_realization, na.rm = TRUE),
        var_predicted = mean(var_predicted, na.rm = TRUE),
        error = mean(error, na.rm = TRUE),
        disagreement_EV = mean(disagreement_EV, na.rm = TRUE),
        time = mean(time, na.rm = TRUE),
        event = mean(event, na.rm = TRUE),
        EV = mean(forecasters_EV, na.rm = TRUE),
        disagreement_EV_minus_person_i = mean(disagreement_EV_minus_person_i, na.rm = TRUE),
        realization = mean(realization, na.rm = TRUE),
        dataSet = first(dataSet)
      ) %>%
      ungroup()

    spf_ind_collapsed_bin_agg[[pred_var]] <- spf_ind_collapsed
  }

  # Combine PRGDP PRUNEMP PRCPCE PRCCPI PRPGDP
  # Initialize empty data frames
  spf_ind_collapsed_bin_all_vars <- data.frame(temp = numeric(0))
  spf_ind_collapsed_bin_all_vars_agg <- data.frame(temp = numeric(0))
  spf_micro_ind_all_vars <- data.frame(temp = numeric(0))

  # Iterate over each variable
  for (pred_var in pred_vars) {
    spf_ind_collapsed_bin_all_vars <- rbind(spf_ind_collapsed_bin_all_vars, spf_ind_collapsed_bin[[pred_var]])

    spf_ind_collapsed_bin_all_vars_agg <-
      rbind(spf_ind_collapsed_bin_all_vars_agg, spf_ind_collapsed_bin_agg[[pred_var]])

    spf_micro_ind_filt <-
      spf_micro_ind[[pred_var]][!spf_micro_ind[[pred_var]]$resolution == 2 &
                  !is.na(spf_micro_ind[[pred_var]]$realization),]
    spf_micro_ind_filt$dataSet <- pred_var
    spf_micro_ind_all_vars <-
      rbind(spf_micro_ind_all_vars, spf_micro_ind_filt)
  }
  return(list("spf_ind_collapsed_bin" = spf_ind_collapsed_bin,
              "spf_ind_collapsed_bin_agg" = spf_ind_collapsed_bin_agg,
              "spf_ind_collapsed_bin_all_vars" = spf_ind_collapsed_bin_all_vars,
              "spf_ind_collapsed_bin_all_vars_agg" = spf_ind_collapsed_bin_all_vars_agg,
              "spf_micro_ind_all_vars" = spf_micro_ind_all_vars))
}