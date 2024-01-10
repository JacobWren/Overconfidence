# "bin_diff_stat": for a forecaster-event @ a point in time, we need a single bin size.
# Options are: mode (default) or mean.
fn_disagreement_mse <- function(spf_micro_ind, bin_diff_stat) {
  vars_to_pred <- vars_to_forecast[-length(vars_to_forecast)] # Exclude 'RECESS'.
  # Initialize empty lists to store data frames by forecasting variable.
  spf_ind_collapsed_bin <- list() # Aggregate over bins
  spf_ind_collapsed_bin_agg <- list()
  
  for (pred_var in vars_to_pred) {
    # First filter: Here, everyone is forced to agree -> not interesting.
    # Second filter: We don't have realization yet for future.
    spf_ind_data <-
      subset(spf_micro_ind[[pred_var]], resolution != 2 &
               !is.na(realization))
    
    # Would be great to smooth out the person's predictions somehow => not so discrete.
    spf_ind_data <- spf_ind_data %>%
      group_by(id, time, event) %>% # For an individual @ a point in time forecasting a given event.
      mutate(grpd_id_time_event = cur_group_id()) %>%
      ungroup()
    
    ############# Continuous Choice START #############
    # JAKE: In the middle of refactoring, but pausing to return to smooth out a forecasters
    # prediction...
    # ITERATE over a few cases, look at densities, try to find a case with "gaps" in 
    # probability, then take EV and variance of the density and compare to discrete case.
    
    # Unique values of grpd_id_time_event
    unique_grpd_id_time_event <- unique(spf_ind_data$grpd_id_time_event)
    
    # Iterate over each unique grpd_id_time_event value
    for (value in unique_grpd_id_time_event) {
      subset <- spf_ind_data[spf_ind_data$grpd_id_time_event == value, ]
  
      # Expanding each row of the dataframe by a factor of 10 for granularity.
      subset <- subset %>%
        uncount(weights = 10, .id = "new")
      # Shrinking wide bins.
      subset <- subset %>%
        mutate(
          binH = ifelse(binH > 100, binL + 1, binH),
          binL = ifelse(binL < -100, binH - 1, binL)
        )
  
      # Make finer bins to then fit kde over.
      subset <- subset %>%
        group_by(bin) %>%
        # Linear interpolation between binL and binH.
        mutate(binValue_fine = binL + (row_number() / 10) * (binH - binL)) %>%
        ungroup()
  
      subset$expand <- subset$p * 10
  
      # 'expand' specifies how many times to replicate each row
      data_expanded <- subset %>%
        uncount(weights = expand)
      # Example density -> fits continuous shape over bins.
      gp <- ggplot(data_expanded, aes(x = binValue_fine)) +
        stat_density(aes(y = after_stat(density)), geom = "line") +
        labs(title = "Normalized Kernel Density Plot of binValue",
             x = "binValue",
             y = "Density") +
        xlim(min(data_expanded$binValue_fine) - 3/4, max(data_expanded$binValue_fine) + 3/4)  # Extend x-axis range
  
      ggsave(paste0("binValue_density_plot", value,".pdf"), plot = gp)
    }
    ############# Continuous Choice END #############
    
    # For a given time + event, what is the expected value / variance of a given forecaster?
    spf_ind_data <- spf_ind_data %>%
      mutate(t_componentEV = p * binValue) %>% # EV
      group_by(grpd_id_time_event) %>%
      arrange(bin, .by_group = TRUE) %>%
      mutate(EV = sum(t_componentEV, na.rm = TRUE)) %>%
      ungroup()
    
    spf_ind_data <- spf_ind_data %>%
      mutate(t_componentVar = p * (binValue - EV) ^ 2) %>% # Var
      group_by(grpd_id_time_event) %>%
      mutate(var_predicted_across_bins = sum(t_componentVar, na.rm = TRUE)) %>%
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
    spf_ind_data <- spf_ind_data %>%
      mutate(bin_diff = binH - binL) %>%
      group_by(grpd_id_time_event) %>%
      mutate(common_bin_diff = get_bin_diff_stat(bin_diff, bin_diff_stat)) %>%
      ungroup()
    
    # Calculate Var Predicted
    # Within bin variance => all bins have the same value because uniform across them.
    spf_ind_data <- spf_ind_data %>%
      mutate(VarPredicted = var_predicted_across_bins + ((1 / 12) * common_bin_diff ^ 2))
    
    # Now, what is our observation of this forecasters squared error FROM REALITY
    spf_ind_data$sqDevFromRealization <- (spf_ind_data$EV - spf_ind_data$realization) ^ 2
    
    # Drop variables starting with t_co
    spf_ind_data <- spf_ind_data[,!grepl("^t_co", names(spf_ind_data))]
    
    # Now, for a given time/event, what is the disagreement (i.e., variance) between forecasters in terms of EVs?
    spf_ind_data <- spf_ind_data %>%
      arrange(id, event, time, bin) %>%
      group_by(time, event) %>% # Subsumes forecasters.
      mutate(grpd_time_event = cur_group_id(),
             meanEV = mean(EV, na.rm = TRUE)) %>%  # EV across forecasters.
      ungroup() %>%
      # Calculate the squared deviation.
      mutate(sqDevFromMeanEV = (EV - meanEV) ^ 2) %>%
      group_by(grpd_time_event) %>%
      # Compute disagreement.
      mutate(disagreementEV = mean(sqDevFromMeanEV, na.rm = TRUE)) %>%
      ungroup()
    
    # For a given event @ a point in time, how many forecasters are there?
    # This may differ by event and across time (e.g., one quarter to the next, etc.)
    # This is a running count.
    spf_ind_data <- spf_ind_data %>%
      arrange(grpd_id_time_event, bin) %>%
      group_by(grpd_id_time_event) %>%
      mutate(t_first = as.integer(row_number() == 1)) %>%
      ungroup() %>%
      arrange(grpd_time_event, id, bin) %>%
      group_by(grpd_time_event) %>%
      mutate(withinId = cumsum(t_first)) %>%
      ungroup()
    
    # Initialize disagreement for all forecasters.
    spf_ind_data$disagreementEVMinusPersoni <- 0
    # Largest number of forecasters for any event.
    max_withinId <- max(spf_ind_data$withinId, na.rm = TRUE)
    
    # Let's cycle through each forecaster, excluding one observation at a time, calculating the disagreement.
    for (i in 1:max_withinId) {
      data_grouped <- spf_ind_data %>%
        group_by(grpd_time_event) %>%
        filter(withinId != i) %>% # Exclude the current observation.
        summarise(
          t_meanEV = mean(EV, na.rm = TRUE),
          t_disagreementEV = mean((EV - t_meanEV) ^ 2, na.rm = TRUE)
        ) %>%
        ungroup()
      
      # Assign the calculated disagreement value.
      for (grp in unique(spf_ind_data$grpd_time_event)) {
        spf_ind_data$disagreementEVMinusPersoni[spf_ind_data$grpd_time_event == grp &
                                          spf_ind_data$withinId == i] <-
          data_grouped$t_disagreementEV[data_grouped$grpd_time_event == grp]
      }
      
      # Remove temporary variables.
      spf_ind_data <- spf_ind_data %>% select(-which(grepl("^t_", names(spf_ind_data))))
    }
    
    spf_ind_data <- spf_ind_data %>%
      mutate(# Time till event "resolves".
        distanceFromResolution = event + 1 - time,
        dataSet = pred_var)
    
    spf_ind_data <- spf_ind_data %>%
      mutate(error = sqDevFromRealization - VarPredicted,
             perc_error = error / VarPredicted)
    
    # One row per forecaster event-time (they're repeated across bins).
    spf_ind_data <- spf_ind_data %>%
      distinct(grpd_id_time_event, .keep_all = TRUE)
    
    cols_to_drop <- grep("^bin|p$", names(spf_ind_data), value = TRUE)
    spf_ind_data <- spf_ind_data %>%
      select(-all_of(cols_to_drop))
    # Collapsed across the bins.
    spf_ind_collapsed_bin[[pred_var]] <- spf_ind_data
    
    # Keep specific columns only.
    spf_ind_sub_data <- spf_ind_data %>%
      select(
        sqDevFromRealization,
        VarPredicted,
        disagreementEV,
        time,
        event,
        grpd_time_event,
        dataSet,
        distanceFromResolution,
        meanEV,
        realization,
        disagreementEVMinusPersoni,
        error
      )
    
    # Collapse data by event-time and average across forecasters.
    spf_ind_collapsed <- spf_ind_sub_data %>%
      group_by(grpd_time_event) %>%
      summarise(
        avg_sqDevFromRealization = mean(sqDevFromRealization, na.rm = TRUE),
        avg_VarPredicted = mean(VarPredicted, na.rm = TRUE),
        avg_error = mean(error, na.rm = TRUE),
        avg_disagreementEV = mean(disagreementEV, na.rm = TRUE),
        time = mean(time, na.rm = TRUE),
        event = mean(event, na.rm = TRUE),
        avgEV = mean(meanEV, na.rm = TRUE),
        avg_disagreementEVMinusPersoni = mean(disagreementEVMinusPersoni, na.rm = TRUE),
        avg_realization = mean(realization, na.rm = TRUE),
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
  for (pred_var in vars_to_pred) {
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