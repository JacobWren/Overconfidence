# Merge in realizations -- treat these as a degenerate forecast.


fn_add_outcomes <- function(outcomes, spf_micro, pred_vars) {
  # Initialize empty lists to store data
  spf_ind <- list() # Individual forecaster
  spf_agg <- list() # Average forecaster
  for (pred_var in pred_vars) {
    # The next several steps are preparing to merge the realized outcomes with the SPF data.
    df_spf_micro <- spf_micro[[pred_var]]
    
    # The data will first be sorted by id, then by event within each id, then by time within each event,
    # and finally by bin within each time.
    df_spf_micro <- df_spf_micro %>%
      arrange(id, event, time, bin) %>%
      select(-year) # Drop year
    
    # Group by id and event and create a new variable grpd_id_event that assigns a unique group identifier.
    df_spf_micro <- df_spf_micro %>%
      group_by(id, event) %>%
      mutate(grpd_id_event = cur_group_id())
    # Calculate the maximum value of the time variable within each forecaster-survey.
    df_spf_micro <- df_spf_micro %>%
      group_by(grpd_id_event) %>%
      mutate(time_max = max(time)) %>%
      ungroup()
    
    df_spf_micro <-
      df_spf_micro %>% # Setting the next operation up.
      mutate(time_dup = ifelse(time_max == time, 2, 1))
    
    # Expand the dataframe based on the time_dup variable by duplicating the rows according to the values in t_dup
    # and create a duplicate variable that distinguishes between the original and duplicated rows.
    df_spf_micro <- df_spf_micro %>%
      mutate(row_id = row_number()) %>%  # Create a temporary identifier for each row
      uncount(time_dup) %>%  # Duplicate rows
      group_by(row_id) %>%  # Group by the temporary identifier
      mutate(duplicate = row_number()) %>%  # Number each row within its group
      ungroup() %>%  # Remove grouping
      select(-row_id)  # Remove the temporary identifier
    
    df_spf_micro <- df_spf_micro %>%
      arrange(grpd_id_event, time, duplicate)
    
    # annoyingly, for recess event=1983 means ending 4th quarter 1982 but, for GDP, event=1982 means ending 4th quarter 1982.
    df_spf_micro <- df_spf_micro %>%
      mutate(
        time = case_when(
          # Very last moment of the year.
          duplicate == 2 & pred_var == "RECESS"  ~ event - 0.001,
          duplicate == 2 & pred_var != "RECESS"  ~ event + 0.999,
          TRUE                              ~ time
        )
      )
    # Passed in data from fn_realized_outcomes_to_bins().
    realized_outcomes <- outcomes[[pred_var]]
    
    # Add a source indicator to each dataframe.
    df_spf_micro$in_data <- TRUE
    realized_outcomes$in_ro <- TRUE
    
    # Perform the merge
    df_spf_micro <- df_spf_micro %>%
      full_join(realized_outcomes, by = c("event", "bin")) %>%
      arrange(event, bin)
    
    # Mimic statas' _merge indicator
    df_spf_micro <- df_spf_micro %>%
      mutate(statas_merge = case_when(
        !is.na(in_data) & is.na(in_ro) ~ 1,
        # Present only in data
        is.na(in_data) &
          !is.na(in_ro) ~ 2,
        # Present only in realized_outcomes
        TRUE ~ 3  # Present in both
      ))
    
    df_spf_micro <- select(df_spf_micro, -in_data, -in_ro)
    
    df_spf_micro <-
      df_spf_micro %>%  # Drop rows where statas' _merge is 2 (only in realized_outcomes).
      filter(!(pred_var == "PRGDP" &
                 duplicate == 2 & event == 1981),
             statas_merge != 2)
    
    df_spf_micro <- df_spf_micro %>%
      mutate(p = ifelse(duplicate == 2, 0, p))
    
    df_spf_micro <- df_spf_micro %>%
      mutate(p = ifelse(duplicate == 2 & statas_merge == 3, 1, p))
    
    # For future events, we don't have resolution -> need to delete that resolution entry.
    df_spf_micro <- df_spf_micro %>%
      group_by(id, event, time) %>%
      mutate(grpd_id_event_time = cur_group_id(),
             # Assign a unique ID to each group
             total = sum(p, na.rm = TRUE)) %>%  # Sum p within each group
      ungroup() %>% # Remove grouping
      filter(total != 0) %>% # For ones without resolution, all p's will be 0.
      select(-grpd_id_event_time, -total)
    
    # Now get variables that are correct.
    df_spf_micro$resolution <- df_spf_micro$duplicate
    df_spf_micro <-
      select(df_spf_micro,-time_max,-duplicate,-statas_merge,-grpd_id_event)
    df_spf_micro <- df_spf_micro %>%
      arrange(id, event, time, bin) # sort
    df_spf_micro <- df_spf_micro %>%
      mutate(quarter = ifelse(resolution == 2, 5, quarter)) # Q5 for the realization.
    
    # Get resolution everywhere in an event.
    df_spf_micro <- df_spf_micro %>%
      group_by(event) %>%
      mutate(realization = mean(!!sym(pred_var), na.rm = TRUE)) %>%
      ungroup() %>%
      select(-all_of(pred_var))
    
    df_spf_micro <- df_spf_micro %>%
      arrange(id, event, time, bin) # sort
    
    # Store the dataframe in the list
    spf_ind[[pred_var]] <- df_spf_micro
    
    df_spf_agg <- df_spf_micro %>%
      group_by(time, event, bin) %>%
      mutate(grpd_time_event_bin = cur_group_id(),
             # Assign a unique ID to each group
             mean_p = mean(p, na.rm = TRUE)) %>%  # Average p over forecasters for a given
      # event @ a point in time in a certain bin.
      ungroup() # Remove grouping
    
    df_spf_agg <- df_spf_agg %>%
      select(event, time, p, resolution, everything()) %>% # Reorder the columns
      arrange(event, time) %>%
      select(-p, -id, -industry, -grpd_time_event_bin) %>% # These are the forecaster-varying variables
      rename(p = mean_p) %>%
      distinct() %>% # Drop duplicates.
      select(event, time, p, resolution, everything()) %>%
      arrange(event, time, bin)
    
    df_spf_agg <- df_spf_agg %>%
      group_by(time, event) %>%
      mutate(grpd_time_event = cur_group_id())
    
    # Store the dataframe in the list
    spf_agg[[pred_var]] <- df_spf_agg
    
    # *Data check* - should be that bin #'s are the same within an event across time.
    # Count the number of bins in each time-event group.
    grpd_data <- df_spf_agg %>%
      group_by(time, event) %>%
      summarize(bin_count = n(), .groups = 'drop') %>%
      ungroup()
    
    # Then check if the number of bins varies within each event
    event_bin_variation <- grpd_data %>%
      group_by(event) %>%
      summarize(variance_in_bins = n_distinct(bin_count),
                .groups = 'drop') %>%
      ungroup()
    
    any_varied_bins <- any(event_bin_variation$variance_in_bins > 1)
    if (any_varied_bins) {
      print("Variance in bin counts found in the following events:")
      print(event_bin_variation %>%
              filter(variance_in_bins > 1))
    }
  }
  # Return both lists as a named list
  return(list("ind" = spf_ind, "agg" = spf_agg))
}