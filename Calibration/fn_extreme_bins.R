# One way to show overprecision:
# (1) Pr(.) in highest bin averages >> how often bin actually occurs
# (2) “Border bins” in which people place 0% occur >> 0% of the time


fn_extreme_bins <- function(df, aggregate = FALSE) {
  # 'data_set' corresponds to a particular variable.
  grouping_vars <- c("data_set", "event", "time")
  
  # Append 'id' to the grouping variables if not aggregating
  if (!aggregate) {
    grouping_vars <- c(grouping_vars, "id")
    cluster <- "double"
    trans <- "but"
  } else {
    cluster <- "single"
    trans <- "and"
  }
  
  df <- df %>%
    group_by(across(all_of(grouping_vars))) %>%
    mutate(grpd_fvar_id_event_time = cur_group_id(),
           extreme_bin_diff = p - p_empirical) %>% # Want to get a p-value on extreme_bin_diff.
    ungroup()
  
  df_far_right <- fn_process_most_likely_bin(df, cluster, trans)
  df_border_bins <- fn_process_border_bins(df, cluster, trans)
}


fn_process_most_likely_bin <- function(df, cluster, trans) {
  df_far_right <- df %>%
    group_by(grpd_fvar_id_event_time) %>%
    # Calculate the maximum perceived probability within each group (^).
    mutate(high = max(p, na.rm = TRUE)) %>%
    ungroup() %>%
    # Keep only rows with maximum perceived probability.
    filter(p == high) %>%
    select(-high)
  
  most_likely_bin_p_value <-
    fn_regress(df_far_right,
               calibration = TRUE,
               extreme_bins = TRUE, 
               cluster = cluster)
  
  # Probability in highest bin averages 52.2% but bin actually occurs 38.1% of the time (p<0.001).
  message <- df_far_right %>%
    summarize(
      avg_perceived = mean(p, na.rm = TRUE),
      avg_reality = mean(p_empirical, na.rm = TRUE)
    ) %>%
    {
      paste0(
        "Forecasters place ",
        percent(.$avg_perceived, accuracy = 0.1),
        " probability in their most likely bin on average ", trans, " these bins are realized only ",
        percent(.$avg_reality, accuracy = 0.1),
        " of the time (p-value of equality: ",
        round(most_likely_bin_p_value, 6),
        ")."
      )
    } %>%
    print()
  
  return(message)
}


fn_process_border_bins <- function(df, cluster, trans) {
  # Next, ``border bins'': bins that forecasters predict are impossible, but are next to bins that are believed to
  # be possible.
  # Task: Identify zeros (i.e., where the density projection is flat) just outside of a person's positive beliefs.
  # Mark adjacent bins.
  df_border_bins <- df %>%
    arrange(grpd_fvar_id_event_time, bin) %>%
    mutate(p_next = lead(p, default = 0),
           p_prev = lag(p, default = 0))
  
  # Now, mark the beginning and end of "sequences" with non-zero probability.
  df_border_bins <- df_border_bins %>%
    group_by(grpd_fvar_id_event_time) %>%
    mutate(
      start_positive_seq = p > 0 &
        p_prev == 0,
      # True for the start of a positive sequence
      end_positive_seq = p > 0 &
        p_next == 0     # True for the end of a positive sequence
    ) %>%
    ungroup()
  
  # Lastly, filter for rows with flat density projections that directly precedes the start or follows the end of
  # a positive probability sequence.
  df_border_bins <- df_border_bins %>%
    filter(p == 0 &
             (
               lag(end_positive_seq, default = FALSE) |
                 lead(start_positive_seq, default = FALSE)
             )) %>%
    select(-p_next,-p_prev,-start_positive_seq,-end_positive_seq)
  
  border_bin_p_value <-
    fn_regress(df_border_bins,
               calibration = TRUE,
               extreme_bins = TRUE,
               cluster = cluster)
  
  message <- df_border_bins %>%
    summarize(avg_reality = mean(p_empirical, na.rm = TRUE)) %>%
    {
      paste0(
        "The outcome actually occurs ",
        percent(.$avg_reality, accuracy = 0.1),
        " of the time ", trans, " they were predicted to be impossible",
        " (p-value of equality: ",
        round(border_bin_p_value, 6),
        ")."
      )
    } %>%
    print()
  
  return(message)
}
