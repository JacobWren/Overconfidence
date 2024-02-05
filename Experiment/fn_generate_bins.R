# Create bins from 0 - 200 by 10 with corresponding bin probability for both:
# (1) the truth and (2) the participant, for each figure.
# Notice the bin range (^), and consider that each participant was only exposed to the range: 60 - 140.
# But given normality, we don't want to ignore bins with non-trivial probability.


fn_generate_bins <- function(df) {
  # Define bin edges and create a dataframe of stacked bins (this just sets things up).
  bin_edges <- seq(0, 200, by = 10)
  bins_df <-
    tibble(bin_start = bin_edges[-length(bin_edges)], bin_end = bin_edges[-1] + 10)
  bins_df$bin_end <- bins_df$bin_start + 10
  
  # Expand the dataframe and compute probabilities.
  expanded_df <- df %>%
    crossing(bins_df) %>%
    mutate(
      # Bin probability: for truth and for perceived or estimated.
      p_true = pnorm(bin_end, mean_true, sd_true) - pnorm(bin_start, mean_true, sd_true),
      p = pnorm(bin_end, mean, sd) - pnorm(bin_start, mean, sd)
    ) %>%
    group_by(id, pic) %>%
    mutate(bin = row_number()) %>%
    ungroup() 
  
  return(expanded_df)
}
