# Create bins from 0 - 200 by 10 with corresponding bin probability (we know the truth).
fn_generate_bins <- function(df) {
  # Define bin edges and create a dataframe of stacked bins
  bin_edges <- seq(0, 200, by = 10)
  bins_df <- tibble(bin_start = bin_edges[-length(bin_edges)], bin_end = bin_edges[-1] + 10)
  bins_df$bin_end <- bins_df$bin_start + 10
  
  # Expand the dataframe and compute probabilities.
  expanded_df <- df %>%
    crossing(bins_df) %>%
    mutate(
      true_bin = mean_true >= bin_start & mean_true < bin_end,
      # Bin probability: for truth and for perceived or estimated.
      p_true = pnorm(bin_end, mean_true, sd_true) - pnorm(bin_start, mean_true, sd_true),
      p_perceive = pnorm(bin_end, mean, sd) - pnorm(bin_start, mean, sd)
    ) %>%
    group_by(id, pic) %>%
    mutate(bin = row_number()) %>%
    ungroup()
  
  return(expanded_df)
}
