source("fn_bin_settings.R")

# Map bin numbers to bin endpoints.


fn_bin_end_points <- function(spf_micro_ind, pred_vars) {
  # Initialize an empty list to store data frames by forecasting variable.
  binned_nums_data <- list()
  
  for (pred_var in pred_vars) {
    df_spf_bins <- spf_micro_ind[[pred_var]]
    
    df_spf_bins$bin_l <- NA # Initialize.
    df_spf_bins$bin_h <- NA
    
    if (pred_var == "RECESS") {
      df_spf_bins$bin_l <- 1
      df_spf_bins$bin_h <- 1
    }
    else {
      # Grab the appropriate bin settings.
      bin_idx_years <- fn_idx_year_cutoffs(pred_var)
      idx <- bin_idx_years[[1]]
      years <- bin_idx_years[[2]]
      
      bin_settings <- fn_bin_limits_time_bounds(idx)
      bin_limits <- bin_settings[[1]]
      bin_ops <- bin_settings[[2]]
      # Mapping bin #'s (not so meaningful) to bin bound points (very meaningful).
      for (j in seq_along(bin_limits)) {
        bins <- bin_limits[[j]]
        year_threshold <- years[[j]]
        ops <- bin_ops[[j]]
        n_bins = length(bins) - 1
        for (x in 1:n_bins) {
          if ((j == 1 &
               pred_var == "PRPGDP") |
              (pred_var == "PRCCPI" | pred_var == "PRCPCE")) {
            # First pass.
            condition <- df_spf_bins$bin == x
          }
          # Now make corrections.
          else if (typeof(year_threshold) == "double") {
            if (!is.list(ops)) {
              condition <- df_spf_bins$bin == x &
                ops(df_spf_bins$event, year_threshold)
            }
            else {
              # Use first of two ops.
              condition <- df_spf_bins$bin == x &
                ops[[1]](df_spf_bins$event, year_threshold)
            }
          }
          else if (is.list(year_threshold)) {
            condition <- df_spf_bins$bin == x &
              (df_spf_bins$event >= year_threshold[[1]]) &
              (df_spf_bins$event < year_threshold[[2]])
          }
          df_spf_bins$bin_h[condition] <- bins[x]
          df_spf_bins$bin_l[condition] <- bins[x + 1]
        }
      }
    }
    
    # Sanity check :)
    # Do the realizations fall within the bins?
    df_spf_bins$incorrect <-
      with(
        df_spf_bins,
        ifelse(
          p == 1 &
            resolution == 2,
          realization > bin_h |
            realization < bin_l,
          NA
        )
      )
    n_incorrect <- sum(df_spf_bins$incorrect, na.rm = TRUE)
    if (n_incorrect > 0) {
      print(paste("There are:", n_incorrect, "errors.")) # :(
    }
    df_spf_bins <-
      df_spf_bins[, -which(names(df_spf_bins) == "incorrect")]
    
    df_spf_bins$bin_value <-
      0.5 * df_spf_bins$bin_h + 0.5 * df_spf_bins$bin_l # Equally weighted average.
    df_spf_bins$bin_value[df_spf_bins$bin_h > 100] <-
      df_spf_bins$bin_l[df_spf_bins$bin_h > 100] + 1
    df_spf_bins$bin_value[df_spf_bins$bin_l < -100] <-
      df_spf_bins$bin_h[df_spf_bins$bin_l < -100] - 1
    
    # First filter: Here, everyone is forced to agree -> not interesting.
    # Second filter: We don't have realization yet for future.
    df_spf_bins <-
      subset(df_spf_bins, resolution != 2 &
               !is.na(realization))
    
    binned_nums_data[[pred_var]] <- df_spf_bins
  }
  return(binned_nums_data)
}
