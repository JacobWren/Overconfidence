source("fn_bin_settings.R")

# Want a dataset that looks like:
# event bin numBins prob timeToEnd

fn_bin_end_points <- function(spf_micro_ind, pred_vars) {
  # Initialize an empty list to store data frames by forecasting variable.
  binned_nums_data <- list()
  
  for (pred_var in pred_vars) {
    spf_data <- spf_micro_ind[[pred_var]]
    
    spf_data$bin_l <- NA # Initialize.
    spf_data$bin_h <- NA
    
    if (pred_var == "RECESS") {
      spf_data$bin_l <- 1
      spf_data$bin_h <- 1
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
            condition <- spf_data$bin == x
          }
          # Now make corrections.
          else if (typeof(year_threshold) == "double") {
            if (!is.list(ops)) {
              condition <- spf_data$bin == x &
                ops(spf_data$event, year_threshold)
            }
            else {
              # Use first of two ops.
              condition <- spf_data$bin == x &
                ops[[1]](spf_data$event, year_threshold)
            }
          }
          else if (is.list(year_threshold)) {
            condition <- spf_data$bin == x &
              (spf_data$event >= year_threshold[[1]]) &
              (spf_data$event < year_threshold[[2]])
          }
          spf_data$bin_h[condition] <- bins[x]
          spf_data$bin_l[condition] <- bins[x + 1]
        }
      }
    }
    
    # Sanity check :)
    # Do the realizations fall within the bins?
    spf_data$incorrect <-
      with(spf_data,
           ifelse(p == 1 &
                    resolution == 2, realization > bin_h |
                    realization < bin_l, NA))
    n_incorrect <- sum(spf_data$incorrect, na.rm = TRUE)
    if (n_incorrect > 0) {
      print(paste("There are:", n_incorrect, "errors.")) # :(
    }
    spf_data <- spf_data[,-which(names(spf_data) == "incorrect")]
    
    spf_data$bin_value <-
      0.5 * spf_data$bin_h + 0.5 * spf_data$bin_l # Equally weighted average.
    spf_data$bin_value[spf_data$bin_h > 100] <- spf_data$bin_l[spf_data$bin_h > 100] + 1
    spf_data$bin_value[spf_data$bin_l < -100] <- spf_data$bin_h[spf_data$bin_l < -100] - 1
    
    # # Save the Data
    # file_path <-
    #   paste0("Data/SPFmicrodataCleanedWithBinValues_", pred_var, ".csv")
    # write.csv(spf_data, file_path, row.names = FALSE)
    
    # First filter: Here, everyone is forced to agree -> not interesting.
    # Second filter: We don't have realization yet for future.
    spf_data <-
      subset(spf_data, resolution != 2 &
               !is.na(realization))
    
    binned_nums_data[[pred_var]] <- spf_data
  }
  return(binned_nums_data)
}
