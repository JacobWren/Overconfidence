# Get and set working directory
current_dir <- getwd()
if (!grepl("SPF$", current_dir)) {
  setwd("SPF")
}
source("init_script.R")
source("fn_bin_settings.R")

# Want a dataset that looks like:
# event bin numBins prob timeToEnd

for (var in vars) {
  file_path <- paste0("Data/SPFmicrodataCleaned_", var, ".csv")
  data <- read.csv(file_path)
  
  data$binL <- NA # Initialize.
  data$binH <- NA
  
  if (var == "RECESS") {
    data$binL <- 1
    data$binH <- 1
  }
  else {
    # Grab the appropriate bin settings.
    bin_idx_years <- fn_idx_year_cutoffs(var)
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
             var == "PRPGDP") |
            (var == "PRCCPI" | var == "PRCPCE")) {
          # First pass.
          condition <- data$bin == x
        }
        # Now make corrections.
        else if (typeof(year_threshold) == "double") {
          if (!is.list(ops)) {
            condition <- data$bin == x &
              ops(data$event, year_threshold)
          }
          else {
            # Use first of two ops.
            condition <- data$bin == x &
              ops[[1]](data$event, year_threshold)
          }
        }
        else if (is.list(year_threshold)) {
          condition <- data$bin == x &
            (data$event >= year_threshold[[1]]) &
            (data$event < year_threshold[[2]])
        }
        data$binH[condition] <- bins[x]
        data$binL[condition] <- bins[x + 1]
      }
    }
  }
  
  # Sanity check :)
  # Do the realizations fall within the bins?
  data$incorrect <-
    with(data,
         ifelse(p == 1 &
                  resolution == 2, realization > binH |
                  realization < binL, NA))
  n_incorrect <- sum(data$incorrect, na.rm = TRUE)
  if (n_incorrect > 0) {
    print(paste("There are:", n_incorrect, "errors.")) # :(
  }
  data <- data[, -which(names(data) == "incorrect")]
  
  data$binValue <-
    0.5 * data$binH + 0.5 * data$binL # Equally weighted average.
  data$binValue[data$binH > 100] <- data$binL[data$binH > 100] + 1
  data$binValue[data$binL < -100] <- data$binH[data$binL < -100] - 1
  
  # Save the Data
  file_path <-
    paste0("Data/SPFmicrodataCleanedWithBinValues_", var, ".csv")
  write.csv(data, file_path, row.names = FALSE)
}
