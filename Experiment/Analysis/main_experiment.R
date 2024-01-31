# Get and set working directory
if (!grepl("Experiment/Analysis", getwd())) {
  setwd("/Users/jakewren/overprecision/Experiment/Analysis")
}

source("../../libraries.R")
source("fn_clean.R")
source("fn_generate_bins.R")
# source("fn_calibration_plots.R")
source("fn_sd_slide_plots.R")

# Combine answers and solutions.
df <- fn_clean()
# Add bins.
df_binned <- fn_generate_bins(df)

# fn_calibration_plots(df_binned)

# Three sets of SD figs from slides.
fn_sd_slide_plots(df)










