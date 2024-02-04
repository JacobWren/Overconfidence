# Get and set working directory
if (!grepl("Experiment/Analysis", getwd())) {
  setwd("/Users/jakewren/overprecision/Experiment/Analysis")
}

source("../../libraries.R")
source("fn_clean.R")
source("fn_generate_bins.R")
# source("fn_calibration_plots.R")
source("fn_sd_slide_plots.R")
source("fn_analysis.R")
source("fn_summary_figs.R")

# Combine answers and solutions.
df <- fn_clean()

# Three sets of SD figs from slides.
fn_sd_slide_plots(df)

# Add bins.
df_binned <- fn_generate_bins(df)

fn_calibration_plots(df_binned)

# Analysis.
fn_analysis(df)

# All figures regarding the experiment in the paper and slides.
fn_summary_figs(df)

# Reformat, Push, Refactor, Add in Neds additions...

