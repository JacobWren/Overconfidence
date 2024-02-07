# Get and set working directory
if (!grepl("Experiment", getwd())) {
  setwd("/Users/jakewren/overprecision/Experiment")
}

source("../libraries.R")
source("fn_clean.R")
source("fn_sd_slide_plots.R")
source("fn_generate_bins.R")
source("../Helpers/fn_aggregate.R")
source("fn_calibration_analysis.R")
source("fn_regression_analysis.R")
source("fn_summary_figs.R")


# Combine answers and solutions.
df <- fn_clean()

# Three sets of SD figs from slides.
fn_sd_slide_plots(df)

# Add bins.
df_binned <- fn_generate_bins(df)

# Aggregate.
df_binned_agg <- fn_aggregate(df_binned, case = "Experiment", to_drop = c("pic_order", "p_true", "first"))

# Calibration scatter plots.
fn_calibration_analysis(df_binned, df_binned_agg) 

# Regressions.
fn_regression_analysis(df)

# All figures regarding the experiment in the paper and slides.
fn_summary_figs(df)
