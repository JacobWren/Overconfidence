# Get and set working directory
current_dir <- getwd()
if (!grepl("SPF$", current_dir)) {
  setwd("/Users/jakewren/overprecision/SPF")
}
# Load libraries.
source("../libraries.R")

# Main files.
source("fn_realized_outcomes_to_bins.R")
source("fn_clean.R")
source("fn_add_outcomes.R")
source("fn_bin_end_points.R")
source("fn_ind_computations.R")
source("fn_agg_computations.R")
source("fn_analysis.R")

realized_outcomes_file_path <-
  "RawData/RealizedOutcomes.xlsx" # 1 sheet per variable.
named_excel_sheets <- excel_sheets(realized_outcomes_file_path)
# Outcomes to bins: e.g., for PRGDP in 2002 of 1.7, that maps to bin 6.
realized_outcomes <-
  fn_realized_outcomes_to_bins(named_excel_sheets, realized_outcomes_file_path)

# The survey’s six probability variables.
vars_to_forecast <-
  c("PRPGDP", "PRCCPI", "PRCPCE", "PRUNEMP", "PRGDP", "RECESS")

SPF_microdata_file_path <- "RawData/SPFmicrodata.xlsx"
# Manual adjustments in accordance with documentation.
spf_cleaned <- fn_clean(SPF_microdata_file_path, vars_to_forecast)

# Merge in realizations and aggregate.
df_spf <-
  fn_add_outcomes(realized_outcomes, spf_cleaned, vars_to_forecast)

df_spf_ind <- df_spf$ind # Individual forecaster
df_spf_agg <- df_spf$agg # Average forecaster

# Bin intervals.
ind_spf_micro_bin_nums <-
  fn_bin_end_points(df_spf_ind, vars_to_forecast)

# Exclude 'RECESS'.
vars_to_forecast <- vars_to_forecast[-length(vars_to_forecast)]

# Compute variance error, disagreement, etc. for each forecaster.
ind_data <-
  fn_ind_computations(ind_spf_micro_bin_nums, vars_to_forecast, smooth = FALSE)

# grpd_event_time_bin -> aggregate_forecaster
# Compute variance error, disagreement, etc. for the "average" forecaster.
agg_data <-
  fn_agg_computations(ind_spf_micro_bin_nums,
                      vars_to_forecast,
                      ind_data$spf_ind_collapsed_bin_agg)

# Analysis
fn_analysis(
  ind_data$spf_micro_ind_all_vars,
  agg_data$all_vars_agg_with_bins,
  ind_data$spf_ind_collapsed_bin_all_vars
)


# ind_data$spf_ind_collapsed_bin_all_vars_agg: => Not really used => all averages compressed over
# event-time-forecasting variable

# agg_data$all_vars_agg => compressed over bins + has statistics like sq error, disagreement
