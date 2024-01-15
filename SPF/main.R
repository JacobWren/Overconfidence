# Get and set working directory
current_dir <- getwd()
if (!grepl("SPF$", current_dir)) {
  setwd("SPF")
}
source("load_libs.R")

# Main files.
source("fn_realized_outcomes_to_bins.R")
source("fn_clean.R")
source("fn_bin_end_points.R") 
source("fn_disagreement_mse.R") 
source("fn_construct_agg_data.R") 
source("fn_analysis.R") 

realized_outcomes_file_path <- "RawData/RealizedOutcomes.xlsx" # 1 sheet per variable.
named_excel_sheets <- excel_sheets(realized_outcomes_file_path)
# Outcomes to bins: e.g., for PRGDP in 2002 of 1.7, that maps to bin 6.
binned_realized_outcomes <- fn_realized_outcomes_to_bins(named_excel_sheets, realized_outcomes_file_path)

# The survey’s six probability variables.
vars_to_forecast <-
  c("PRPGDP", "PRCCPI", "PRCPCE", "PRUNEMP", "PRGDP", "RECESS")

SPFmicrodata_file_path <- "RawData/SPFmicrodata.xlsx"
# Manual adjustments in accordance with documentation, restructuring and aggregating data.
spf_micro_data <- fn_clean(SPFmicrodata_file_path, binned_realized_outcomes, vars_to_forecast)
ind_spf_micro <- spf_micro_data$ind # Individual forecaster
agg_spf_micro <- spf_micro_data$agg # Average forecaster

# Bin intervals.
ind_spf_micro_bin_nums <- fn_bin_end_points(ind_spf_micro, vars_to_forecast) # paste0("Data2/SPFmicrodataCleanedWithBinValues_", var, ".csv")

vars_to_forecast <- vars_to_forecast[-length(vars_to_forecast)] # Exclude 'RECESS'.

# Compute disagreement
disagreement <- fn_disagreement_mse(ind_spf_micro_bin_nums, vars_to_forecast, bin_diff_stat = "mode", smooth = FALSE)

# Construct aggregate data.
agg_data <- fn_construct_agg_data(ind_spf_micro_bin_nums, vars_to_forecast, disagreement$spf_ind_collapsed_bin_agg)

# Analysis
fn_analysis(disagreement$spf_micro_ind_all_vars, agg_data$all_vars_agg_with_bins, 
            disagreement$spf_ind_collapsed_bin_all_vars)


# NOTES START #
# disagreement$spf_ind_collapsed_bin_all_vars_agg: => Not really used => all averages compressed over 
# event-time-forecasting variable

# agg_data$all_vars_agg => compressed over bins + has statistics like sq error, disagreement
# NOTES END #

# Checking results after refactoring #
# temp <- disagreement$spf_ind_collapsed_bin # paste0("Data2/collapsedVarInfoNotCompressed_", var, ".csv")
# temp <- disagreement$spf_ind_collapsed_bin_agg # paste0("Data2/collapsedVarInfo_", var, ".csv")
# temp <- disagreement$spf_ind_collapsed_bin_all_vars # paste0("Data2/combinedVarInfoNotCompressed)
# temp <- disagreement$spf_ind_collapsed_bin_all_vars_agg # paste0("Data2/combinedVarInfo)
# temp <- disagreement$spf_micro_ind_all_vars # paste0("Data2/combinedVarInfoWithBins)

# temp <- agg_data$all_vars_agg_with_bins # paste0("Data2/combinedVarInfoAggWithBins.csv")  
# temp <- agg_data$all_vars_agg # paste0("Data2/combinedVarInfoAgg.csv")

# path <-
#   paste0("Data2/combinedVarInfoAggWithBins.csv")
# data <- read.csv(path)
# browser()
# # # Check for equality of data frame columns.
# # col_names_equal <- identical(names(data), names(temp))
# # if (!col_names_equal) {
# #   print("**************************")
# #   print("Column names are not equal")
# #   print(var)
# # }
# # temp <- temp %>%
# #   select(-common_bin_diff)
# # Use all.equal to check for equality while ignoring column attributes
# eq <- all.equal(data, temp, check.attributes = FALSE)
# if (!isTRUE(eq)){
#   print(var)
#   print(eq)
# }


# for (var in vars_to_forecast[-length(vars_to_forecast)]) {
#   path <-
#     paste0("Data2/collapsedVarInfo_", var, ".csv")
#   data <- read.csv(path)
#   
#   temp2 <- temp[[var]] 
#   # %>%
#   #   select(-common_bin_diff)
#   
#   # browser()
# 
#   # # Check for equality of data frame columns.
#   # col_names_equal <- identical(names(data), names(temp2))
#   # if (!col_names_equal) {
#   #   print("**************************")
#   #   print("Column names are not equal")
#   #   print(var)
#   # }
# 
#   # Use all.equal to check for equality while ignoring column attributes
#   eq <- all.equal(data, temp2, check.attributes = FALSE)
#   if (!isTRUE(eq)){
#     print(var)
#     print(eq)
#   }
# }
