# Get and set working directory
current_dir <- getwd()
if (!grepl("SPF$", current_dir)) {
  setwd("SPF")
}
source("init_script.R")

source("fn_realized_outcomes_to_bins.R")
source("fn_clean.R")
source("fn_bin_end_points.R")
source("fn_disagreement_mse.R")

realized_outcomes_file_path <- "RawData/RealizedOutcomes.xlsx" # 1 sheet per variable.
named_excel_sheets <- excel_sheets(realized_outcomes_file_path)
# Outcomes to bins: e.g., for PRGDP in 2002 of 1.7, that maps to bin 6.
binned_realized_outcomes <- fn_realized_outcomes_to_bins(named_excel_sheets, realized_outcomes_file_path)

SPFmicrodata_file_path <- "RawData/SPFmicrodata.xlsx"
# Manual adjustments in accordance with documentation, restructuring and aggregating data.
spf_micro_data <- fn_clean(SPFmicrodata_file_path, binned_realized_outcomes)
ind_spf_micro <- spf_micro_data$Ind # Individual forecaster
agg_spf_micro <- spf_micro_data$Agg # Average forecaster

# Bin intervals.
ind_spf_micro_bin_nums <- fn_bin_end_points(ind_spf_micro) # paste0("Data2/SPFmicrodataCleanedWithBinValues_", var, ".csv")

# Compute disagreement
disagreement <- fn_disagreement_mse(ind_spf_micro_bin_nums, bin_diff_stat = "mode") # mode/mean
# temp <- disagreement$spf_ind_collapsed_bin # paste0("Data2/collapsedVarInfoNotCompressed_", var, ".csv")
# temp <- disagreement$spf_ind_collapsed_bin_agg # paste0("Data2/collapsedVarInfo_", var, ".csv")
# temp <- disagreement$spf_ind_collapsed_bin_all_vars # combinedVarInfoNotCompressed
# temp <- disagreement$spf_ind_collapsed_bin_all_vars_agg # combinedVarInfo
#temp <- disagreement$spf_micro_ind_all_vars # combinedVarInfoWithBins

# path <-
#   paste0("Data2/combinedVarInfoWithBins.csv")
# data <- read.csv(path)
# 
# # Check for equality of data frame columns.
# col_names_equal <- identical(names(data), names(temp))
# if (!col_names_equal) {
#   print("**************************")
#   print("Column names are not equal")
#   print(var)
# }
# 
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
#   # Check for equality of data frame columns.
#   col_names_equal <- identical(names(data), names(temp[[var]]))
#   if (!col_names_equal) {
#     print("**************************")
#     print("Column names are not equal")
#     print(var)
#   }
# 
#   # Use all.equal to check for equality while ignoring column attributes
#   eq <- all.equal(data, temp[[var]], check.attributes = FALSE)
#   if (!isTRUE(eq)){
#     print(var)
#     print(eq)
#   }
# }
