# Get and set working directory
current_dir <- getwd()
if (!grepl("SPF$", current_dir)) {
  setwd("SPF")
}

source("init_script.R")

# Want a dataset that looks like:
# event bin numBins prob timeToEnd
vars <- vars[-length(vars)]
for (var in vars) {
  # Exclude 'RECESS'.
  file_path <- # Data file to 'iterate' off of.
    paste0("Data/SPFmicrodataCleanedWithBinValues_", var, ".csv")
  data <- read.csv(file_path)
  # First filter: Here, everyone is forced to agree -> not interesting.
  # Second filter: We don't have realization yet for future.
  data <-
    subset(read.csv(file_path), resolution != 2 &
             !is.na(realization))
  
  data <- data %>%
    group_by(event, time, bin) %>%
    mutate(grpd_event_time_bin = cur_group_id()) %>%
    # Get average probabilities over event/time/bin.
    mutate(pAgg = mean(p, na.rm = TRUE)) %>%
    ungroup() %>%
    # Drop individual-level stuff.
    select(-id, -industry, -p) %>%
    distinct() %>% # Remove duplicate rows.
    arrange(event, time, bin) %>%
    mutate(dataSet = var)
  
  write.csv(
    data,
    paste0("Data/SPFmicrodataCleanedWithBinValuesAgg_", var, ".csv"),
    row.names = FALSE
  )
  
  # Sanity check: probability adds up to 1.
  data <- data %>%
    group_by(time, event) %>%
    mutate(shouldBe1 = sum(pAgg, na.rm = TRUE)) %>%
    ungroup()
  tolerance = 0.00199
  prob_check <- all(abs(data$shouldBe1 - 1) < tolerance)
  if (!prob_check) {
    print(var)
    print("Probabilities don't add to 1")
  }
  # stopifnot(all(abs(data$shouldBe1 - 1) < tolerance))
  
  data <- data %>%
    # For a given event-time, what is the expected value?
    mutate(t_componentEV = pAgg * binValue) %>%
    # Group by time and event
    group_by(time, event) %>%
    mutate(grpd_time_event = cur_group_id()) %>%
    # Sort by bin within an event-time.
    arrange(bin, .by_group = TRUE) %>%
    mutate(EV = sum(t_componentEV, na.rm = TRUE)) %>%
    # For a given event-time, what is the variance of the aggregate?
    mutate(t_componentVar = pAgg * (binValue - EV) ^ 2) %>%
    mutate(VarPredictedAgg = sum(t_componentVar, na.rm = TRUE)) %>%
    ungroup()
  
  # Now, what is our observation of this "aggregate" forecasters squared error FROM REALITY
  data <- data %>%
    mutate(sqDevFromRealizationAgg = (EV - realization) ^ 2) %>%
    # Drop columns that start with t_co
    select(-starts_with("t_co"), -shouldBe1)
  
  data <- data %>%
    mutate(errorAgg = sqDevFromRealizationAgg - VarPredictedAgg) %>%
    mutate(perc_errorAgg = errorAgg / VarPredictedAgg) %>%
    # One row per event-time (they're repeated across bins).
    distinct(grpd_time_event, .keep_all = TRUE) %>% # ".keep_all = TRUE" ensures that all other columns are retained
    # Drop columns that start with bin and pAgg
    select(-starts_with("bin"), -starts_with("pAgg"))
  
  # Let's get the measure of disagreement -> this is only accessible from the individual level data.
  collapsed_var_info <-
    read.csv(paste0("Data/collapsedVarInfo_", var, ".csv")) # Load the dataset to be merged.
  
  # Merge the datasets on event-time appending disagreement only.
  data <- data %>%
    left_join(
      collapsed_var_info %>% select(event, time, avg_disagreementEV),
      by = c("event", "time")
    ) %>%
    arrange(event, time)
  
  write.csv(data,
            paste0("Data/collapsedVarInfoAgg_", var, ".csv"),
            row.names = FALSE)
  
  # Run the regressions:
  # Regression of error on a constant, with standard errors clustered by event.
  model1 <- lm_robust(errorAgg ~ 1,
                      se_type = "stata",
                      clusters = event,
                      data = data)
  # Regression of error on expected disagreement without an intercept, again with standard errors clustered by event.
  model2 <- lm_robust(
    errorAgg ~ avg_disagreementEV - 1,
    se_type = "stata",
    clusters = event,
    data = data
  )
  # View model results
  # Clustered robust standard errors
  cat(
    "************************************",
    var,
    "************************************",
    "\n"
  )
  options(digits = 7)
  print(model1)
  print(model2)
  cat("\n")
}

# Combine PRGDP PRUNEMP PRCPCE PRCCPI PRPGDP
# Initialize empty data frames
combinedVarInfoAgg <- data.frame(temp = numeric(0))
combinedVarInfoAggWithBins <- data.frame(temp = numeric(0))

# Save initial empty data frames
write.csv(combinedVarInfoAgg,
          "Data/combinedVarInfoAgg.csv",
          row.names = FALSE)
write.csv(combinedVarInfoAggWithBins,
          "Data/combinedVarInfoAggWithBins.csv",
          row.names = FALSE)

# Iterate over each variable
for (var in vars) {
  # Append to combinedVarInfo
  temp_data <-
    read.csv(paste0("Data/collapsedVarInfoAgg_", var, ".csv"))
  combinedVarInfoAgg <- rbind(combinedVarInfoAgg, temp_data)
  write.csv(combinedVarInfoAgg,
            "Data/combinedVarInfoAgg.csv",
            row.names = FALSE)
  
  # Append to combinedVarInfoNotCompressed
  temp_data <-
    read.csv(paste0("Data/SPFmicrodataCleanedWithBinValuesAgg_", var, ".csv"))
  combinedVarInfoAggWithBins <-
    rbind(combinedVarInfoAggWithBins, temp_data)
  write.csv(combinedVarInfoAggWithBins,
            "Data/combinedVarInfoAggWithBins.csv",
            row.names = FALSE)
}
