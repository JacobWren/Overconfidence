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
    subset(read.csv(file_path), resolution != 2 & !is.na(realization))
  
  # Would be great to smooth out the person's predictions somehow => not so discrete.
  data <- data %>%
    group_by(id, time, event) %>% # For an individual @ a point in time forecasting a given event.
    mutate(grpd_id_time_event = cur_group_id()) %>%
    ungroup()
  
  ############# Continuous Choice #############
  # data <- data[data$grpd_id_time_event == 1, ] # One group for illustration.
  #
  # # Expanding each row of the dataframe by a factor of 10
  # data <- data %>%
  #   uncount(weights = 10, .id = "new")
  # # Shrinking wide bins.
  # data <- data %>%
  #   mutate(
  #     binH = ifelse(binH > 100, binL + 1, binH),
  #     binL = ifelse(binL < -100, binH - 1, binL)
  #   )
  #
  # # Make finer bins to then fit kde over; linear "finer".
  # data <- data %>%
  #   group_by(bin) %>%
  #   # Linear interpolation between binL and binH.
  #   mutate(binValue_fine = binL + (row_number() / 10) * (binH - binL)) %>%
  #   ungroup()
  #
  # data$expand <- data$p * 10
  #
  # # 'expand' specifies how many times to replicate each row
  # data_expanded <- data %>%
  #   uncount(weights = expand)
  # # Example density -> fits continuous shape over bins.
  # gp <- ggplot(data_expanded, aes(x = binValue_fine)) +
  #   stat_density(aes(y = after_stat(density)), geom = "line") +
  #   labs(title = "Normalized Kernel Density Plot of binValue",
  #        x = "binValue",
  #        y = "Density") +
  #   xlim(min(data_expanded$binValue_fine) - 3/4, max(data_expanded$binValue_fine) + 3/4)  # Extend x-axis range
  #
  # ggsave("binValue_density_plot.pdf", plot = gp)
  
  # For a given time + event, what is the expected value / variance of a given forecaster?
  data <- data %>%
    mutate(t_componentEV = p * binValue) %>% # EV
    group_by(grpd_id_time_event) %>%
    arrange(bin, .by_group = TRUE) %>%
    mutate(EV = sum(t_componentEV, na.rm = TRUE)) %>%
    ungroup()
  
  data <- data %>%
    mutate(t_componentVar = p * (binValue - EV) ^ 2) %>% # Var
    group_by(grpd_id_time_event) %>%
    mutate(VarPredictedAcross = sum(t_componentVar, na.rm = TRUE)) %>%
    ungroup()
  
  # Mode func.
  get_mode <- function(v) {
    freq <- table(na.omit(v))
    as.numeric(names(freq)[which.max(freq)])
  }
  
  # Calculate binDiff Mode.
  data <- data %>%
    mutate(binDiff = binH - binL) %>%
    group_by(grpd_id_time_event) %>%
    mutate(binDiffMode = get_mode(binDiff)) %>%
    ungroup()
  
  # Calculate Var Predicted
  # Within bin variance => all bins have the same value because uniform across them.
  data <- data %>%
    mutate(VarPredicted = VarPredictedAcross + (1 / 12) * binDiffMode)
  
  # Now, what is our observation of this guys squared error FROM REALITY
  data$sqDevFromRealization <- (data$EV - data$realization) ^ 2
  
  # Drop variables starting with t_co
  data <- data[,!grepl("^t_co", names(data))]
  
  # Now, for a given time/event, what is the disagreement (i.e., variance) between forecasters in terms of EVs?
  data <- data %>%
    arrange(id, event, time, bin) %>%
    group_by(time, event) %>% # Subsumes forecasters.
    mutate(grpd_time_event = cur_group_id(),
           meanEV = mean(EV, na.rm = TRUE)) %>%  # EV across forecasters.
    ungroup() %>%
    # Calculate the squared deviation.
    mutate(sqDevFromMeanEV = (EV - meanEV) ^ 2) %>%
    group_by(grpd_time_event) %>%
    # Compute disagreement.
    mutate(disagreementEV = mean(sqDevFromMeanEV, na.rm = TRUE)) %>%
    ungroup()
  
  # For a given event @ a point in time, how many forecasters are there?
  # This may differ by event and across time (e.g., one quarter to the next, etc.)
  # This is a running count.
  data <- data %>%
    arrange(grpd_id_time_event, bin) %>%
    group_by(grpd_id_time_event) %>%
    mutate(t_first = as.integer(row_number() == 1)) %>%
    ungroup() %>%
    arrange(grpd_time_event, id, bin) %>%
    group_by(grpd_time_event) %>%
    mutate(withinId = cumsum(t_first)) %>%
    ungroup()
  
  # Initialize disagreement for all forecasters.
  data$disagreementEVMinusPersoni <- 0
  # Largest number of forecasters for any event.
  max_withinId <- max(data$withinId, na.rm = TRUE)
  
  # Let's cycle through each forecaster, excluding one observation at a time, calculating the disagreement.
  for (i in 1:max_withinId) {
    data_grouped <- data %>%
      group_by(grpd_time_event) %>%
      filter(withinId != i) %>% # Exclude the current observation.
      summarise(
        t_meanEV = mean(EV, na.rm = TRUE),
        t_disagreementEV = mean((EV - t_meanEV) ^ 2, na.rm = TRUE)
      ) %>%
      ungroup()
    
    # Assign the calculated disagreement value.
    for (grp in unique(data$grpd_time_event)) {
      data$disagreementEVMinusPersoni[data$grpd_time_event == grp &
                                        data$withinId == i] <-
        data_grouped$t_disagreementEV[data_grouped$grpd_time_event == grp]
    }
    
    # Remove temporary variables.
    data <- data %>% select(-which(grepl("^t_", names(data))))
  }
  
  data <- data %>%
    mutate(# Time till event "resolves".
      distanceFromResolution = event + 1 - time,
      dataSet = var)
  
  data <- data %>%
    mutate(error = sqDevFromRealization - VarPredicted,
           perc_error = error / VarPredicted)
  
  # One row per forecaster event-time (they're repeated across bins).
  data <- data %>%
    distinct(grpd_id_time_event, .keep_all = TRUE)
  
  cols_to_drop <- grep("^bin|p$", names(data), value = TRUE)
  data <- data %>%
    select(-all_of(cols_to_drop))
  # Collapsed across the bins.
  save_path <-
    paste0("Data/collapsedVarInfoNotCompressed2_", var, ".csv")
  write.csv(data, file = save_path, row.names = FALSE)
  
  # Keep specific columns only.
  data_selected <- data %>%
    select(
      sqDevFromRealization,
      VarPredicted,
      disagreementEV,
      time,
      event,
      grpd_time_event,
      dataSet,
      distanceFromResolution,
      meanEV,
      realization,
      disagreementEVMinusPersoni,
      error
    )
  
  # Collapse data by event-time and average across forecasters.
  data_collapsed <- data_selected %>%
    group_by(grpd_time_event) %>%
    summarise(
      mean_sqDevFromRealization = mean(sqDevFromRealization, na.rm = TRUE),
      mean_VarPredicted = mean(VarPredicted, na.rm = TRUE),
      mean_error = mean(error, na.rm = TRUE),
      mean_disagreementEV = mean(disagreementEV, na.rm = TRUE),
      mean_time = mean(time, na.rm = TRUE),
      event = mean(event, na.rm = TRUE),
      meanEV = mean(meanEV, na.rm = TRUE),
      mean_disagreementEVMinusPersoni = mean(disagreementEVMinusPersoni, na.rm = TRUE),
      mean_realization = mean(realization, na.rm = TRUE),
      dataSet = first(dataSet)
    ) %>%
    ungroup()
  
  save_path <- paste0("Data/collapsedVarInfo2_", var, ".csv")
  write.csv(data_collapsed, file = save_path, row.names = FALSE)
}

# Combine PRGDP PRUNEMP PRCPCE PRCCPI PRPGDP
# Initialize empty data frames
combinedVarInfo <- data.frame(temp = numeric(0))
combinedVarInfoNotCompressed <- data.frame(temp = numeric(0))
combinedVarInfoWithBins <- data.frame(temp = numeric(0))

# Save initial empty data frames
write.csv(combinedVarInfo, "Data/combinedVarInfo2.csv", row.names = FALSE)
write.csv(
  combinedVarInfoNotCompressed,
  "Data/combinedVarInfoNotCompressed2.csv",
  row.names = FALSE
)
write.csv(combinedVarInfoWithBins,
          "Data/combinedVarInfoWithBins2.csv",
          row.names = FALSE)

# Iterate over each variable
for (var in vars) {
  # Append to combinedVarInfo
  temp_data <-
    read.csv(paste0("Data/collapsedVarInfo_", var, ".csv"))
  combinedVarInfo <- rbind(combinedVarInfo, temp_data)
  write.csv(combinedVarInfo, "Data/combinedVarInfo2.csv", row.names = FALSE)
  
  # Append to combinedVarInfoNotCompressed
  temp_data <-
    read.csv(paste0("Data/collapsedVarInfoNotCompressed_", var, ".csv"))
  combinedVarInfoNotCompressed <-
    rbind(combinedVarInfoNotCompressed, temp_data)
  write.csv(
    combinedVarInfoNotCompressed,
    "Data/combinedVarInfoNotCompressed2.csv",
    row.names = FALSE
  )
  
  # Append to combinedVarInfoWithBins
  temp_data <-
    read.csv(paste0("Data/SPFmicrodataCleanedWithBinValues_", var, ".csv"))
  temp_data <-
    temp_data[!temp_data$resolution == 2 &
                !is.na(temp_data$realization),]
  temp_data$dataSet <- var
  combinedVarInfoWithBins <-
    rbind(combinedVarInfoWithBins, temp_data)
  write.csv(combinedVarInfoWithBins,
            "Data/combinedVarInfoWithBins2.csv",
            row.names = FALSE)
}
