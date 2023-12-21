library(ggplot2)

source("init_script.R")

# Want a dataset that looks like:
# event bin numBins prob timeToEnd

for (var in vars[1]) {
  file_path <- paste0("Data/SPFmicrodataCleanedWithBinValues_", var, ".csv")
  data <- read.csv(file_path)
  # Here, everyone is forced to agree -> not interesting.
  data <- data[data$resolution != 2, ]
  # We don't have realization yet for future.
  data <- data[!is.na(data$realization), ]
  
  # Would be great to smooth out the person's predictions somehow => not so chunky
  data <- data %>%
    group_by(id, time, event) %>% # For a person @ a point in time forecasting a given event.
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
  
  # For a given time + event, what is the expected value / variance of a forecaster?
  # EV
  data$time_componentEV <- data$p * data$binValue
  data <- data %>%
    group_by(grpd_id_time_event) %>%
    arrange(bin) %>%  # Sort by 'bin'
    mutate(EV = sum(time_componentEV, na.rm = TRUE)) %>%
    ungroup()
  
  # Var
  data <- data %>%
    mutate(time_componentVar = p * (binValue - EV)^2) %>%
    group_by(grpd_id_time_event) %>%
    mutate(VarPredictedAcross = sum(time_componentVar, na.rm = TRUE)) %>%
    ungroup()
  
  data <- data %>%
    mutate(binDiff = binH - binL)
  # Mode func.
  get_mode <- function(v) {
    freq <- table(na.omit(v))
    as.numeric(names(freq)[which.max(freq)])
  }
  
  # Calculate binDiff Mode 
  data <- data %>%
    group_by(grpd_id_time_event) %>%
    mutate(binDiffMode = get_mode(binDiff)) %>%
    ungroup()
  
  # Calculate Var Predicted
  # Within bin variance => all bins have the same value because uniform across them.
  data <- data %>%
    mutate(VarPredicted = VarPredictedAcross + (1/12) * binDiffMode)
  
  # Now, what is our observation of this guys squared error FROM REALITY
  data$sqDevFromRealization <- (data$EV - data$realization)^2
  
  # Drop variables starting with t_co
  data <- data[, !grepl("^t_co", names(data))]
  
  # Sort the data by id, event, time, and bin
  data <- data %>%
    arrange(id, event, time, bin)
  
  # Now, for a given time/event, what is the disagreement (i.e., variance) between forecasters in terms of EVs?
  data <- data %>%
    group_by(time, event) %>% # Subsumes forecasters.
    mutate(grpd_time_event = cur_group_id(), 
           meanEV = mean(EV, na.rm = TRUE)) %>%  # EV across forecasters.
    ungroup() %>%
    # Calculate the squared deviation.
    mutate(sqDevFromMeanEV = (EV - meanEV)^2) %>%
    group_by(grpd_time_event) %>%
    # Compute disagreement.
    mutate(disagreementEV = mean(sqDevFromMeanEV, na.rm = TRUE)) %>%
    ungroup()
}








