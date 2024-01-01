source("init_script.R")

# Data Description
data <- read.csv("Data/combinedVarInfoWithBins.csv")

data <- data %>%
  group_by(event, dataSet) %>%
  mutate(grpd_event_dataSet = cur_group_id()) %>%
  ungroup() %>%
  
  group_by(event, time, dataSet) %>%
  mutate(grpd_event_time_dataSet = cur_group_id()) %>%
  ungroup() %>%
  
  group_by(id, event, time, dataSet) %>%
  mutate(grpd_id_event_time_dataSet = cur_group_id()) %>%
  ungroup()

# Calculate various statistics
NYears <- n_distinct(data$event)
NEvents <- n_distinct(data$grpd_event_dataSet)
NEventTimes <- n_distinct(data$grpd_event_time_dataSet)
NIds <- n_distinct(data$id)
NPredictionIds <- n_distinct(data$grpd_id_event_time_dataSet)
NAll <- nrow(data)

# Display the summary
cat("****************\n")
cat("******** Data Description\n")
cat("****************\n\n")
cat("The longest running survey contains data for", NYears, "years.\n")
cat(
  "There are 5 surveys for different variables (which have been run for different lengths of time), so the total
    number of predicted events is",
  NEvents,
  ".\n"
)
cat(
  "Predictions for each event occur multiple times as the event approaches (which varies over the predicted
    variable and over time), for a total of",
  NEventTimes,
  "prediction points.\n"
)
cat(
  "Each prediction point contains predictions made by a subset of",
  NIds,
  "participants (which varies over the
    predicted variable and over time), for a total of",
  NPredictionIds,
  "participant-prediction-points.\n"
)
cat(
  "Each prediction consists of a set of bins (which vary over the predicted variable and over time), for a
    total number of binary probabilistic predictions of",
  NAll,
  "\n"
)
