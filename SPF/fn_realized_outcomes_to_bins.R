source("fn_bin_settings.R")
source("fn_binning.R")

# Maps observable outcomes to bins across years and forecasting variables.
# Data structure: one observation per event, where an event is a point in time, e.g., 1983.75 is Q3 of 1983.
# Columns are time (usually "year" alone), variable (e.g., recession?), event, and bin.

fn_quick_tidy <- function(df, pred_var) {
  # Cleaning helper.
  df$binOutcome <- NA  # Initialize binOutcome with NAs
  
  # Reorder columns
  cols <- names(df)
  new_order <-
    c(cols[cols != "binOutcome" &
             cols != pred_var], "binOutcome", pred_var)
  df <- df[new_order]
  df <- df %>% rename(year = "Year (Q4)")  # Rename YearQ4 to year
  return(df)
}


fn_realized_outcomes_to_bins <- function(sheet_names, data_path) {
  # Initialize an empty list to store data frames by forecasting variable.
  binned_data <- list()
  
  for (forecast_var in sheet_names) {
    # Write data to separate csv's for each sheet.
    outcomes_data <-
      read_excel(data_path, sheet = forecast_var) # Read in the data from the specified sheet of the Excel file
    
    # Process each sheet based on specific conditions
    if (forecast_var == "RECESS") {
      # E.g., specific processing for "RECESS"
      # Recess=1 if quarterly real GDP (or GNP, pre-1992) growth is less than 0.
      outcomes_data <- outcomes_data %>%
        mutate(event = Year + Quarter / 4,
               binOutcome = RECESS)
    }
    
    else if (forecast_var == "PRPGDP") {
      outcomes_data <- fn_quick_tidy(outcomes_data, forecast_var)
      
      bin_idx_years <- fn_idx_year_cutoffs(forecast_var)
      idx <- bin_idx_years[[1]]
      years <- bin_idx_years[[2]]
      bin_settings <-
        fn_bin_limits_time_bounds(idx) # Grab the appropriate bins.
      outcomes_data <-
        fn_bin_outcome(outcomes_data,
                       forecast_var,
                       years,
                       bin_settings[[1]],
                       bin_settings[[2]])
      outcomes_data <- outcomes_data %>% rename(event = year)
    }
    
    else if (forecast_var == "PRGDP") {
      outcomes_data <- fn_quick_tidy(outcomes_data, forecast_var)
      # Removes all rows where forecast_var equals 999 (i.e., missing).
      outcomes_data <-
        subset(outcomes_data, outcomes_data[[forecast_var]] != 999.0)
      bin_idx_years <- fn_idx_year_cutoffs(forecast_var)
      idx <- bin_idx_years[[1]]
      years <- bin_idx_years[[2]]
      bin_settings <- fn_bin_limits_time_bounds(idx)
      outcomes_data <-
        fn_bin_outcome(outcomes_data,
                       forecast_var,
                       years,
                       bin_settings[[1]],
                       bin_settings[[2]])
      outcomes_data <- outcomes_data %>% rename(event = year)
    }
    
    else if (forecast_var == "PRCCPI" | forecast_var == "PRCPCE") {
      # Realized Q4 to Q4 core CPI or core PCE price index percent change
      outcomes_data <- fn_quick_tidy(outcomes_data, forecast_var)
      bin_idx_years <- fn_idx_year_cutoffs(forecast_var)
      idx <- bin_idx_years[[1]]
      years <- bin_idx_years[[2]]
      bin_settings <- fn_bin_limits_time_bounds(idx)
      outcomes_data <-
        fn_bin_outcome(outcomes_data, forecast_var, years, bin_settings[[1]], NA)
      outcomes_data <- outcomes_data %>% rename(event = year)
    }
    
    else if (forecast_var == "PRUNEMP") {
      # Average realized 12-month unemployment.
      outcomes_data <- fn_quick_tidy(outcomes_data, forecast_var)
      bin_idx_years <- fn_idx_year_cutoffs(forecast_var)
      idx <- bin_idx_years[[1]]
      years <- bin_idx_years[[2]]
      bin_settings <- fn_bin_limits_time_bounds(idx)
      outcomes_data <-
        fn_bin_outcome(outcomes_data,
                       forecast_var,
                       years,
                       bin_settings[[1]],
                       bin_settings[[2]][c(1, 6:7)])
      outcomes_data <- outcomes_data %>% rename(event = year)
    }
    
    outcomes_data <- outcomes_data %>%
      rename(bin = binOutcome) %>%  # for merging
      select(-Notes) # 'Notes' is a column to be dropped
    
    # Store the dataframe in the list
    binned_data[[forecast_var]] <- outcomes_data
  }
  return(binned_data)
}
