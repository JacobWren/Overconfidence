source("fn_bin_settings.R")
source("fn_binning.R")

# Maps observable outcomes to bin numbers across years and forecasting variables.
# Data structure: one observation per event, where an event is a point in time, e.g., 1983.75 is Q3 of 1983.
# Columns are event (usually "year" alone), variable (e.g., recession?), and bin.


fn_quick_tidy <- function(df, pred_var) {
  # Cleaning helper.
  df$bin_outcome <- NA  # Initialize bin_outcome with NAs
  
  # Reorder columns
  cols <- names(df)
  new_order <-
    c(cols[cols != "bin_outcome" &
             cols != pred_var], "bin_outcome", pred_var)
  df <- df[new_order]
  df <- df %>% rename(year = "Year (Q4)")  # Rename YearQ4 to year
  return(df)
}


fn_realized_outcomes_to_bins <- function(sheet_names, data_path) {
  # Initialize an empty list to store data frames by forecasting variable.
  binned_outcomes <- list()
  
  for (forecast_var in sheet_names) {
    # Write data to separate csv's for each sheet.
    df_outcomes <-
      read_excel(data_path, sheet = forecast_var) # Read in the data from the specified sheet of the Excel file
    
    # Process each sheet based on specific conditions
    if (forecast_var == "RECESS") {
      # E.g., specific processing for "RECESS"
      # Recess=1 if quarterly real GDP (or GNP, pre-1992) growth is less than 0.
      df_outcomes <- df_outcomes %>%
        mutate(event = Year + Quarter / 4,
               bin_outcome = RECESS)
    }
    
    else if (forecast_var == "PRPGDP") {
      df_outcomes <- fn_quick_tidy(df_outcomes, forecast_var)
      
      bin_idx_years <- fn_idx_year_cutoffs(forecast_var)
      idx <- bin_idx_years[[1]]
      years <- bin_idx_years[[2]]
      bin_settings <-
        fn_bin_limits_time_bounds(idx) # Grab the appropriate bins.
      df_outcomes <-
        fn_bin_outcome(df_outcomes,
                       forecast_var,
                       years,
                       bin_settings[[1]],
                       bin_settings[[2]])
      df_outcomes <- df_outcomes %>% rename(event = year)
    }
    
    else if (forecast_var == "PRGDP") {
      df_outcomes <- fn_quick_tidy(df_outcomes, forecast_var)
      # Removes all rows where forecast_var equals 999 (i.e., missing).
      df_outcomes <-
        subset(df_outcomes, df_outcomes[[forecast_var]] != 999.0)
      bin_idx_years <- fn_idx_year_cutoffs(forecast_var)
      idx <- bin_idx_years[[1]]
      years <- bin_idx_years[[2]]
      bin_settings <- fn_bin_limits_time_bounds(idx)
      df_outcomes <-
        fn_bin_outcome(df_outcomes,
                       forecast_var,
                       years,
                       bin_settings[[1]],
                       bin_settings[[2]])
      df_outcomes <- df_outcomes %>% rename(event = year)
    }
    
    else if (forecast_var == "PRCCPI" | forecast_var == "PRCPCE") {
      # Realized Q4 to Q4 core CPI or core PCE price index percent change
      df_outcomes <- fn_quick_tidy(df_outcomes, forecast_var)
      bin_idx_years <- fn_idx_year_cutoffs(forecast_var)
      idx <- bin_idx_years[[1]]
      years <- bin_idx_years[[2]]
      bin_settings <- fn_bin_limits_time_bounds(idx)
      df_outcomes <-
        fn_bin_outcome(df_outcomes, forecast_var, years, bin_settings[[1]], NA)
      df_outcomes <- df_outcomes %>% rename(event = year)
    }
    
    else if (forecast_var == "PRUNEMP") {
      # Average realized 12-month unemployment.
      df_outcomes <- fn_quick_tidy(df_outcomes, forecast_var)
      bin_idx_years <- fn_idx_year_cutoffs(forecast_var)
      idx <- bin_idx_years[[1]]
      years <- bin_idx_years[[2]]
      bin_settings <- fn_bin_limits_time_bounds(idx)
      df_outcomes <-
        fn_bin_outcome(df_outcomes,
                       forecast_var,
                       years,
                       bin_settings[[1]],
                       bin_settings[[2]][c(1, 6:7)])
      df_outcomes <- df_outcomes %>% rename(event = year)
    }
    
    df_outcomes <- df_outcomes %>%
      rename(bin = bin_outcome) %>%  # for merging
      select(-Notes) # 'Notes' is a column to be dropped
    
    # Store the dataframe in the list
    binned_outcomes[[forecast_var]] <- df_outcomes
  }
  return(binned_outcomes)
}
