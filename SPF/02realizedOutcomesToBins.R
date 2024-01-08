# Get and set working directory
current_dir <- getwd()
if (!grepl("SPF$", current_dir)) {
  setwd("SPF")
}
source("init_script.R")
source("fn_bin_settings.R")

# Maps observable outcomes to bins that across years and variables.
# Data structure: one observation per event, where an event is a point in time, e.g., 1983.75 is Q3 of 1983.
# Columns are time (usually "year" alone), variable (e.g., recession?), event, and bin.

# Apply binning logic
bin_data <-
  function(df,
           var_name,
           year_thresholds,
           bin_limits,
           operators) {
    for (j in seq_along(year_thresholds)) {
      year_threshold <- year_thresholds[[j]]
      ops <- operators[[j]]
      bin_limit <- bin_limits[[j]]
      
      for (i in 1:(length(bin_limit) - 1)) {
        lower_limit <- bin_limit[i + 1]
        upper_limit <- bin_limit[i]
        
        # Nest years and limits.
        if (is.list(year_threshold)) {
          df$binOutcome <- ifelse(
            df[[var_name]] < upper_limit &
              df[[var_name]] >= lower_limit &
              ops[[1]](df$year, year_threshold[1]) &
              ops[[2]](df$year, year_threshold[2]),
            i,
            df$binOutcome
          )
        } else if (typeof(year_threshold) == "double") {
          df$binOutcome <- ifelse(
            df[[var_name]] < upper_limit &
              df[[var_name]] >= lower_limit &
              ops(df$year, year_threshold),
            i,
            df$binOutcome
          )
        }
        else if (is.na(year_threshold)) {
          # No applicable year
          df$binOutcome <- ifelse(df[[var_name]] < upper_limit &
                                    df[[var_name]] >= lower_limit,
                                  i, df$binOutcome)
        }
      }
    }
    return(df)
  }

quick_tidy <- function(df) {
  # Cleaning helper.
  df$binOutcome <- NA  # Initialize binOutcome with NAs
  
  # Reorder columns
  cols <- names(df)
  new_order <-
    c(cols[cols != "binOutcome" & cols != var], "binOutcome", var)
  df <- df[new_order]
  df <- df %>% rename(year = "Year (Q4)")  # Rename YearQ4 to year
  return(df)
}

# 1 sheet per variable
excel_file_path <- "RawData/RealizedOutcomes.xlsx"
sheet_names <- excel_sheets(excel_file_path)

for (var in sheet_names) {
  # Write data to separate csv's for each sheet.
  data <-
    read_excel(excel_file_path, sheet = var) # Read in the data from the specified sheet of the Excel file
  
  # Process each sheet based on specific conditions
  if (var == "RECESS") {
    # E.g., specific processing for "RECESS"
    # Recess=1 if quarterly real GDP (or GNP, pre-1992) growth is less than 0.
    data <- data %>%
      mutate(event = Year + Quarter / 4,
             binOutcome = RECESS)
  }
  
  else if (var == "PRPGDP") {
    data <- quick_tidy(data)
    
    bin_idx_years <- fn_idx_year_cutoffs(var)
    idx <- bin_idx_years[[1]]
    years <- bin_idx_years[[2]]
    bin_settings <-
      fn_bin_limits_time_bounds(idx) # Grab the appropriate bins.
    data <-
      bin_data(data, var, years, bin_settings[[1]], bin_settings[[2]])
    data <- data %>% rename(event = year)
  }
  
  else if (var == "PRGDP") {
    data <- quick_tidy(data)
    # Removes all rows where var equals 999 (i.e., missing).
    data <- subset(data, data[[var]] != 999.0)
    bin_idx_years <- fn_idx_year_cutoffs(var)
    idx <- bin_idx_years[[1]]
    years <- bin_idx_years[[2]]
    bin_settings <- fn_bin_limits_time_bounds(idx)
    data <-
      bin_data(data, var, years, bin_settings[[1]], bin_settings[[2]])
    data <- data %>% rename(event = year)
  }
  
  else if (var == "PRCCPI" | var == "PRCPCE") {
    # Realized Q4 to Q4 core CPI or core PCE price index percent change
    data <- quick_tidy(data)
    bin_idx_years <- fn_idx_year_cutoffs(var)
    idx <- bin_idx_years[[1]]
    years <- bin_idx_years[[2]]
    bin_settings <- fn_bin_limits_time_bounds(idx)
    data <- bin_data(data, var, years, bin_settings[[1]], NA)
    data <- data %>% rename(event = year)
  }
  
  else if (var == "PRUNEMP") {
    # Average realized 12-month unemployment.
    data <- quick_tidy(data)
    bin_idx_years <- fn_idx_year_cutoffs(var)
    idx <- bin_idx_years[[1]]
    years <- bin_idx_years[[2]]
    bin_settings <- fn_bin_limits_time_bounds(idx)
    data <-
      bin_data(data, var, years, bin_settings[[1]], bin_settings[[2]][c(1, 6:7)])
    data <- data %>% rename(event = year)
  }
  
  data <- data %>%
    rename(bin = binOutcome) %>%  # for merging
    select(-Notes) # 'Notes' is a column to be dropped
  
  # Save the processed data
  write.csv(data,
            paste0("Data/RealizedOutcomesClean_", var, ".csv"),
            row.names = FALSE)
}
