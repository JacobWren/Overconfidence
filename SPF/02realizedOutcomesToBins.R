library(readxl)
library(dplyr)
library(haven)


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
  df$binOutcome <- NA  # Initialize binOutcome with NAs
  
  # Reorder columns
  cols <- names(df)
  new_order <-
    c(cols[cols != "binOutcome" & cols != var], "binOutcome", var)
  df <- df[new_order]
  
  df <- df %>% rename(year = "Year (Q4)")  # Rename YearQ4 to year
  
  return(df)
}

bin_params <- function(elems) {
  # Define the binning limits + year bounds (trying not to repeat myself)
  end_point <- 1000
  limits <- list(
    c(end_point, 10:-3, -end_point),
    c(end_point, 12:-1, -end_point),
    c(end_point, 16:3, -end_point),
    c(end_point, seq(12, 4, by = -2), -end_point),
    c(end_point, seq(10, 2, by = -2), -end_point),
    c(end_point, 8:0, -end_point),
    c(end_point, seq(4, 0, by = -0.5), -end_point),
    c(end_point, seq(6, -2, by = -2), -end_point),
    c(end_point, 6:-2, -end_point),
    c(end_point, 6:-3, -end_point),
    c(end_point, 16, 10, 7, 4, 2.5, 1.5, 0,-3,-6,-12, -end_point),
    c(end_point, 11, seq(10, 7, by = -0.5), 6, -end_point),
    c(end_point, 9, seq(8, 5, by = -0.5), 4, -end_point),
    c(end_point, 15, 12, 10, 8:3, -end_point)
  )
  ops <-
    list(`<`,
         `==`,
         c(`>=`, `<`),
         c(`>=`, `<`),
         c(`>=`, `<`),
         c(`>=`, `<`),
         `>=`)
  
  return(list(limits[elems], ops))
}
# Internal check
# stata_data <- read_dta("Data/RealizedOutcomesClean_PRUNEMP.dta")

# 1 sheet per variable
excel_file_path <- "RealizedOutcomes.xlsx"
sheet_names <- excel_sheets(excel_file_path)

for (var in sheet_names) {
  # Write data to separate csv's for each sheet.
  data <-
    read_excel(excel_file_path, sheet = var) # Read in the data from the specified sheet of the Excel file
  
  # Process each sheet based on specific conditions
  if (var == "RECESS") {
    # E.g., specific processing for "RECESS"
    data <- data %>%
      mutate(event = Year + Quarter / 4,
             binOutcome = RECESS)
  }
  
  else if (var == "PRPGDP") {
    data <- quick_tidy(data)
    
    years <-
      list(
        1973,
        1973,
        list(1974, 1981),
        list(1981, 1985),
        list(1985, 1992),
        list(1992, 2014),
        2014
      )
    bin_settings <- bin_params(1:7)
    data <-
      bin_data(data, var, years, bin_settings[[1]], bin_settings[[2]])
    data <- data %>% rename(event = year)
  }
  
  else if (var == "PRGDP") {
    data <- quick_tidy(data)
    # Removes all rows where var equals 999 (i.e., missing).
    data <- subset(data, data[[var]] != 999.0)
    
    years <-
      list(
        1973,
        1973,
        list(1974, 1981),
        list(1981, 1992),
        list(1992, 2009),
        list(2009, 2020),
        2020
      )
    bin_settings <- bin_params(c(1:3, 8:11))
    data <-
      bin_data(data, var, years, bin_settings[[1]], bin_settings[[2]])
    data <- data %>% rename(event = year)
  }
  
  else if (var == "PRCCPI" | var == "PRCPCE") {
    data <- quick_tidy(data)
    years <- NA
    bin_settings <- bin_params(7)
    data <- bin_data(data, var, years, bin_settings[[1]], NA)
    data <- data %>% rename(event = year)
  }
  
  else if (var == "PRUNEMP") {
    data <- quick_tidy(data)
    years <- list(2014, list(2014, 2020), 2020)
    bin_settings <- bin_params(c(12:14))
    data <-
      bin_data(data, var, years, bin_settings[[1]], bin_settings[[2]][c(1, 6:7)])
    data <- data %>% rename(event = year)
  }
  
  data <- data %>%
    rename(bin = binOutcome) %>%  # for merging
    select(-Notes) # 'Notes' is a column to be dropped
  
  # # Check for equality of data #
  # col_names_equal <- identical(names(data), names(stata_data))
  # print(paste("Column names are equal:", col_names_equal))
  
  # matrix1 <- as.matrix(data)
  # matrix2 <- as.matrix(stata_data)
  # data_values_equal <- all.equal(matrix1, matrix2)
  # print(paste("Data values are equal:", data_values_equal))
  
  # Save the processed data
  write.csv(data,
            paste0("Data/RealizedOutcomesClean_", var, ".csv"),
            row.names = FALSE)
}
