library(dplyr)
library(data.table)
library(haven)
library(readxl)
library(tidyr)



# Get the current working directory
current_dir <- getwd()
# Check if in "SPF"
if (!grepl("SPF$", current_dir)) {
  setwd("SPF")
}

# Create  a dataset that looks like:
# event bin numBins prob timeToEnd
# Lot's of "one-off" cleaning lines/corrections, etc. -> hard to "systematize". 

vars <-
  c("PRPGDP", "RECESS", "PRCCPI", "PRCPCE", "PRUNEMP", "PRGDP")
# vars <- c("PRGDP")
stata_data <- read_dta("Data/SPFmicrodataCleaned_PRGDP.dta")

# An Excel workbook with multiple worksheets contains the individual responses of the
# forecasters: SPFmicrodata.xlsx. Each worksheet in the workbook covers the surveys of a
# different variable from 1968:Q4 to present.
# See page 31 for the variable definitions: https://www.philadelphiafed.org/-/media/frbp/assets/surveys-and-data/survey-of-professional-forecasters/spf-documentation.pdf?la=en&hash=F2D73A2CE0C3EA90E71A363719588D25
# Example: The variable PRGDP is the mean probability that the percent change in
# GDP falls in a particular range.
excel_file_path <- "SPFmicrodata.xlsx"
sheet_names <- excel_sheets(excel_file_path)

for (var in vars) {
  # Read the data
  data <-
    read_excel(excel_file_path, sheet = var)

  # Reshape from wide to long format.
  data <- melt(
    setDT(data),
    id.vars = c("YEAR", "QUARTER", "ID", "INDUSTRY"),
    variable.name = "bin",
    value.name = "p"
  )
  
  # Ordering the data to stack values row-wise for each set of id variables
  data <- data[order(YEAR, QUARTER, ID, INDUSTRY, bin)]
  
  # Convert the factor to character
  data$bin <- as.character(data$bin)
  
  # Extract the final number using regular expressions
  # This regular expression looks for one or more digits (\d+) at the end of the string ($)
  # Could use running count...
  # E.g., PRPGDP5 -> 5
  data$bin <- as.numeric(gsub("[^0-9]", "", data$bin))
  
  # Convert column names to lowercase
  names(data) <- tolower(names(data))
  
  # Remove rows where year contains missing values.
  data <- subset(data,!is.na(year))
  data <- subset(data, p != "#N/A")
  # Convert to numeric
  data$p <- as.numeric(data$p) / 100 # bounded in [0,1]
  data$time <-
    data$year + (1 / 8) + (data$quarter - 1) / 4
  
  if (var == "PRPGDP") {
    data$event <- data$year # An event is demarcated by a point in time.
    data$event <-
      ifelse(
        data$time > 1981.5 & data$time < 1992 &
          data$bin > 6,
        data$year + 1,
        data$event
      )
    data$bin <-
      ifelse(
        data$time > 1981.5 & data$time < 1992 &
          data$bin > 6,
        data$bin - 6,
        data$bin
      )
    
    data$event <-
      ifelse(data$time > 1992 & data$bin > 10,
             data$year + 1,
             data$event)
    data$bin <-
      ifelse(data$time > 1992 & data$bin > 10,
             data$bin - 10,
             data$bin)
    
    # Each element in the list is a pair: (event year, time threshold)
    conditions <-
      list(
        c(1973, 1973.25),
        c(1974, 1974.75),
        c(1981, 1981.5),
        c(1985, 1985.25),
        c(1992, 1992),
        c(2014, 2014)
      )
    
    # Apply conditions
    for (condition in conditions) {
      event_year <- condition[1]
      time_threshold <- condition[2]
      data <-
        data[!(data$event == event_year &
                      data$time < time_threshold),]
    }
  }
    
  else if (var == "PRGDP") {
    data$event <- data$year
    data$event <-
      ifelse(
        data$time > 1981.5 & data$time < 1992 &
          data$bin > 6,
        data$year + 1,
        data$event
      )
    data$bin <-
      ifelse(
        data$time > 1981.5 & data$time < 1992 &
          data$bin > 6,
        data$bin - 6,
        data$bin
      )
    
    data$event <-
      ifelse(
        data$time > 1992 & data$time < 2009.25 &
          data$bin > 10,
        data$year + 1,
        data$event
      )
    data$bin <-
      ifelse(
        data$time > 1992 & data$time < 2009.2 &
          data$bin > 10,
        data$bin - 10,
        data$bin
      )
    
    # Define the condition parameters
    conditions <- list(
      list("event", 1, 11, 22),
      list("event", 2, 22, 33),
      list("event", 3, 33, 44),
      list("bin",-11, 11, 22),
      list("bin",-22, 22, 33),
      list("bin",-33, 33, 44)
    )
    
    # Loop over conditions
    for (cond in conditions) {
      col_name <- cond[[1]]        # Column to modify (event or bin)
      add_val <- cond[[2]]
      lower_bound <- cond[[3]]     # Lower bound for bin
      upper_bound <- cond[[4]]     # Upper bound for bin
      
      if (col_name == "event") {
        data$event <-
          ifelse(
            data$time > 2009.25 &
              data$bin > lower_bound & data$bin <= upper_bound,
            data$year + add_val,
            data$event
          )
      } else if (col_name == "bin") {
        data$bin <-
          ifelse(
            data$time > 2009.25 &
              data$bin > lower_bound & data$bin <= upper_bound,
            data$bin + add_val,
            data$bin
          )
      }
    }
    
    # Each element in the list is a pair: (event year, time threshold)
    conditions <-
      list(
        c(1973, 1973.25),
        c(1974, 1974.75),
        c(1981, 1981.5),
        c(1992, 1992),
        c(2009, 2009.25),
        c(2010, 2009.25),
        c(2020, 2020.25)
      )
    
    # Apply conditions
    for (condition in conditions) {
      event_year <- condition[1]
      time_threshold <- condition[2]
      data <-
        data[!(data$event == event_year &
                      data$time < time_threshold),]
    }
    # p23: "However, an error was made in the first-quarter surveys of 1985 and 1986
    data <- subset(data, time != 1985.125)
    data <- subset(data, time != 1990.125)
    
  }
  
  else if (var == "PRCCPI" | var == "PRCPCE")  {
    data$event <- data$year
    
    data$event <-
      ifelse(
          data$bin > 10,
        data$year + 1,
        data$event
      )
    data$bin <-
      ifelse(
          data$bin > 10,
        data$bin - 10,
        data$bin
      )
  }

if (var == "PRUNEMP") {
  data$event <- data$year
  
  # Define the condition parameters. 
  conditions <- list(
    list("event", 1, 10, 20),
    list("event", 2, 20, 30),
    list("event", 3, 30, 40),
    list("bin",-10, 10, 20),
    list("bin",-20, 20, 30),
    list("bin",-30, 30, 40)
  )
  
  # Loop over conditions
  for (cond in conditions) {
    col_name <- cond[[1]]        # Column to modify (event or bin)
    add_val <- cond[[2]]
    lower_bound <- cond[[3]]     # Lower bound for bin
    upper_bound <- cond[[4]]     # Upper bound for bin
    
    if (col_name == "event") {
      data$event <-
        ifelse(
            data$bin > lower_bound & data$bin <= upper_bound,
          data$year + add_val,
          data$event
        )
    } else if (col_name == "bin") {
      data$bin <-
        ifelse(
            data$bin > lower_bound & data$bin <= upper_bound,
          data$bin + add_val,
          data$bin
        )
    }
  }
  data <-
    data[!(data$event >= 2014 &
                  data$time < 2014),]
  data <-
    data[!(data$event >= 2020 &
                  data$time < 2020.25),]
}
  if (var == "RECESS") {
    data$event <- data$time - (1/8) + (data$bin / 4)
    data$bin <- 1
  }
  
  # Convert the data.table to a data frame
  data <- as.data.frame(data)
  
  # Move 'event' to the first position
  data <- data[c("event", setdiff(names(data), "event"))]
  
  # The data will first be sorted by id, then by event within each id, then by time 
  # within each event, and finally by bin within each time.
  data <- data %>%
    arrange(id, event, time, bin)
  
  data <- subset(data, select = -year) # Drop year
  
  # Group by id and event and create a new variable grpd_id_event that assigns a unique group identifier
  data <- data %>%
    group_by(id, event) %>%
    mutate(grpd_id_event = cur_group_id())
  # Calculate the maximum value of the time variable within each group defined by the grpd_id_event variable.
  data <- data %>%
    group_by(grpd_id_event) %>%
    mutate(time_max = max(time)) %>%
    ungroup()
  
  data <- data %>% # Setting the next operation up.
    mutate(time_dup = ifelse(time_max == time, 2, 1))
  
  # Expand the dataframe based on the time_dup variable/Duplicate the rows according to the values in t_dup
  # and create a duplicate variable that distinguishes between the original and duplicated rows.
  data <- data %>%
    mutate(row_id = row_number()) %>%  # Create a temporary identifier for each row
    uncount(time_dup) %>%  # Duplicate rows
    group_by(row_id) %>%  # Group by the temporary identifier
    mutate(duplicate = row_number()) %>%  # Number each row within its group
    ungroup() %>%  # Remove grouping
    select(-row_id)  # Remove the temporary identifier
  
  data <- data %>%
    arrange(grpd_id_event, time, duplicate)
  
  # annoyingly, for recess event=1983 means ending 4th quarter 1982
  # but, for GDP, event=1982 means ending 4th quarter 1982
  data <- data %>%
    mutate(time = ifelse(duplicate == 2 & var == "RECESS", event - 0.001, time))
  data <- data %>%
    mutate(time = ifelse(duplicate == 2 & var != "RECESS", event + 0.999, time))
  
  file_path <- paste0("Data/RealizedOutcomesClean_", var, ".csv")  
  realized_outcomes <- read.csv(file_path)
  
  # Add a source indicator to each dataframe
  data$in_data <- TRUE
  realized_outcomes$in_ro <- TRUE
  
  # Perform the merge
  data <- data %>%
    full_join(realized_outcomes, by = c("event", "bin")) %>%
    arrange(event, bin)
  
  # Mimic statas' _merge indicator
  data <- data %>%
    mutate(statas_merge = case_when(
      !is.na(in_data) & is.na(in_ro) ~ 1,   # Present only in data
      is.na(in_data) & !is.na(in_ro) ~ 2,   # Present only in realized_outcomes
      TRUE ~ 3                              # Present in both
    ))
  
  data <- select(data, -in_data, -in_ro)
  
  data <- data %>%
    filter(!(var == "PRGDP" & duplicate == 2 & event == 1981))
  # Drop rows where statas' _merge is 2 (only in realized_outcomes)
  data <- filter(data, statas_merge != 2)
  
  data <- data %>%
    mutate(
      p = ifelse(duplicate == 2, 0, p))
  
  data <- data %>%
    mutate(
      p = ifelse(duplicate == 2 & statas_merge == 3, 1, p))
  
  # For future events, we don't have resolution -> need to delete that resolution entry
  data <- data %>%
    group_by(id, event, time) %>%
    mutate(grpd_id_event_time = cur_group_id(),  # Assign a unique ID to each group
           total = sum(p, na.rm = TRUE)) %>%  # Sum p within each group
    ungroup() %>% # Remove grouping
    filter(total != 0) %>% # For ones without resolution, all p's will be 0.
    select(-grpd_id_event_time, -total)
  
  # Now get variables that are correct.
  data$resolution <- data$duplicate
  data <- select(data, -time_max, -duplicate, -statas_merge, -grpd_id_event)
  data <- data %>%
    arrange(id, event, time, bin) # sort
  data <- data %>%
    mutate(quarter = ifelse(resolution == 2, 5, quarter))
  
  # Get resolution everywhere in an event
  data <- data %>%
    group_by(event) %>%
    mutate(realization = mean(!!sym(var), na.rm = TRUE)) %>%
    ungroup() %>%
    select(-all_of(var))
  
  data <- data %>%
    arrange(id, event, time, bin) # sort

  # data <- data %>%
  #   arrange(id, event, quarter, time, bin, p, realization, resolution, industry) # JAKE
  # 
  stata_data$resolution <- stata_data$resolution + 1
  stata_data$realization <- replace(stata_data$realization, is.na(stata_data$realization), NaN)
  # stata_data <- stata_data %>%
  #   arrange(id, event, quarter, time, bin, p, realization, resolution, industry) 
  
  # Check for equality of data #
  col_names_equal <- identical(names(data), names(stata_data))
  # if (!col_names_equal) {
  #   print("**************************")
  #   print("Column names are not equal")
  #   print(var)
  # }
  
  standardize_values <- function(df, digits = 3) {
    # Function to round and remove trailing zeros
    round_and_trim <- function(x, digits) {
      rounded <- round(x, digits)
      # Remove trailing zeros and convert to character
      trimmed <- gsub("0+$", "", gsub("\\.$", "", format(rounded, nsmall = digits)))
      return(trimmed)
    }
    
    df %>%
      mutate(across(where(is.numeric), ~round_and_trim(., digits))) %>%
      mutate(across(everything(), as.character))
  }
  
  data_processed <- standardize_values(data)
  data_processed <- data_processed %>%
    mutate(industry = gsub("\\.0000$", "", industry))
  stata_data_processed <- standardize_values(stata_data)
  
  # Check for differences
  differences <- mapply(function(x, y) {
    ifelse(x != y, paste("R:", x, "Stata:", y), NA)
  }, data_processed, stata_data_processed)
  
  # Convert to a dataframe for better readability
  diff_df <- as.data.frame(differences)
  
  # Find rows with differences
  diff_rows <- which(apply(diff_df, 1, function(x) any(!is.na(x))))
  
  # Rows with differences
  diff_df <- diff_df[diff_rows, ]
  # Drop columns that are all NA
  diff_df <- diff_df %>%
    select_if(~any(!is.na(.)))
  
  # if (nrow(diff_df) != 0) {
  #   print("**************************")
  #   print("Differences")
  #   print(var)
  #   # print(diff_df)
  # }

  # file_path <- paste0("Data/SPFmicrodataCleaned_", var, ".csv")
  # write.csv(data, file_path, row.names = FALSE)
}

# Look for typos from last 3 files!














