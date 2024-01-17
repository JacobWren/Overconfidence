# Lot's of "one-off" cleaning lines/corrections, etc. -> hard to "systematize".

# An Excel workbook with multiple worksheets contains the individual responses of the forecasters:
# SPFmicrodata.xlsx. Each worksheet in the workbook covers the surveys of a different variable.
# See page 31 for the variable definitions:
# https://www.philadelphiafed.org/-/media/frbp/assets/surveys-and-data/survey-of-professional-forecasters/spf-documentation.pdf?la=en&hash=F2D73A2CE0C3EA90E71A363719588D25
# Example: The variable PRGDP is the probability that the percent change in GDP falls in a particular range.

fn_clean <- function(SPF_path, pred_vars) {
  # Initialize empty lists to store data
  cleaned <- list()
  for (pred_var in pred_vars) {
    # Read the data
    df_spf <-
      read_excel(SPF_path, sheet = pred_var)
    
    # Reshape from wide to long format.
    df_spf <- melt(
      setDT(df_spf),
      id.vars = c("YEAR", "QUARTER", "ID", "INDUSTRY"),
      variable.name = "bin",
      value.name = "p"
    )
    
    # Ordering the data to stack values row-wise for each set of id variables
    df_spf <- df_spf[order(YEAR, QUARTER, ID, INDUSTRY, bin)]
    
    # Convert the factor to character
    df_spf$bin <- as.character(df_spf$bin)
    
    # Extract the final number using regular expressions.
    # This regular expression looks for one or more digits (\d+) at the end of the string ($).
    # Could use running count...
    # E.g., PRPGDP5 -> 5
    df_spf$bin <- as.numeric(gsub("[^0-9]", "", df_spf$bin))
    
    # Convert column names to lowercase
    names(df_spf) <- tolower(names(df_spf))
    
    # Remove rows where year contains missing values.
    df_spf <- subset(df_spf, !is.na(year) & p != "#N/A")
    
    # Convert to numeric
    df_spf$p <- as.numeric(df_spf$p) / 100 # bounded in [0,1]
    df_spf$time <-
      df_spf$year + (1 / 8) + (df_spf$quarter - 1) / 4
    
    if (pred_var != "RECESS") {
      df_spf$event <-
        df_spf$year # An event is demarcated by a point in time.
    }
    
    if (pred_var == "PRPGDP") {
      # First condition set
      condition1 <-
        # Consequence of a changing forecast horizon -- probability projections for the current year
        # and the next.
        df_spf$time > 1981.5 & df_spf$time < 1992 & df_spf$bin > 6
      df_spf$event[condition1] <- df_spf$year[condition1] + 1
      df_spf$bin[condition1] <- df_spf$bin[condition1] - 6
      
      # Second condition set
      condition2 <- df_spf$time > 1992 & df_spf$bin > 10
      df_spf$event[condition2] <- df_spf$year[condition2] + 1
      df_spf$bin[condition2] <- df_spf$bin[condition2] - 10
      
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
        df_spf <-
          df_spf[!(df_spf$event == event_year &
                     df_spf$time < time_threshold),]
      }
    }
    
    else if (pred_var == "PRGDP") {
      # First condition set
      condition1 <-
        df_spf$time > 1981.5 & df_spf$time < 1992 & df_spf$bin > 6
      df_spf$event <-
        ifelse(condition1, df_spf$year + 1, df_spf$event)
      df_spf$bin <- ifelse(condition1, df_spf$bin - 6, df_spf$bin)
      
      # Second condition set
      condition2 <-
        df_spf$time > 1992 & df_spf$time < 2009.25 & df_spf$bin > 10
      df_spf$event <-
        ifelse(condition2, df_spf$year + 1, df_spf$event)
      df_spf$bin <- ifelse(condition2, df_spf$bin - 10, df_spf$bin)
      
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
        
        # Define the common condition
        common_condition <-
          df_spf$time > 2009.25 &
          df_spf$bin > lower_bound & df_spf$bin <= upper_bound
        
        # Update event or bin
        if (col_name == "event") {
          df_spf$event <-
            ifelse(common_condition,
                   df_spf$year + add_val,
                   df_spf$event)
        } else if (col_name == "bin") {
          df_spf$bin <-
            ifelse(common_condition, df_spf$bin + add_val, df_spf$bin)
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
        df_spf <-
          df_spf[!(df_spf$event == event_year &
                     df_spf$time < time_threshold),]
      }
      # p23: "However, an error was made in the first-quarter surveys of 1985 and 1986...
      # In addition, in the first quarter of 1990, for the same variables listed above, the same error was made."
      df_spf <- subset(df_spf, !(time %in% c(1985.125, 1990.125)))
    }
    
    else if (pred_var == "PRCCPI" | pred_var == "PRCPCE")  {
      # Define the condition
      condition <- df_spf$bin > 10
      # Update event and bin using the defined condition
      df_spf$event <-
        ifelse(condition, df_spf$year + 1, df_spf$event)
      df_spf$bin <- ifelse(condition, df_spf$bin - 10, df_spf$bin)
    }
    
    else if (pred_var == "PRUNEMP") {
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
          df_spf$event <-
            ifelse(
              df_spf$bin > lower_bound & df_spf$bin <= upper_bound,
              df_spf$year + add_val,
              df_spf$event
            )
        } else if (col_name == "bin") {
          df_spf$bin <-
            ifelse(
              df_spf$bin > lower_bound & df_spf$bin <= upper_bound,
              df_spf$bin + add_val,
              df_spf$bin
            )
        }
      }
      df_spf <-
        df_spf[!(df_spf$event >= 2014 &
                   df_spf$time < 2014),]
      df_spf <-
        df_spf[!(df_spf$event >= 2020 &
                   df_spf$time < 2020.25),]
    }
    if (pred_var == "RECESS") {
      df_spf$event <- df_spf$time - (1 / 8) + (df_spf$bin / 4)
      df_spf$bin <- 1
    }
    
    # Convert the data.table to a data frame and move 'event' to the first position.
    df_spf <- df_spf %>%
      as.data.frame() %>%
      select(event, setdiff(names(.), "event"))
    
    # Store the dataframe in the list
    cleaned[[pred_var]] <- df_spf
  }
  return(cleaned)
}