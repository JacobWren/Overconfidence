# Get and set working directory
current_dir <- getwd()
if (!grepl("SPF$", current_dir)) {
  setwd("SPF")
}

source("init_script.R")

tpath <-
  paste0("Data/SPFmicrodataCleaned_PRGDP.csv")
t1 <- read.csv(tpath) # %>%

# For comparing files.

# 1 sheet per variable
excel_file_path <- "RawData/prob.xlsx"
sheet_names <- excel_sheets(excel_file_path)
sheet_names <- sheet_names[sheet_names != "RECESS"]

path <-
  paste0("Data/combinedVarInfoAggWithBins.csv")
df1 <- read.csv(path) # %>%
  # arrange(event, quarter, bin)

# Helper function.
convert_and_average <- function(x) {
  numeric_values <- as.numeric(replace(x, x == "#NA", NA))
  mean(numeric_values, na.rm = TRUE)
}

for (var in sheet_names) {
  df2 <-
    read_excel(excel_file_path, sheet = var) # Read in the data from the specified sheet of the Excel file
    
  # df2_filt <- df2 %>%
  #   select_if(~ !any(. == "#N/A")) 
  
  df1_sub <- df1 %>%
    filter(dataSet == var) %>%
    select(event, bin, time, quarter, pAgg)
  
  # # Find non-unique rows based on combination of event, quarter, and bin
  # non_unique_rows <- df1_sub %>%
  #   group_by(event, time, bin) %>%
  #   summarise(count = n(), .groups = 'drop') %>%
  #   filter(count > 1)
  
  # Reshape from long to wide format
  # df1_wide <- dcast(setDT(df1_sub), event + time + quarter ~ bin, value.var = "pAgg") %>%
  #   mutate(across(
  #     .cols = -c(event, time, quarter), 
  #     .fns = ~ map_if(., is.numeric,  ~ . * 100)
  #   ))
  df1_sub <- df1_sub %>%
    mutate(pAgg = pAgg * 100)
  
  df1_wide <- dcast(setDT(df1_sub), event + time + quarter ~ bin, value.var = "pAgg")
  
  df1_wide_avg <- df1_wide %>%
    group_by(event, quarter) %>%
    arrange(desc(time)) %>%
    slice(1) %>%
    ungroup() %>%
    select(-time)
  
  df2 <- df2 %>%
    rename(
      event = YEAR,
      quarter = QUARTER
    )
  # Rows from df2 where there are no corresponding rows in df1_wide_avg.
  # result <- anti_join(df2, df1_wide_avg, by = c("event", "quarter"))
  
  # Common rows.
  df2_cmn <- semi_join(df2, df1_wide_avg, by = c("event", "quarter"))
  
  names(df2_cmn) <- gsub(paste0("^", var), "", names(df2_cmn))
  
  # Identify columns that are numeric.
  numeric_cols_df1 <- names(df1_wide_avg)[grepl("^[0-9]+$", names(df1_wide_avg))]
  numeric_cols_df2 <- names(df2_cmn)[grepl("^[0-9]+$", names(df2_cmn))]
  
  # Find the largest common numeric column
  common_numeric_cols <- intersect(numeric_cols_df1, numeric_cols_df2)
  max_common_col <- max(as.numeric(common_numeric_cols))
  
  # Identify columns in df2_cmn to be averaged, including the largest common column
  cols_to_average <- numeric_cols_df2[as.numeric(numeric_cols_df2) >= max_common_col]
  # 
  # # Convert '#NA' to NA in df2_cmn
  # df2_cmn[numeric_cols_df2] <- lapply(df2_cmn[numeric_cols_df2], function(x) {
  #   as.numeric(gsub("#NA", NA, x))
  # })
  #
  # # Calculating the average for the identified columns
  # average_values <- rowMeans(df2_cmn[cols_to_average], na.rm = TRUE)
  
  # Replacing the values of the largest common numeric column with this average
  max_common_col_name <- as.character(max_common_col)
  
  # # Replace the values of the column named as max_common_col with average_values
  # df2_cmn[[max_common_col_name]] <- average_values
  
  # Exclude max_common_col from cols_to_average
  cols_to_drop <- setdiff(cols_to_average, max_common_col)
  
  # Drop cols_to_drop from df2_cmn
  df2_cmn <- df2_cmn[, !names(df2_cmn) %in% cols_to_drop]
  
  first_two_df1 <- df1_wide_avg[, 1:2]
  first_two_df2 <- df2_cmn[, 1:2]
  
  # Check if they are equal
  are_equal <- all(first_two_df1 == first_two_df2)
  
  col_names_equal <- identical(names(df1_wide_avg), names(df2_cmn))

  if (!are_equal | !col_names_equal) {
    print("**************************")
    print(var)
  }
  
  # JAKE: Need to see which rows differ by missing before comparing numerics.
  # Extract the names of the columns
  # Select columns excluding 'event' and 'quarter'
  columns <- setdiff(names(df1_wide_avg), c("event", "quarter"))
  
  # # Initialize an empty vector to store indices of rows that meet the condition
  # mismatch_indices <- c()
  # 
  # # Iterate over rows
  # for (i in 1:nrow(df1_wide_avg)) {
  #   for (col in columns) {
  #     val1 <- df1_wide_avg[i, col]
  #     val2 <- df2_cmn[i, col]
  #     
  #     # Check for NA (including NaN) values
  #     na_val1 <- is.na(val1)
  #     na_val2 <- is.na(val2)
  #     
  #     # Check for NA in one and not the other
  #     if ((na_val1 && !na_val2) || (!na_val1 && na_val2)) {
  #       mismatch_indices <- c(mismatch_indices, i)
  #       break # No need to check other columns for this row
  #     }
  #   }
  # }
  # 
  # # Get the rows that meet the condition from df1_wide_avg
  # mismatch_rows_df1 <- df1_wide_avg[mismatch_indices, ]
  # 
  # # Assuming df2_cmn has the same number of rows, get corresponding rows
  # mismatch_rows_df2 <- df2_cmn[mismatch_indices, ]
  
  # Set the tolerance level for comparison
  tolerance <- 1e-4  # Adjust this value as needed
  
  # Initialize a list to store the indices of differing values
  differences <- list()
  
  # Set the tolerance level for comparison
  tolerance <- 1e-4  # Adjust this value as needed
  
  # Iterate over the rows and columns
  for (i in 1:nrow(df1_wide_avg)) {
    for (j in 3:ncol(df1_wide_avg)) {
      val1 <- df1_wide_avg[[i, j]]
      val2 <- df2_cmn[[i, j]]
      
      # Check if both values are numeric and not NA or NaN
      if (is.numeric(val1) && is.numeric(val2) && !is.na(val1) && !is.na(val2)) {
        # Check if the difference is greater than the tolerance
        if (abs(val1 - val2) > tolerance) {
          print(var)
          print(paste("Year:", df1_wide_avg[i, "event"],
                      "Quarter:", df1_wide_avg[i, "quarter"],
                      "Column:", j - 2, 
                      "df1_wide_avg Value:", val1, 
                      "df2_cmn Value:", val2))
        }
      }
    }
  }
  
  # JAKE: you need to compare those numeric columns. 
  # 1 comp for NA vs not
  # And 1 comp on the difference.
  
  # JAKE: find which rows are in df2 but not df1_wide_avg. Does this make sense?
  
  # We want to check that our aggregate numbers match theirs. However, the file structures don’t match. This is 
  # because, for them, a row is uniquely identified my a variable-year-quarter, while for us it is identified by a 
  # variable-event-time. It is the time variable that creates the challenge.
}
  