# Merge truth and respondent predictions.
fn_clean <- function() {
  # Read the answers
  answers_df <- read.csv("Data/ans100423.csv")
  
  answers_df <- answers_df %>%
    mutate(
      mbestsd = as.numeric(as.character(mbestsd)),
      mavgsd = as.numeric(as.character(mavgsd)),
      sd = if_else(problem == 12 & line == "l", 11, sd),
      # Issue
      sd_true = sd,
      # Take avg because not sure what to do.
      sd_true_within = if_else(!is.na(mbestsd), (mbestsd + mavgsd) / 2, sd),
      sd_true_model = sd_true - sd_true_within
    ) %>%
    select(-sd)
  
  answers_df <- answers_df %>%
    mutate(
      pic = paste0(problem, distance, line, ".jpeg"),
      with_line = line == "l",
      # Add bools.
      far = distance == "f"
    ) %>%
    select(pic, mean, starts_with("sd"), with_line, far, scaled) %>%
    rename(mean_true = mean)
  
  # Read in respondent data
  responses_df <- read.csv("Data/231003PES.csv")
  
  responses_df <- responses_df %>%
    slice(-2) %>% # Skip first two rows.
    filter(loopCounter == "13") # Missing/incomplete responses.
  
  # Initialize comp_score column.
  responses_df$comp_score <- 0
  
  # List of question numbers.
  questions <- c(31, 32, 37, 92, 93, 94)
  
  # Iterate over questions.
  for (q_num in questions) {
    question <- paste0("Q", q_num)
    
    # Convert to numeric.
    responses_df[[question]] <-
      as.numeric(as.character(responses_df[[question]]))
    
    # Calculate mode and create a relative scoring measure.
    mode_value <- mfv(responses_df[[question]])
    responses_df$comp_score <-
      responses_df$comp_score + (responses_df[[question]] == mode_value)
  }
  
  # Keeping only specific columns.
  responses_df <- responses_df %>%
    rename(duration_secs = `Duration..in.seconds.`) %>%
    select(allSliderData,
           StartDate,
           EndDate,
           duration_secs,
           Condition,
           comp_score)
  
  # Clean slider data information.
  responses_df$allSliderData <-
    str_replace_all(responses_df$allSliderData, "'", "")
  responses_df$allSliderData <-
    str_replace_all(responses_df$allSliderData, "~", " ")
  
  # Break slider data apart.
  # Create a sequence for each picture.
  n_pics <- 1:15
  n_info_per_pic <- 5
  # Create a temporary dataframe to store the slider data.
  slider_data_df <-
    data.frame(matrix(
      ncol = length(n_pics) * n_info_per_pic,
      nrow = nrow(responses_df)
    ))
  
  # Name the new columns appropriately.
  col_names <- c()
  for (q in n_pics) {
    for (i in 1:n_info_per_pic) {
      name <- switch(
        i,
        "1" = "loop_counter",
        "2" = "cond",
        "3" = "pic",
        "4" = "mean",
        "5" = "sd"
      )
      col_names <- c(col_names, paste0(name, q))
    }
  }
  names(slider_data_df) <- col_names
  
  # Split the slider data column into words and assign to df.
  df_words <-
    str_split_fixed(responses_df$allSliderData, " ", length(col_names))
  slider_data_df[] <- df_words
  
  # Combine the original dataframe with the new.
  responses_df <- cbind(responses_df, slider_data_df)
  
  # Identify each respondent.
  responses_df$id <- seq_len(nrow(responses_df))
  
  # wide -> long format
  responses_df <- responses_df %>%
    pivot_longer(
      cols = starts_with(c(
        "loop_counter", "cond", "pic", "mean", "sd"
      )),
      names_to = c(".value", "pic_order"),
      names_pattern = "(.*?)(\\d+)$"
    ) %>%
    drop_na(loop_counter, cond, pic, mean, sd)  # Dropping rows with missing values in these columns
  
  responses_df <- responses_df %>%
    # string -> numeric
    mutate(mean = as.numeric(as.character(mean)),
           sd = as.numeric(as.character(sd)))
  
  # DROPPING two individuals who never moved the variance slider.
  # Calculate the standard deviation for each participant
  responses_df <- responses_df %>%
    group_by(id) %>%
    mutate(sdAnswers = sd(sd, na.rm = TRUE)) %>%
    ungroup()
  
  responses_df <- responses_df %>%
    filter(sdAnswers != 0) %>%
    select(-sdAnswers)
  
  # Merge the answers with predictions
  all_data_df <- responses_df %>%
    full_join(answers_df, by = "pic")
  
  all_data_df <- all_data_df %>%
    mutate(loop_counter = as.numeric(as.character(loop_counter))) %>%
    filter(loop_counter >= 1)
  
  # Extract picture number.
  all_data_df <- all_data_df %>%
    mutate(pic_clean = str_replace_all(pic, "[cflng.jpe]", ""),
           pic_num = as.numeric(pic_clean)) %>%
    select(-pic_clean)
  
  # Let's get things into variances as well.
  all_data_df <- all_data_df %>%
    mutate(
      var = sd ^ 2,
      var_true = sd_true ^ 2,
      var_true_within = sd_true_within ^ 2,
      var_true_model = var_true - var_true_within
    )
  
  # MSE & Error
  all_data_df <- all_data_df %>%
    mutate(
      mse_true = var_true + (mean - mean_true) ^ 2,
      sd_true_from_RMSE = sqrt(mse_true),
      var_true_added_from_wrong = mse_true - var_true,
      sd_true_added_from_wrong = sd_true_from_RMSE - sd_true
    )
  
  # Calculating the average of sd/var by figure.
  all_data_df <- all_data_df %>%
    group_by(pic) %>%
    mutate(avg_sd = mean(sd, na.rm = TRUE),
           avg_var = mean(var, na.rm = TRUE)) %>%
    ungroup()
  
  all_data_df <- all_data_df %>%
    group_by(pic) %>%
    mutate(
      avg_mean = mean(mean, na.rm = TRUE),
      avg_sd_of_mean = sd(mean, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(avg_var_of_mean = avg_sd_of_mean ^ 2)
  
  # Are people generally right in terms of mean?
  all_data_df$avg_var_of_mean_less_ith_id <- NA # Initialize
  
  unique_ids <- unique(all_data_df$id)
  unique_pics <- unique(all_data_df$pic)
  
  # Calculate variance excluding current individual for each pic.
  for (i in unique_ids) {
    for (p in unique_pics) {
      # Picture p excluding individual i.
      pth_pic_less_ith_id <-
        all_data_df %>% filter(id != i, pic == p)
      pth_pic_var <- var(pth_pic_less_ith_id$mean, na.rm = TRUE)
      all_data_df$avg_var_of_mean_less_ith_id[all_data_df$id == i &
                                                all_data_df$pic == p] <- pth_pic_var
    }
  }
  all_data_df$avg_sd_of_mean_less_ith_id <-
    sqrt(all_data_df$avg_var_of_mean_less_ith_id)
  
  # So I can run regressions pic by pic (when working with averages):
  # Binary variable indicating the first observation in each pic.
  all_data_df <- all_data_df %>%
    group_by(pic) %>%
    arrange(id) %>%  # Ensure the data is ordered by 'id' within each 'pic' group
    mutate(first = row_number() == 1) %>%
    ungroup()
  
  # Reorder columns
  all_data_df <- all_data_df %>%
    select(
      id,
      pic,
      pic_num,
      pic_order,
      starts_with("mean"),
      starts_with("sd"),
      starts_with("var"),
      starts_with("mse"),
      starts_with("avg"),
      with_line,
      far,
      first
    )
  
  # Sort rows by pic_num, pic, and then id
  all_data_df <- all_data_df %>%
    arrange(pic_num, pic, id)
  
  return(all_data_df)
}
