fn_sd_plots <- function(df) {
  
  # Filter and keep specific columns
  df_filtered <- df %>%
    filter(first == 1, with_line == 0) %>%
    select(avg_sd, sd_true, sd_true_within, pic_num, far)
  
  # Collapse the data
  df_collapsed <- df_filtered %>%
    group_by(picNum) %>%
    summarize(across(c(avgSd, sdTrueTotal, sdTrueWithin, far), mean, na.rm = TRUE))
  
  # Create additional variables and sort
  df_collapsed <- df_collapsed %>%
    mutate(num = picNum * 3 - 2 + far,
           zero = 0) %>%
    arrange(sdTrueTotal) %>%
    mutate(num = row_number() * 1.5)
  
  # Create the plot
  p <- ggplot(df_collapsed, aes(x = num)) +
    geom_errorbar(aes(ymin = zero, ymax = sdTrueTotal), color = "grey", size = 1.2) +
    scale_x_continuous(breaks = 1, labels = " ") +
    labs(x = "", y = "Standard Deviation", title = "") +
    theme_minimal()
  
  # Save the plot
  ggsave("Graphs/all1.png", plot = p, width = 10, height = 6)
  
  # Return the plot object for additional manipulation if needed
  return(p)
}
