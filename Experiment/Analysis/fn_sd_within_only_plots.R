# Remove all model uncertainty by providing the line.


fn_sd_within_only <-
  function(df, v_line_width, with_color, fill_color) {
    plots <- list()
    
    # Plot 1: SD: no model uncertainty
    df_prepared <- df %>%
      filter(first == 1, with_line == 1) %>%
      select(avg_sd, sd_true, sd_true_within, pic_num, far) %>%
      group_by(pic_num) %>% # Average over far/close
      summarize(
        avg_sd = mean(avg_sd),
        sd_true = mean(sd_true),
        sd_true_within = mean(sd_true_within),
        far = mean(far)
      ) %>%
      arrange(sd_true) %>%
      mutate(num = row_number() * 1.5,
             zero = 0,
             truth_label = fn_get_true_label(FALSE)) %>%
      ungroup()
    
    x_min <- min(df_prepared$num)
    x_max <- max(df_prepared$num)
    
    # Adjust the limits slightly beyond the actual data range
    x_lim_min <- x_min - 1
    x_lim_max <- x_max + 1
    
    # Common plot aesthetics
    base_plot <- fn_base_sd_plot(df_prepared)
    
    plots[[1]] <-
      fn_line_sd_plot(base_plot, v_line_width, with_color)
    
    # Plot 2: Now overlay participant's (avg.) estimates.
    # Prediction: people will be generally correct on these problems.
    plots[[2]] <- base_plot +
      geom_bar(aes(y = avg_sd, fill = "Estimate"),
               stat = "identity") +
      geom_segment(
        aes(
          y = zero,
          yend = sd_true,
          linetype = fn_get_true_label(FALSE)
        ),
        size = v_line_width,
        color = with_color
      ) +
      scale_fill_manual(values = fill_color) +
      labs(fill = "", linetype = "") +
      guides(linetype = guide_legend(keywidth = 3)) +
      theme(legend.title = element_text(size = 13), )
    # Save the plots
    fn_save_plots(plots, "lines")
  }