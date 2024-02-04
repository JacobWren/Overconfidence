# True Avg. SD > Avg. estimates of SD.
# Show this visually.


fn_sd_plots <-
  function(df, v_line_width, with_color, model_color, fill_color) {
    # Filter to one row per pic.
    df_filtered <- df %>%
      filter(first == 1,!with_line) %>%
      select(avg_sd, sd_true, sd_true_within, pic_num, far)
    
    # Average across far/close.
    df_collapsed <- df_filtered %>%
      group_by(pic_num) %>%
      summarize(across(c(avg_sd, sd_true, sd_true_within, far), ~ mean(., na.rm = TRUE)))
    
    # Create additional plot variables
    df_collapsed <- df_collapsed %>%
      arrange(sd_true) %>%
      mutate(num = row_number() * 1.5,
             zero = 0,
             truth_label = "True")
    
    plots <- list()
    base_plot <- fn_base_sd_plot(df_collapsed)
    
    # Plot 1: True SD
    plots[[1]] <-
      fn_line_sd_plot(base_plot, v_line_width,  'black')
    
    # Plot 2: True SD overlayed w/ participant estimates
    plots[[2]] <- base_plot +
      geom_bar(aes(y = avg_sd, fill = "Estimate"),
               stat = "identity") +
      geom_segment(aes(
        y = zero,
        yend = sd_true,
        linetype = "True"
      ),
      size = v_line_width) +
      scale_fill_manual(values = fill_color) +
      labs(fill = "", linetype = "") +
      guides(linetype = guide_legend(keywidth = 3)) +
      theme(legend.title = element_text(size = 13),)
    
    # Get labels
    true_within_label <- fn_get_true_label(FALSE)
    true_model_label <- fn_get_true_label(TRUE)
    
    # Plot 3: True SD partitioned: within and model
    plots[[3]] <- base_plot +
      geom_segment(aes(
        y = zero,
        yend = sd_true_within,
        color = fn_get_true_label(FALSE)
      ),
      size = v_line_width) +
      geom_segment(aes(
        y = sd_true_within,
        yend = sd_true,
        color = fn_get_true_label(TRUE)
      ),
      size = v_line_width) +
      scale_color_manual(values = setNames(
        c(with_color, model_color),
        c(true_within_label, true_model_label)
      )) +
      labs(color = "") +
      guides(color = guide_legend(
        title = "",
        keywidth = 3,
        keyheight = 1
      )) +
      theme(
        legend.position = "bottom",
        legend.spacing.x = unit(0.7, "cm"),
        legend.text = element_text(margin = margin(l = 2))  # Adjust space between legend key and label
      )
    
    # Plot 4: True SD partitioned: within and model overlayed w/ participant estimates
    plots[[4]] <- base_plot +
      geom_bar(aes(y = avg_sd, fill = "Estimate"),
               stat = "identity") +
      geom_segment(aes(
        y = zero,
        yend = sd_true_within,
        color = fn_get_true_label(FALSE)
      ),
      size = v_line_width) +
      geom_segment(aes(
        y = sd_true_within,
        yend = sd_true,
        color = fn_get_true_label(TRUE)
      ),
      size = v_line_width) +
      scale_fill_manual(values = fill_color) +
      scale_color_manual(values = setNames(
        c(with_color, model_color),
        c(true_within_label, true_model_label)
      )) +
      labs(fill = "", color = "") +
      guides(
        fill = guide_legend(
          title = "",
          keywidth = 1.7,
          keyheight = 0.7,
          keyspacing = unit(1, "lines")
        ),
        color = guide_legend(
          title = "",
          keywidth = 3,
          keyheight = 0.8
        )
      ) +
      theme(
        legend.position = "bottom",
        legend.spacing.x = unit(0.7, "cm"),
        legend.text = element_text(margin = margin(l = 2))
      )
    
    # Save the plots
    fn_save_plots(plots, "all")
  }
