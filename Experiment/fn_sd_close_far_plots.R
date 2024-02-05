# Prediction: people will not adjust to increased model uncertainty (close vs. far).
# Show this visually.

fn_sd_close_far_plots <-
  function(df,
           v_line_width,
           with_color,
           model_color) {
    df_filtered <- df %>%
      filter(first == 1, with_line == 0, pic_num == 3) %>%
      mutate(num = pic_num + far / 2, zero = 0)
    
    plots <- list()
    
    # Get labels
    true_within_label <- fn_get_true_label(FALSE)
    true_model_label <- fn_get_true_label(TRUE)
    
    # Common plot aesthetics
    base_plot <- fn_base_sd_plot(df_filtered)
    
    # Plot 1: Close vs. Far for pic 3 only.
    plots[[1]] <- base_plot +
      geom_segment(aes(
        y = zero,
        yend = sd_true_within,
        color = fn_get_true_label(FALSE)
      ),
      linewidth = v_line_width) +
      geom_segment(aes(
        y = sd_true_within,
        yend = sd_true,
        color = fn_get_true_label(TRUE)
      ),
      linewidth = v_line_width) +
      scale_color_manual(values = setNames(
        c(with_color, model_color),
        c(true_within_label, true_model_label)
      )) +
      scale_x_continuous(limits = c(1.5, 5)) +
      labs(color = "") +
      guides(color = guide_legend(
        title = "",
        keywidth = 3,
        keyheight = 1
      )) +
      theme(
        legend.position = "bottom",
        legend.spacing.x = unit(0.7, "cm"),
        legend.text = element_text(margin = margin(l = 2))
      )
    
    # Plot 2: Again, close vs. far but now for a few pics.
    df_filtered <- df %>%
      filter(first == 1, pic_num < 7, with_line == 0) %>%
      select(avg_sd, sd_true, sd_true_within, pic_num, far) %>%
      mutate(num = pic_num * 3 - 2 + far,
             zero = 0)
    
    base_plot <- fn_base_sd_plot(df_filtered)
    
    plots[[2]] <- base_plot +
      geom_segment(aes(
        y = zero,
        yend = sd_true_within,
        color = fn_get_true_label(FALSE)
      ),
      linewidth = v_line_width) +
      geom_segment(aes(
        y = sd_true_within,
        yend = sd_true,
        color = fn_get_true_label(TRUE)
      ),
      linewidth = v_line_width) +
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
        legend.text = element_text(margin = margin(l = 2))
      )
    
    df_filtered_avg <- df %>%
      filter(first == 1, with_line == 0, pic_num <= 6) %>%
      mutate(pic_num = 1 + (pic_num > 6)) %>%
      select(avg_sd, sd_true, sd_true_within, pic_num, far) %>%
      group_by(pic_num, far) %>%
      summarize(
        avg_sd = mean(avg_sd),
        sd_true = mean(sd_true),
        sd_true_within = mean(sd_true_within),
        .groups = "drop"
      ) %>%
      mutate(num = pic_num + far * 1.5,
             zero = 0) %>%
      ungroup()
    
    dfs <- list(df_filtered, df_filtered_avg) # For plots 3 & 4.
    
    # Plot 3: Close vs Far now partitioned by model/within uncertainty; Plot 4: Aggregate across pics.
    for (i in seq_along(dfs)) {
      df <- dfs[[i]]
      base_plot <- fn_base_sd_plot(df)
      
      plots[[i + 2]] <- base_plot +
        geom_bar(
          data = df %>% filter(far == 0),
          aes(y = avg_sd, fill = "Close"),
          stat = "identity",
          position = position_dodge(width = 0.50),
          width = 0.50
        ) +
        geom_bar(
          data = df %>% filter(far == 1),
          aes(y = avg_sd, fill = "Far"),
          stat = "identity",
          position = position_dodge(width = 0.50),
          width = 0.50
        ) +
        geom_segment(
          data = df %>% filter(far == 0),
          aes(
            y = zero,
            yend = sd_true_within,
            color = fn_get_true_label(FALSE)
          ),
          linewidth = v_line_width
        ) +
        geom_segment(
          data = df %>% filter(far == 1),
          aes(
            y = zero,
            yend = sd_true_within,
            color = fn_get_true_label(FALSE)
          ),
          linewidth = v_line_width
        ) +
        geom_segment(
          data = df %>% filter(far == 0),
          aes(
            y = sd_true_within,
            yend = sd_true,
            color = fn_get_true_label(TRUE)
          ),
          linewidth = v_line_width
        ) +
        geom_segment(
          data = df %>% filter(far == 1),
          aes(
            y = sd_true_within,
            yend = sd_true,
            color = fn_get_true_label(TRUE)
          ),
          linewidth = v_line_width
        ) +
        scale_fill_manual(values = c("Close" = "yellow2", "Far" = "yellow4")) +
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
      if (i == 2) {
        plots[[i + 2]] <-
          plots[[i + 2]] + scale_x_continuous(limits = c(0, 3.5))
      }
    }
    
    # Save the plots
    fn_save_plots(plots, "firstSet")
  }
