# Common plot aesthetics


fn_base_sd_plot <- function(data, y_label = "Standard Deviation") {
  base_plot <-
    ggplot(data, aes(x = num, xend = num)) + # 'xend' might go unused.
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", color = "white"),
      panel.background = element_rect(fill = "white", color = "white"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = 15),
      axis.title.y = element_text(size = 16),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 15),
      legend.key.size = unit(0.65, 'cm')
    ) +
    labs(x = "", y = y_label, title = "")
  
  return(base_plot)
}


# SD line plot
fn_line_sd_plot <- function(base_plot, v_line_width, line_color) {
  plot <- base_plot +
    geom_segment(aes(y = zero,
                     yend = sd_true,
                     color = truth_label),
                 linewidth = v_line_width) +
    scale_color_manual(values = line_color) +
    labs(color = "") +
    guides(color = guide_legend(keywidth = 3)) +
    theme(legend.title = element_text(size = 13),)
  
  return(plot)
}


# Label helper
fn_get_true_label <- function(is_model) {
  if (is_model) {
    return("True: Model")
  } else {
    return("True: Within")
  }
}


fn_save_plots <-
  function(plots,
           prefix,
           start_seq = 1,
           end_seq = length(plots)) {
    seq_nums <-
      seq(from = start_seq,
          to = end_seq,
          length.out = length(plots))
    
    # Generate output paths.
    output_paths <- paste0("Graphs/", prefix, seq_nums, ".png")
    
    for (i in seq_along(plots)) {
      ggsave(
        file = output_paths[i],
        plot = plots[[i]],
        width = 10,
        height = 6,
        bg = "white"
      )
    }
  }
