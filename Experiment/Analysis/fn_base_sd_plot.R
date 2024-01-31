# Common plot aesthetics

fn_base_sd_plot <- function() {
  base_plot <- ggplot() +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", color = "white"),
      panel.background = element_rect(fill = "white", color = "white"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    labs(x = "", y = "Standard Deviation", title = "")
  
  return(base_plot)
}


# SD line plot
fn_line_sd_plot <- function(df, base_plot, v_line_width, text_size, leg_title, line_color) {
  plot <- base_plot +
    geom_segment(
      data = df,
      aes(
        x = num,
        xend = num,
        y = zero,
        yend = sd_true,
        color = truth_label
      ),
      linewidth = v_line_width
    ) +
    # scale_linetype_manual(values = "solid") +
    scale_color_manual(values = line_color) +
    labs(color = "") +
    guides(color = guide_legend(keywidth = 3)) +
    theme(
      legend.text = element_text(size = text_size),
      legend.title = element_text(size = 13),
      legend.key.size = unit(leg_title, 'cm')
    )
  
  return(plot)
}


# Label helper
get_true_label <- function(is_model) {
  if (is_model) {
    return("True: Model")
  } else {
    return("True: Within")
  }
}















