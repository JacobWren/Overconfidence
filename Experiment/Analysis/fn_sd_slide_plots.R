# Call all SD slide plots (sequential).
source("fn_sd_plots.R")
source("fn_sd_close_far_plots.R")
source("fn_sd_within_only.R")
source("fn_base_sd_plot.R")


fn_sd_slide_plots <- function(df) {
  # Common plot aesthetics
  base_plot <- fn_base_sd_plot()
  
  # Set some parameters that we want to be consistent across figures.
  v_line_width <- 1.6
  text_size <- 15
  leg_title <- 0.65
  with_color <- "darkorange"
  model_color <- "blue"
  fill_color <- "pink"
  
  # SD figs: Total vs. within vs. model.
  fn_sd_plots(
    df,
    base_plot,
    common_params,
    v_line_width,
    text_size,
    leg_title,
    with_color,
    model_color,
    fill_color
  )
  
  # Contrast "close" w/ "far".
  fn_sd_close_far_plots(
    df,
    base_plot,
    common_params,
    v_line_width,
    text_size,
    leg_title,
    with_color,
    model_color,
    fill_color
  )
  
  # Within model uncertainty only.
  fn_sd_within_only(
    df,
    base_plot,
    common_params,
    v_line_width,
    text_size,
    leg_title,
    with_color,
    model_color,
    fill_color
  )
}