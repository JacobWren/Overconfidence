# Call all SD slide plots (sequential).

source("fn_sd_plots.R")
source("fn_sd_close_far_plots.R")
source("fn_sd_within_only_plots.R")
source("fn_mse_plots.R")
source("fn_base_sd_plot.R")


fn_sd_slide_plots <- function(df) {
  # Set some parameters that we want to be consistent across figures.
  v_line_width <- 1.6
  with_color <- "darkorange"
  model_color <- "blue"
  fill_color <- "pink"
  
  # SD figs: Total vs. within vs. model.
  fn_sd_plots(df, v_line_width, with_color, model_color, fill_color)
  
  # Contrast "close" w/ "far".
  fn_sd_close_far_plots(df, v_line_width, model_color, fill_color)
  
  # Within model uncertainty only.
  fn_sd_within_only(df, v_line_width, with_color, fill_color)
  
  # Doesn't appear in slides or paper??
  # fn_mse_plots(
  #   df,
  #   v_line_width,
  #   with_color,
  #   model_color,
  #   fill_color
  # )
}