source("../../Calibration/fn_calibration.R")

fn_calibration_plots <- function(df_indvl) {
  browser()
  smoothing_param <-
    1.475 # Used for smoothing scatterplot via a spline.
  data_idvl_smoothed <-
    fn_smooth(df_indvl, "p", "true_bin_smoothed", smoothing_param)
  
}