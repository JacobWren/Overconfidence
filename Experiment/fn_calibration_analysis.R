source("../Calibration/fn_calibration.R")
source("../Helpers/fn_help.R")


fn_calibration_analysis <- function(df_indvl, df_agg) {
  # JAKE: going to skip these calibration regressions for now since Ned didn't ask for it...
  # INDIVIDUAL
  # idvl_calibration_regs <- fn_calibration_regs(df_indvl)
  
  # Traditional calibration bin scatter plots.
  # Individual level
  idvl_bin_scatter <-
    fn_calibration_bin_scatter(df_indvl, "p", "Individual", case = "Experiment")
  # Aggregate level
  agg_bin_scatter <-
    fn_calibration_bin_scatter(df_agg,
                               "p_agg",
                               "Aggregate",
                               case = "Experiment",
                               cluster = "single")
  
  combined_plot <-
    fn_combine_plots(idvl_bin_scatter, agg_bin_scatter)
  # Export the combined plot
  ggsave(
    "Graphs/calibrationBin.png",
    combined_plot,
    device = "png",
    width = 7,
    height = 3
  )
}
