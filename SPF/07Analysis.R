# Get and set working directory
current_dir <- getwd()
if (!grepl("SPF$", current_dir)) {
  setwd("SPF")
}
source("init_script.R")
source("fn_calibration.R")
source("fn_variance.R")

# Individual level
# combinedVarInfoWithBins => every forecaster and every bin
# combinedVarInfoNotCompressed => compressed over bins + has statistics like sq error, disagreement
# combinedVarInfo => Not really used => all averages compressed over event-time-forecasting variable

# Agg level
# combinedVarInfoAggWithBins => every bin (agg beliefs have been compressed over forecasters)
# combinedVarInfoAgg => compressed over bins + has statistics like sq error, disagreement

data_idvl <- read.csv("Data/combinedVarInfoWithBins.csv") %>%
  fn_generate_trueBin_var()

# INDIVIDUAL
idvl_calibration_regs <- fn_calibration_regs(data_idvl)
cat("\n\n")
print(
  "________________________________________________________________________________________________"
)

# AGGREGATE
data_agg <- read.csv("Data/combinedVarInfoAggWithBins.csv") %>%
  fn_generate_trueBin_var()
names(data_agg)[names(data_agg) == "pAgg"] <- "p" # For table
agg_calibration_regs <-
  fn_calibration_regs(data_agg, cluster_by_event_only = TRUE)

# Create the Calibration Table
stargazer_table_calib <-
  stargazer(
    idvl_calibration_regs$model,
    agg_calibration_regs$model,
    type = "latex",
    header = FALSE,
    se = list(
      idvl_calibration_regs$robust_se,
      agg_calibration_regs$robust_se
    ),
    add.lines = list(
      c(
        "P(Beta=1)",
        sprintf("%.4f", idvl_calibration_regs$p_value_p),
        sprintf("%.4f", agg_calibration_regs$p_value_p)
      ),
      c(
        "P(Cons=0)",
        sprintf("%.4f", idvl_calibration_regs$p_value_intercept),
        sprintf("%.4f", agg_calibration_regs$p_value_intercept)
      )
    ),
    covariate.labels = c("Prob", "Constant"),
    omit.stat = c("f", "ser", "adj.rsq"),
    # Drop
    column.labels = c("Individual", "Average"),
    model.numbers = FALSE,
    star.cutoffs = NA,
    # Disable significance stars
    dep.var.labels.include = FALSE,
    # Remove "Dependent variable" label
    dep.var.caption = ""  # Remove the dependent variable caption
  )

# Write the table to a .tex file
cat(stargazer_table_calib, file = "Results/calibration.tex")

# Calibration: Plots

names(data_agg)[names(data_agg) == "p"] <- "pAgg" # Command z.
# Smoothing operation on the "trueBin" variable against prob.
# Individual level
data_idvl_smoothed <-
  fn_smooth(data_idvl, "p", "trueBinSmoothed", 1.475)
# Aggregate level
data_agg_smoothed <-
  fn_smooth(data_agg, "pAgg", "trueBinSmoothedAgg", 1.475)

# Append the dfs.
combined_smoothed <-
  bind_rows(data_idvl_smoothed, data_agg_smoothed)
write.csv(combined_smoothed,
          "Data/combined_smoothed_data.csv",
          row.names = FALSE)

# Calibration plot
calibration_smoothed_fig <- ggplot() +
  geom_line(
    data = combined_smoothed,
    aes(x = p, y = p, color = "Bayesian"),
    size = 0.90,
    linetype = "dashed"
  ) +
  geom_line(
    data = combined_smoothed,
    aes(x = p, y = trueBinSmoothed, color = "Individual"),
    size = 0.85,
    alpha = .85
  ) +
  geom_line(
    data = combined_smoothed,
    aes(x = pAgg, y = trueBinSmoothedAgg, color = "Average"),
    size = 0.85,
    alpha = .90
  ) +
  scale_x_continuous("Belief",
                     labels = percent_format(),
                     breaks = seq(0, 1, by = 0.25)) +
  scale_y_continuous("Empirical Probability",
                     labels = percent_format(),
                     breaks = seq(0, 1, by = 0.25)) +
  scale_color_manual(
    values = c(
      "Bayesian" = "darkgrey",
      "Individual" = "navy",
      "Average" = "red"
    ),
    name = "Legend"
  ) +
  scale_linetype_manual(values = c(
    "Bayesian" = "5",
    "Individual" = "solid",
    "Average" = "solid"
  )) + # Adjust linetype pattern
  theme_minimal() +
  theme(
    legend.position = c(0.18, 0.77),
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),
    legend.key = element_blank(),
    # Remove boxes around legend colors
    legend.key.size = unit(.01, "lines"),
    legend.spacing = unit(0.001, "cm"),
    legend.margin = margin(.001, 3, .001, .001),
    legend.box.margin = margin(.01, .3, .01, .3),
    # Reduce legend box margin
    legend.spacing.y = unit(.01, "lines"),
    # Reduce vertical spacing between legend items
    legend.background = element_rect(fill = "white", color = "black"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    # Set plot background to white
    plot.margin = unit(c(1, 1, 1, 1), "lines")
  ) + # Adjust plot margins
  guides(color = guide_legend(override.aes = list(
    size = 11,
    linetype = c("solid", "dashed", "solid")
  )))

# Save the plot to a PDF file
ggsave(
  "Graphs/calibration.png",
  plot = calibration_smoothed_fig,
  device = "png",
  width = 6.75,
  height = 4.75
)

# Traditional calibration bin scatter plots.
# Individual level
idvl_bin_scatter <-
  fn_calibration_bin_scatter(data_idvl, "p", "Individual")
# Aggregate level
agg_bin_scatter <-
  fn_calibration_bin_scatter(data_agg, "pAgg", "Aggregate", cluster_by_event_only =
                               TRUE)
# Combine the plots
combined_plot <-
  plot_grid(
    idvl_bin_scatter,
    agg_bin_scatter,
    ncol = 2,
    align = 'v',
    axis = 'tb',
    rel_widths = c(2, 2)
  )
# Export the combined plot
ggsave(
  "Graphs/calibrationBin.png",
  combined_plot,
  device = "png",
  width = 7,
  height = 3
)

# Variance Regressions
# "Basic" Error On Disagreement Regressions.
data_error <- read.csv("Data/combinedVarInfoNotCompressed.csv")
var_regs_int <- fn_variance_regs(data_error)
var_regs_noInt <-
  fn_variance_regs(data_error, include_intercept = FALSE)

names(data_error)[names(data_error) == "dataSet"] <- "forecastVar"
# Combining groups ==> that is, taking averages over forecasted variables/timeToResolution.
data_error <- data_error %>%
  # Create and modify 'distanceFromResolutionTrucate'.
  mutate(
    distanceFromResolutionTrucate = if_else(distanceFromResolution > 2, 1.875, distanceFromResolution)
  ) %>%
  group_by(forecastVar, distanceFromResolutionTrucate) %>%
  mutate(grpd_forecastVar_distance = cur_group_id()) %>%
  ungroup()

data_collapsed <- data_error %>%
  group_by(grpd_forecastVar_distance) %>%
  # Take averages.
  summarise(
    error = mean(error, na.rm = TRUE),
    sqDevFromRealization = mean(sqDevFromRealization, na.rm = TRUE),
    VarPredicted = mean(VarPredicted, na.rm = TRUE),
    disagreementEVMinusPersoni = mean(disagreementEVMinusPersoni, na.rm = TRUE),
    forecastVar = first(forecastVar)
  )
# Regressions
var_regs_int_avg <-
  fn_variance_regs(data_collapsed, simpleReg = TRUE)
var_regs_noInt_avg <-
  fn_variance_regs(data_collapsed,
                   simpleReg = TRUE,
                   include_intercept = FALSE)

# Completely compressed (i.e., no regression).

# Collapse the Data to Overall Means
data_fully_collapsed <- data_error %>%
  summarise(
    error = mean(error, na.rm = TRUE),
    disagreementEVMinusPersoni = mean(disagreementEVMinusPersoni, na.rm = TRUE)
  )

# Duplicate the single row of means, otherwise won't run the "regession".
data_fully_collapsed <- data_fully_collapsed[rep(1, 2), ]
var_regs_noInt_avgOnly <-
  fn_variance_regs(data_fully_collapsed,
                   simpleReg = TRUE,
                   include_intercept = FALSE)

# Creating the disagreement regression table with stargazer.
mods <-
  list(
    var_regs_int$model,
    var_regs_noInt$model,
    var_regs_int_avg,
    var_regs_noInt_avg,
    var_regs_noInt_avgOnly
  )
stargazer_table_disagg <- stargazer(
  mods,
  type = "latex",
  header = FALSE,
  se = list(
    var_regs_int$robust_se,
    var_regs_noInt$robust_se,
    summary(var_regs_int_avg)$coefficients[, "Std. Error"],
    summary(var_regs_noInt_avg)$coefficients[, "Std. Error"],
    summary(var_regs_noInt_avgOnly)$coefficients[, "Std. Error"]
  ),
  add.lines = list(c(
    "P(Beta=0)",
    sprintf("%.4f", var_regs_int$p_value),
    sprintf("%.4f", var_regs_noInt$p_value),
    sprintf("%.4f", summary(var_regs_int_avg)$coefficients["disagreementEVMinusPersoni", "Pr(>|t|)"]),
    sprintf("%.4f", summary(var_regs_noInt_avg)$coefficients["disagreementEVMinusPersoni", "Pr(>|t|)"]),
    ""
  )),
  covariate.labels = c("Disagreement", "Constant"),
  omit.stat = c("f", "ser", "adj.rsq"),
  # Drop
  column.labels = c("Individual", "est2", "est3", "est4", "est5"),
  model.numbers = FALSE,
  star.cutoffs = NA,
  # Disable significance stars
  dep.var.labels.include = FALSE,
  # Remove "Dependent variable" label
  dep.var.caption = "",
  # Remove the dependent variable caption
  digits = 4
)

# Write the table to a .tex file
cat(stargazer_table_disagg, file = "Results/disagreement.tex")
