source("init_script.R")
source("fn_calibration.R")
source("fn_variance.R")

# Individual level
# combinedVarInfoWithBins => every person and every bin
# combinedVarInfoNotCompressed => compressed over bins + has statistics like sq error, disagreement
# combinedVarInfo => Not really used => all averages compressed over event_time_dataSet
#
# Agg level
# combinedVarInfoAggWithBins => every bin (agg beliefs have been compressed over people)
# combinedVarInfoAgg => compressed over bins + has statistics like sq error, disagreement

# generate_truth_var <- function(df) {
#   df %>% 
#     mutate(truth = ifelse(realization > binL & realization <= binH, TRUE, FALSE))
# }


# START
# data_idvl <- read.csv("Data/combinedVarInfoWithBins.csv") %>%
#   generate_truth_var()


# # INDIVIDUAL
# idvl_calibration_regs <- fn_calibration_regs(data_idvl)
# cat("\n\n")
# print("________________________________________________________________________________________________")

# AGGREGATE
# data_agg <- read.csv("Data/combinedVarInfoAggWithBins.csv") %>%
#   generate_truth_var()
# names(data_agg)[names(data_agg) == "pAgg"] <- "p" # For table
# agg_calibration_regs <- fn_calibration_regs(data_agg, cluster_by_event_only=TRUE)

# # Create the Calibration Table
# stargazer_table <- stargazer(idvl_calibration_regs$model, agg_calibration_regs$model, type = "text",
#                              header = FALSE,
#                              se = list(idvl_calibration_regs$robust_se, agg_calibration_regs$robust_se),
#                              add.lines = list(
#                                c("P(Beta=1)", sprintf("%.4f", idvl_calibration_regs$p_value_p), sprintf("%.4f", agg_calibration_regs$p_value_p)),
#                                c("P(Cons=0)", sprintf("%.4f", idvl_calibration_regs$p_value_intercept), sprintf("%.4f", agg_calibration_regs$p_value_intercept))
#                              ),
#                              covariate.labels = c("Prob", "Constant"),
#                              omit.stat = c("f", "ser", "adj.rsq"),  # Drop
#                              column.labels = c("Individual", "Average"),
#                              model.numbers = FALSE,
#                              star.cutoffs = NA,  # Disable significance stars
#                              dep.var.labels.include = FALSE,  # Remove "Dependent variable" label
#                              dep.var.caption = ""  # Remove the dependent variable caption
# )

# # Write the table to a .tex file
# cat(stargazer_table, file = "Results/calibration.tex")
# END

# Calibration: Plots

# smooth_probs <- function(df, prob_var, smoothed_name, num_bins) {
#   # num_bins: trying to have results similar to STATA's "running" pkg.
#   # Add a new column 'p_bin' to 'df', categorizing 'p' into 'num_bins' bins
#   df %>%
#     mutate(
#       # 'cut' divides 'p' into 'num_bins' bins for grouping; 'include.lowest = TRUE' ensures the lowest value is included
#       p_bin = cut(get(prob_var), breaks = num_bins, include.lowest = TRUE, labels = FALSE)
#     ) %>%
#     # Group the dataframe by the newly created 'p_bin' column
#     group_by(p_bin) %>%
#     # For each group (bin), calculate the mean of 'truth' values, which gives the proportion of TRUEs in each bin
#     mutate(
#       !!smoothed_name := mean(truth, na.rm = TRUE)
#     ) %>%
#     # Remove the grouping, returning the dataframe to its original state
#     ungroup() %>%
#     select(!!sym(prob_var), !!sym(smoothed_name)) %>%  
#     distinct()  # Remove duplicate rows
# }


# # Individual level  
# data_idvl_smoothed <- smooth_probs(data_idvl, "p", "truthSmoothed", num_bins = 80)
# # Aggregate level
# data_agg_smoothed <- smooth_probs(data_agg, "pAgg", "truthSmoothedAgg", num_bins = 60)

# START
# Individual level
# data_idvl_smoothed <- fn_smooth(data_idvl, "p", "truthSmoothed", 1.475)
# # Aggregate level
# data_agg_smoothed <- fn_smooth(data_agg, "pAgg", "truthSmoothedAgg", 1.475)
# 
# # Append the dfs.
# combined_smoothed <- bind_rows(data_idvl_smoothed, data_agg_smoothed)
# write.csv(combined_smoothed, "Data/combined_smoothed_data.csv", row.names = FALSE)
# 
# # Calibration plot
# calibration_smoothed_fig <- ggplot() +
#   geom_line(data = combined_smoothed, aes(x = p, y = p, color = "Bayesian"), size = 0.82) +
#   geom_line(data = combined_smoothed, aes(x = p, y = truthSmoothed, color = "Individual"), size = 0.82) +
#   geom_line(data = combined_smoothed, aes(x = pAgg, y = truthSmoothedAgg, color = "Average"), size = 0.82) +
#   scale_x_continuous("Belief", labels = percent_format(), breaks = seq(0, 1, by = 0.25)) +
#   scale_y_continuous("Empirical Probability", labels = percent_format(), breaks = seq(0, 1, by = 0.25)) +
#   scale_color_manual(values = c("Bayesian" = "darkgrey", "Individual" = "navy", "Average" = "red"), name = "Legend") +
#   theme_minimal() +
#   theme(legend.position = c(0.18, 0.82),
#         legend.direction = "vertical",
#         legend.title = element_blank(),
#         legend.text = element_text(size = 14),
#         legend.key = element_blank(),  # Remove boxes around legend colors
#         legend.key.size = unit(1, "lines"),
#         legend.spacing = unit(0.1, "cm"),
#         legend.margin = margin(3, 3, 4, 4),
#         legend.background = element_rect(fill = "white", color = "black"),
#         axis.title = element_text(size = 14),
#         axis.text = element_text(size = 12)) +
#   guides(color = guide_legend(override.aes = list(size = 14)))
# 
# # Save the plot to a PDF file
# ggsave("Graphs/calibration.png", plot = calibration_smoothed_fig, device = "png", width = 10, height = 6)
# END

# # START
# # Traditional calibration bin scatter plots.
# # Individual level
# idvl_bin_scatter <- fn_calibration_bin_scatter(data_idvl, "p", "Individual")
# # Aggregate level
# agg_bin_scatter <- fn_calibration_bin_scatter(data_agg, "pAgg", "Aggregate", cluster_by_event_only=TRUE)
# # Combine the plots
# combined_plot <- plot_grid(idvl_bin_scatter, agg_bin_scatter, ncol = 2, align = 'v', axis = 'tb', rel_widths = c(2, 2))
# # Export the combined plot
# ggsave("Graphs/calibrationBin.png", combined_plot, device = "png", width = 7, height = 3)
# # END


# Variance Regressions
# "Basic" Error On Disagreement Regressions. 
data_error <- read.csv("Data/combinedVarInfoNotCompressed.csv")
var_regs_int <- fn_variance_regs(data_error)
var_regs_noInt <- fn_variance_regs(data_error, include_intercept=FALSE)

# Combining groups ==> that is, taking averages over forecasted variables (dataSet)/timeToResolution
data_error <- data_error %>%
  # Create and modify 'distanceFromResolutionTrucate'
  mutate(distanceFromResolutionTrucate = if_else(distanceFromResolution > 2, 1.875, distanceFromResolution)) %>%
  group_by(dataSet, distanceFromResolutionTrucate) %>%
  mutate(grpd_dataSet_distance = group_indices()) %>% 
  ungroup()

data_collapsed <- data_error %>%
  group_by(grpd_dataSet_distance) %>%
  # Take averages.
  summarise(
    error = mean(error, na.rm = TRUE),
    sqDevFromRealization = mean(sqDevFromRealization, na.rm = TRUE),
    VarPredicted = mean(VarPredicted, na.rm = TRUE),
    disagreementEVMinusPersoni = mean(disagreementEVMinusPersoni, na.rm = TRUE),
    dataSet = first(dataSet)
  )
# Regressions
var_regs_int_avg <- fn_variance_regs(data_collapsed, simpleReg=TRUE)
var_regs_noInt_avg <- fn_variance_regs(data_collapsed, simpleReg=TRUE, include_intercept=FALSE)


# Completely compressed (i.e., no regression).

# Collapse the Data to Overall Means
data_fully_collapsed <- data_error %>%
  summarise(
    error = mean(error, na.rm = TRUE),
    disagreementEVMinusPersoni = mean(disagreementEVMinusPersoni, na.rm = TRUE)
  )

# Duplicate the single row of means, otherwise won't run the "regession".
data_fully_collapsed <- data_fully_collapsed[rep(1, 2), ]
var_regs_noInt_avgOnly <- fn_variance_regs(data_fully_collapsed, simpleReg=TRUE, include_intercept=FALSE)

# (*) Working on getting the disagreement.tex table. The table beneath was used for a similar table.
stargazer(var_regs_int$model, var_regs_noInt$model, var_regs_int_avg, var_regs_noInt_avg, var_regs_noInt_avgOnly, 
          type = "text", 
          title = "Regression Results",
          covariate.labels = c("Constant", "Disagreement"),
          omit.stat = c("adj.rsq", "f", "ser"),
          single.row = TRUE)

# stargazer_table <- stargazer(idvl_calibration_regs$model, agg_calibration_regs$model, type = "text",
#                              header = FALSE,
#                              se = list(idvl_calibration_regs$robust_se, agg_calibration_regs$robust_se),
#                              add.lines = list(
#                                c("P(Beta=1)", sprintf("%.4f", idvl_calibration_regs$p_value_p), sprintf("%.4f", agg_calibration_regs$p_value_p)),
#                                c("P(Cons=0)", sprintf("%.4f", idvl_calibration_regs$p_value_intercept), sprintf("%.4f", agg_calibration_regs$p_value_intercept))
#                              ),
#                              covariate.labels = c("Prob", "Constant"),
#                              omit.stat = c("f", "ser", "adj.rsq"),  # Drop
#                              column.labels = c("Individual", "Average"),
#                              model.numbers = FALSE,
#                              star.cutoffs = NA,  # Disable significance stars
#                              dep.var.labels.include = FALSE,  # Remove "Dependent variable" label
#                              dep.var.caption = ""  # Remove the dependent variable caption
# )

