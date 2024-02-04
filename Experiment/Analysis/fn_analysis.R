source("../../Regressions/fn_regress_and_test.R")
source("../../Regressions/fn_regression_table.R")

# Regression tables:
# (1) Mistakes in variance
# (2) Also account for the wrong-mean effect
# (3) Another way: Can we predict error?
# (4/5) Errors related to disagreement -- (4): in terms of variance & (5): in terms of MSE

fn_analysis <-
  function(df) {
    names(df)[names(df) == "pic"] <- "event" # Same idea...
    case <- "Experiment"
    # How much people account for both types of uncertainty: within and across model.
    partitioned_var_reg <-
      fn_regress_and_test(df, case = case, error_as_dv = FALSE)
    # Now, account for the wrong-mean effect.
    partitioned_var_mse_reg <-
      fn_regress_and_test(df,
                          case = case,
                          wrong_mean = TRUE,
                          error_as_dv = FALSE)
    
    # Add in error terms.
    df <- df %>%
      mutate(
        error_in_var = var_true - var,
        error_in_mse = mse_true - var,
        error_in_est = (mean - mean_true) ^ 2
      )
    
    # Regression of over precision on model-uncertainty.
    error_var_reg <-
      fn_regress_and_test(df, case = case, wrong_mean = FALSE)
    
    df_no_line <- df %>%
      filter(with_line == 0)
    # Are people's errors related to disagreement?
    # First, variance estimate being off by disagreement (of -i).
    disagreement_var_reg <-
      fn_regress_and_test(
        df_no_line,
        case = case,
        wrong_mean = FALSE,
        disagreement = TRUE
      )
    # Now, what about the MSE being off
    disagreement_mse_reg <-
      fn_regress_and_test(
        df_no_line,
        case = case,
        wrong_mean = TRUE,
        disagreement = TRUE
      )
    
    # Write to tables.
    fn_regression_table(partitioned_var_reg, "withinAcross", column_label = case)
    
    fn_regression_table(
      partitioned_var_mse_reg,
      "withinAcrossMSE",
      column_label = case,
      covariate_labels = c(
        "Within-model Variance",
        "Across-model Variance",
        "Wrong Mean"
      )
    )
    
    fn_regression_table(
      error_var_reg,
      "regErrorOnAcross",
      column_label = case,
      covariate_labels = c("Across-model Variance")
    )
    # One file for these.
    fn_regression_table(
      disagreement_var_reg,
      "disagreementAndError",
      column_label = case,
      covariate_labels = c("Disagreement (−i)")
    )
    fn_regression_table(
      disagreement_mse_reg,
      "disagreementAndError",
      column_label = case,
      covariate_labels = c("Disagreement (−i)"),
      stack = TRUE
    )
    
  }