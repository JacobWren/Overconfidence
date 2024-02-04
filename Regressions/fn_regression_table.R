# Regression output -> .tex table

fn_regression_table <-
  function(mod,
           table_name,
           column_label = "Experiment",
           covariate_labels = c("Within-model Variance", "Across-model Variance"),
           stack = FALSE) {
    file_path <- paste0("Tables/", table_name, ".tex")
    
    # Create the regression table using stargazer
    reg_table <- stargazer(
      list(mod$model),
      type = "latex",
      header = FALSE,
      se = list(mod$robust_se),
      covariate.labels = covariate_labels,
      omit.stat = c("f", "ser", "adj.rsq"),
      column.labels = c(column_label, "Constant"),
      model.numbers = FALSE,
      star.cutoffs = NA,
      dep.var.labels.include = FALSE,
      dep.var.caption = "",
      digits = 4
    )
    
    # Stack tables?
    if (stack) {
      # Read the table content
      prev_table_content <- readLines(file_path, warn = FALSE)
      
      # Concatenate the previous table content with the new table.
      reg_table <-
        paste(c(prev_table_content, reg_table), collapse = "\n")
    }
    
    # Save table.
    cat(reg_table, file = file_path)
  }
