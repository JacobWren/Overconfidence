# General helpers.

fn_help <- function(model, dataset, cluster) {
  # A function to extract and print variable names from the regression model.
  
  if (cluster == "single") {
    print("Level: Aggregate \n")
  }
  else if (cluster == "double") {
    print("Level: Individual \n")
  }
  
  # Extracting model formula
  model_formula <- formula(model)
  
  # Extracting dependent variable
  dependent_var <- all.vars(model_formula)[1]
  
  # Extracting independent variables
  independent_vars <- all.vars(model_formula)[-1]
  if (length(independent_vars) == 0 || independent_vars[1] == "1") {
    independent_vars <- "None (Intercept only model)"
  }
  print(
    "________________________________________________________________________________________________"
  )
  # Print dependent and independent variable names
  cat("Dependent Variable:", dependent_var, "\n")
  cat("Independent Variables:", independent_vars, "\n")
  # Print the number of observations
  cat("Number of Observations:", nrow(dataset), "\n")
}


# Horizontally glue two plots together.
fn_combine_plots <- function(plot1, plot2, ncol = 2, align = 'v', axis = 'tb', rel_widths = c(2, 2)) {
  combined_plot <- plot_grid(
    plot1,
    plot2,
    ncol = ncol,
    align = align,
    axis = axis,
    rel_widths = rel_widths
  )
  
  return(combined_plot)
}