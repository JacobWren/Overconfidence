fn_reg_variable_names <- function(model, dataset) {
  # A function to extract and print variable names from the regression model.
  
  # Extracting model formula
  model_formula <- formula(model)
  
  # Extracting dependent variable
  dependent_var <- all.vars(model_formula)[1]
  
  # Extracting independent variables
  independent_vars <- all.vars(model_formula)[-1]
  if(length(independent_vars) == 0 || independent_vars[1] == "1") {
    independent_vars <- "None (Intercept only model)"
  }
  print("________________________________________________________________________________________________")
  # Print dependent and independent variable names
  cat("Dependent Variable:", dependent_var, "\n")
  cat("Independent Variables:", independent_vars, "\n")
  # Print the number of observations
  cat("Number of Observations:", nrow(dataset), "\n")
}
