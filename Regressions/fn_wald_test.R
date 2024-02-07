# Wald test.

fn_wald_test <- function(coefficients, robust_se, coef_name, hyp_value = 0, display = FALSE) {
  coef <- coefficients[coef_name]
  se <- robust_se[coef_name]
  # Calculates the Wald test statistic to test if the coefficient of coef_name is equal to hyp_value..
  # It uses the formula (Coefficient − Hypothesized Value) ^ 2 / Standard Error ^ 2
  wald_statistic <- (coef - hyp_value) ^ 2 / se ^ 2
  # Calculate p-values for Chi-squared distribution with 1 degree of freedom.
  p_value <- pchisq(wald_statistic, df = 1, lower.tail = FALSE)
  
  if (display) {
    # Print Wald test results
    print(paste(
      "\nWald test for", coef_name, "=", hyp_value, ": Chi-squared statistic =", wald_statistic,
      ", p-value =", p_value, "\n"))
    print("________________________________________________________________________________________________")
  }
  return(p_value)
}
