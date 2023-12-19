# Function to compute the correct predicted mean/sd for the last period using
# "pre" data.
# With a line_ it just estimates the variance of the residuals and uses the real
# trend for the point estimate. Without a line_ it draws a best fit line_ and then
# bootstraps a standard error (bit longer run time).
# Also returns whether the truth is in a 90/95% confidence interval as a check
# on the coverage probability.

fn_trudist <-
  function(alldat_,
           slope_ = .2,
           line_ = 1,
           draws_ = 10000) {
    # Time period to predict.
    tpred <- nrow(alldat_)
    # Data we can use for predictions.
    dat <- subset(alldat_, period == "Pre")
    sample_size <- nrow(dat)  # n
    deg_freedom = sample_size - 1
    # This small sample correction is specific to normal distributions;
    # not really necessary, however.
    # bias_correction = (sqrt(2 / deg_freedom) * gamma(sample_size / 2) /
    #                      gamma(deg_freedom / 2))
    bias_correction <- 1
    # The "truth" of this simulation; used to compute coverage.
    realsales <-
      alldat_$sales[tpred]  # True point estimate (with noise)
    # Computing Bayesian mean/sd for those who get the line_.
    if (line_ == 1) {
      # the line_
      realtrend <- 100 + slope_ * (1:nrow(dat))
      sdest <- sd(dat$sales - realtrend) / bias_correction
      # point estimate which is trivially correct
      pointest <-
        100 + slope_ * tpred # True point estimate (w/ out noise)
      # Checking whether the real prediction point is in a 90/95% confidence
      # interval.
      cv90 = qt(0.10 / 2, deg_freedom, lower.tail = FALSE)
      lb90 = pointest - cv90 * sdest
      ub90 = pointest + cv90 * sdest
      hit90 <- (realsales >= lb90) * (realsales <= ub90)
      
      cv95 = qt(0.05 / 2, deg_freedom, lower.tail = FALSE)
      lb95 = pointest - cv95 * sdest
      ub95 = pointest + cv95 * sdest
      hit95 <- (realsales >= lb95) * (realsales <= ub95)
      return(list(
        mean = pointest,
        sd = sdest,
        hit90 = hit90,
        hit95 = hit95
      ))
    }
    # Case where the trend line_ is not given.
    if (line_ == 0) {
      # Drawing best fit line_ through the given data.
      fixed_intercept <- 100
      # Subtract the explicit intercept from the regressand and then fit the
      # intercept-free model (simple way to fix the intercept).
      coeftable <- summary(lm(I(sales - fixed_intercept) ~ 0 + time,
                              data = dat))$coef
      pointest <- 100 + coeftable[1, 1] * tpred
      # To get real variance, draw a bunch of slopes with a mean equal to the
      # point estimate and a sd equal to the standard error.
      betaests <- rnorm(draws_, coeftable[1, 1], coeftable[1, 2])
      # Create one prediction for each draw equal to the mean prediction given
      # beta plus an error term with a variance estimated from the residuals
      # conditional on beta.
      preds <- rep(NA, draws_)
      msres <- rep(NA, draws_)
      for (i in 1:draws_) {
        drawest <- 100 + betaests[i] * tpred
        res <- (dat$sales - (100 + betaests[i] * (1:nrow(dat))))
        msres[i] <- sd(res)
        preds[i] <- drawest + rnorm(1, 0, msres[i])
      }
      sdest <- sd(preds) / bias_correction
      avgsd <- mean(msres) / bias_correction
      bestsd <-
        sd(dat$sales - (100 + coeftable[1, 1] * (1:nrow(dat)))) / bias_correction
      # Check confidence intervals
      cv90 = qt(0.10 / 2, deg_freedom, lower.tail = FALSE)
      hit90 <-
        (realsales >= pointest - cv90 * sdest) * (realsales <= pointest + cv90 *
                                                    sdest)
      cv95 = qt(0.05 / 2, deg_freedom, lower.tail = FALSE)
      hit95 <-
        (realsales >= pointest - cv95 * sdest) * (realsales <= pointest + cv95 *
                                                    sdest)
      return(
        list(
          mean = pointest,
          sd = sdest,
          avgsd = avgsd,
          bestsd = bestsd,
          hit90 = hit90,
          hit95 = hit95
        )
      )
    }
  }