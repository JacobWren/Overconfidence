# Function to compute the correct predicted mean/sd for the last period
# using pre data.
# With a line it just estimates the variance of the residuals
# And uses the real trend for the point estimate
# Without a line it draws a best fit line and then bootstraps
# a standard error
# Also returns whether the truth is in a 90/95% confidence interval
# to check for correct coverage.
trudist <- function(alldat,
                    slope = .2,
                    line = 1,
                    draws = 10000) {
  # time period to predict
  tpred <- nrow(alldat)
  # data we can use to predict
  dat <- subset(alldat, period == "Pre")
  sample_size <- nrow(dat)
  df = sample_size - 1
  # Only applies for normal distributions.
  bias_correction = (sqrt(2 / df) * gamma(sample_size / 2) / gamma(df /
                                                                     2))
  # the "truth" of this simulation
  # Just used to correct coverage
  realsales <-
    alldat$sales[tpred]  # True point estimate (with noise)
  # Computing Bayesian mean/sd for those who get the line
  if (line == 1) {
    # the line
    realtrend <- 100 + slope * (1:nrow(dat))
    sdest <- sd(dat$sales - realtrend) / bias_correction
    # point estimate which is trivially correct
    pointest <-
      100 + slope * tpred # True point estimate (w/ out noise)
    # checking whether the real prediction point is in a 90/95%
    # confidence interval
    cv90 = qt(0.10 / 2, df, lower.tail = FALSE)
    lb90 = pointest - cv90 * sdest
    ub90 = pointest + cv90 * sdest
    hit90 <- (realsales >= lb90) * (realsales <= ub90)
    
    cv95 = qt(0.05 / 2, df, lower.tail = FALSE)
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
  # Case where the trend line is not given
  if (line == 0) {
    # Drawing best fit line through the given data
    fixed_intercept <- 100
    # Subtract the explicit intercept from the regressand and then fit the
    # intercept-free model:
    coeftable <- summary(lm(I(sales - fixed_intercept) ~ 0 + time,
                            data = dat))$coef
    pointest <- 100 + coeftable[1, 1] * tpred
    # To get real variance, drawing a bunch of slopes with a
    # mean equal to the point estimate
    # and a sd equal to the standard error
    betaests <- rnorm(draws, coeftable[1, 1], coeftable[1, 2])
    # creating one prediction for each draw
    # equal to the mean prediction given this beta
    # plus an error term with a variance estimated from the
    # residuals given this beta
    preds <- rep(NA, draws)
    msres <- rep(NA, draws)
    for (i in 1:draws) {
      drawest <- 100 + betaests[i] * tpred
      res <- (dat$sales - (100 + betaests[i] * (1:nrow(dat))))
      msres[i] <- sd(res)
      preds[i] <- drawest + rnorm(1, 0, msres[i])
    }
    sdest <- sd(preds) / bias_correction
    avgsd <- mean(msres) / bias_correction
    bestsd <-
      sd(dat$sales - (100 + coeftable[1, 1] * (1:nrow(dat)))) / bias_correction
    # checking confidence intervals
    cv90 = qt(0.10 / 2, df, lower.tail = FALSE)
    hit90 <-
      (realsales >= pointest - cv90 * sdest) * (realsales <= pointest + cv90 *
                                                  sdest)
    cv95 = qt(0.05 / 2, df, lower.tail = FALSE)
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