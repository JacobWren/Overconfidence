# Function to create the plot
makeplot <- function(alldata,
                     slope,
                     tpred,
                     treat = 1,
                     showbest = 0,
                     xlim = c(0, 28)) {
  predates <- alldata$time[alldata$period == "Pre"]
  presales <- alldata$sales[alldata$period == "Pre"]
  par(
    mfrow = c(1, 1),
    mar = c(2.8, 2.8, .5, .5),
    mgp = c(1.6, .5, 0)
  )
  plot(
    predates,
    presales,
    pch = 19,
    ylim = c(60, 140),
    xlab = "Day",
    ylab = "Sales",
    xlim = xlim,
    axes = FALSE
  )
  axis(1)
  axis(
    2,
    at = seq(60, 140, by = 10),
    labels = seq(60, 140, by = 10),
    cex.axis = .8,
    las = 2
  )
  box()
  abline(v = tpred, col = "red", lwd = 3)
  if (treat)
    abline(a = 100, b = slope)
  if (showbest) {
    coefs <- summary(lm(presales ~ predates))$coefficients
    # fixed_intercept <- 100
    # coefs <- summary(lm(I(presales - fixed_intercept) ~ 0 + predates))$coefficients
    abline(a = coefs[1], b = coefs[2], lty = 3)
  }
  abline(h = seq(60, 140, by = 10),
         col = rgb(0, 0, 0, alpha = .5))
  # text(tpred + 1, seq(135, 65, length.out=8),
  #      c("A","B","C","D","E","F","G","H"))
  # arrows(tpred + 1, 138.5, tpred+1, 143, length=.02)
  # arrows(tpred + 1, 61, tpred+1, 57.5, length=.02)
}
