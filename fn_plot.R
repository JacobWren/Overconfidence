# Two plot function.

# Plot parameters.
fn_plot_params <- function() {
  par(
    mfrow = c(1, 1),
    mar = c(2.8, 2.8, .5, .5),
    mgp = c(1.6, .5, 0)
  )
  return(list(xlab = "Day", ylab = "Sales"))
}

# Function to create the primary plots.
fn_makeplot <-
  function(alldata_,
           slope_,
           tpred_,
           treat_ = 1,
           showbest_ = 0,
           xlim_ = c(0, 28),
           plot_name_,
           text_,
           text_letter_ = "",
           width_ = 480) {
    jpeg(paste0(plot_name_, ".jpeg"),
         height = 300,
         width = width_)
    predates <- alldata_$time[al
                              ldata_$period == "Pre"]
    presales <- alldata_$sales[alldata_$period == "Pre"]
    pars <- fn_plot_params()
    plot(
      predates,
      presales,
      pch = 19,
      ylim = c(60, 140),
      xlab = pars$xlab,
      ylab = pars$ylab,
      xlim = xlim_,
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
    abline(v = tpred_, col = "red", lwd = 3)
    if (treat_)
      abline(a = 100, b = slope_)
    if (showbest_) {
      coefs <- summary(lm(presales ~ predates))$coefficients
      abline(a = coefs[1], b = coefs[2], lty = 3)
    }
    abline(h = seq(60, 140, by = 10),
           col = rgb(0, 0, 0, alpha = .5))
    if (text_)
      text(5, 135, text_letter_, cex = 3)
    dev.off()
  }

# Function to create "range" plot.
fn_rangeplot <- function(params_, xlim_) {
  jpeg("range.jpeg", height = 300)
  pars <- fn_plot_params()
  plot(
    0:params_['n'],
    60:80,
    pch = 19,
    ylim = c(60, 140),
    xlab = pars$xlab,
    ylab = pars$ylab,
    xlim = xlim_,
    col = "white",
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
  abline(v = params_['n'],
         col = "red",
         lwd = 3)
  abline(h = seq(60, 140, by = 10),
         col = rgb(0, 0, 0, alpha = .3))
  for (rs in c(-2, 2)) {
    lines(c(0, params_['n']), c(100, 100 + params_['n'] * rs), lwd = 3)
  }
  for (rs in runif(6, -2, 2)) {
    lines(c(0, params_['n']), c(100, 100 + params_['n'] * (rs)), lwd = 1)
  }
  text(18, 100, "?", cex = 3, col = "dark grey")
  dev.off()
}