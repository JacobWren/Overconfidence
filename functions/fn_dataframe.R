source("functions\fn_true_dist.R")

# Make Data Frame
data_frame <- function(lowvarv2, morevarv2, nolinev2,
                       nolinev2) {
  df <- data.frame(
    trainnum = c(1, 2, 3),
    mean = c(
      trudist(
        alldat = lowvarv2,
        slope = 1.6,
        line = 1
      )$mean,
      trudist(
        alldat = morevarv2,
        slope = -.3,
        line = 1
      )$mean,
      trudist(
        alldat = nolinev2,
        slope = .4,
        line = 0
      )$mean
    ),
    sd = c(
      trudist(
        alldat = lowvarv2,
        slope = 1.6,
        line = 1
      )$sd,
      trudist(
        alldat = morevarv2,
        slope = -.3,
        line = 1
      )$sd,
      trudist(
        alldat = nolinev2,
        slope = .4,
        line = 0
      )$sd
    )
  )
  return(df)
}
