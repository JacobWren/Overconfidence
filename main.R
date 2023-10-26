# Version for early October "big pilot"

# Load functions
source("functions\fn_data.R")
source("functions\fn_true_dist.R")
source("functions\fn_plot.R")
source("functions\fn_coverage.R")


# Test data set with slope .5
test <- makedata(slope = .5)
plot(test$time, test$sales)
trudist(alldat = test,
        slope = .5,
        line = 1)
trudist(alldat = test,
        slope = .5,
        line = 0)

# Plot data
makeplot(
  alldata = test,
  slope = .5,
  tpred = 20,
  xlim = c(0, 19.5)
)


# Training examples v2
# setwd("~/Dropbox/Apps/Overleaf/Thinking and Confidence/_img/trainingv2")
set.seed(2445633)
lowvarv2 <- makedata(slope = 1.6,
                     noisevec = rnorm(20, 0 , 1.5),
                     post = 10)
morevarv2 <- makedata(slope = -.3,
                      noisevec = rnorm(20, 0 , 15),
                      post = 10)
nolinev2 <- makedata(slope = .4,
                     noisevec = rnorm(20, 0, 4),
                     post = 10)


trudist(alldat = lowvarv2,
        slope = 1.6,
        line = 1)


set.seed(24532)
toscale <- rnorm(20, 0, 1)
practice1 <- makedata(slope = -1.1,
                      noisevec = 3 * toscale,
                      post = 10)
practice2 <- makedata(slope = -1.1,
                      noisevec = 22 * toscale,
                      post = 10)
practice3 <- makedata(slope = .5,
                      noisevec = rnorm(20, 0, 4),
                      post = 10)
practice4 <- makedata(slope = -.5,
                      noisevec = rnorm(20, 0, 15),
                      post = 10)

toscale_comp <- rnorm(20, 0, 1)
comp1 <- makedata(slope = .2,
                  noisevec = 2 * toscale_comp,
                  post = 10)
comp2 <- makedata(slope = .2,
                  noisevec = 8 * toscale_comp,
                  post = 10)


jpeg("train1.jpeg", height = 300)
makeplot(
  alldata = lowvarv2,
  slope = 1.6,
  tpred = 20,
  xlim = c(0, 19.5)
)
dev.off()

jpeg("train2.jpeg", height = 300)
makeplot(
  alldata = morevarv2,
  slope = -.3,
  tpred = 20,
  xlim = c(0, 19.5)
)
dev.off()

jpeg("train3.jpeg", height = 300)
makeplot(
  alldata = nolinev2,
  treat = 0,
  tpred = 20,
  xlim = c(0, 19.5)
)
dev.off()

jpeg("practice1.jpeg", height = 300)
makeplot(
  alldata = practice1,
  slope = -1.1,
  tpred = 20,
  xlim = c(0, 19.5)
)
dev.off()

jpeg("practice2.jpeg", height = 300)
makeplot(
  alldata = practice2,
  slope = -1.1,
  tpred = 20,
  xlim = c(0, 19.5)
)
dev.off()

jpeg("practice3.jpeg", height = 300)
makeplot(
  alldata = practice3,
  treat = 0,
  tpred = 20,
  xlim = c(0, 19.5)
)
dev.off()

jpeg("practice4.jpeg", height = 300)
makeplot(
  alldata = practice4,
  treat = 0,
  tpred = 20,
  xlim = c(0, 19.5)
)
dev.off()

jpeg("comp1.jpeg", height = 300)
makeplot(
  alldata = comp1,
  treat = 0,
  tpred = 20,
  xlim = c(0, 19.5)
)
text(5, 135, "A", cex = 3)
dev.off()

jpeg("comp2.jpeg", height = 300)
makeplot(
  alldata = comp2,
  treat = 0,
  tpred = 20,
  xlim = c(0, 19.5)
)
text(5, 135, "B", cex = 3)
dev.off()

jpeg("range.jpeg", height = 300)
par(
  mfrow = c(1, 1),
  mar = c(2.8, 2.8, .5, .5),
  mgp = c(1.6, .5, 0)
)
plot(
  0:20,
  60:80,
  pch = 19,
  ylim = c(60, 140),
  xlab = "Day",
  ylab = "Sales",
  xlim = c(0, 19.5),
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
abline(v = 20, col = "red", lwd = 3)
abline(h = seq(60, 140, by = 10), col = rgb(0, 0, 0, alpha = .3))
lines(c(0, 20), c(100, 100 + 20 * 2), lwd = 3)
lines(c(0, 20), c(100, 100 + 20 * (-2)), lwd = 3)
for (rs in runif(6, -2, 2)) {
  lines(c(0, 20), c(100, 100 + 20 * (rs)), lwd = 1)
}
text(18, 100, "?", cex = 3, col = "dark grey")
dev.off()

practiceans <- data.frame(problem = c(1:4))
practiceans$mean <-
  c(
    trudist(
      alldat = practice1,
      slope = -1.1,
      line = 1
    )$mean,
    trudist(
      alldat = practice2,
      slope = -1.1,
      line = 1
    )$mean,
    trudist(
      alldat = practice3,
      slope = .5,
      line = 0
    )$mean,
    trudist(
      alldat = practice4,
      slope = -.5,
      line = 0
    )$mean
  )

practiceans$sd <-
  c(
    trudist(
      alldat = practice1,
      slope = -1.1,
      line = 1
    )$sd,
    trudist(
      alldat = practice2,
      slope = -1.1,
      line = 1
    )$sd,
    trudist(
      alldat = practice3,
      slope = .5,
      line = 0
    )$sd,
    trudist(
      alldat = practice4,
      slope = -.5,
      line = 0
    )$sd
  )
practiceans
write.csv(practiceans, "practiceans.csv")

####### CREATING DATA A PLOTS FOR THE PILOT
## For 1-6 the randomization is no line/line and near/far
# Number of sims
N <- 6
# Real SDs across sims
sds <- seq(1, 8, length.out = 6)

# Pre data points
prepoints <- 10
# post for close problems
postc <- 3
# post for long problems
postf <- 13

set.seed(2342423)
bins <- seq(-2, 2, length.out = 7)
slopestrat <- rep(NA, 6)
# shuffling
for (i in 1:6)
  slopestrat[i] <- runif(1, bins[i], bins[i + 1])

# Simulating data
# Randomizing the order of the slopes
slopes <- slopestrat[sample(1:6, 6)]
# Empty list for data
dats <- list()
# Also making a separate "close" data that stops earlier
# For easier input to the trudist function
datsclose <- list()
for (i in 1:6) {
  dats[[i]] <- makedata(
    slope = slopes[i],
    pre = prepoints,
    post = postf,
    noisevec <-
      rnorm(prepoints + postf, 0, sds[i])
  )
  datsclose[[i]] <- dats[[i]][1:(prepoints + postc),]
}

# Making plots
#setwd("~/Dropbox/Apps/Overleaf/Thinking and Confidence/_img/stratv2")
for (i in 1:N) {
  jpeg(paste(i, "c", "l", ".jpeg", sep = ""),
       height = 300,
       width = 300)
  makeplot(
    alldata = dats[[i]],
    slope = slopes[i],
    tpred = prepoints + postc,
    xlim = c(0, prepoints + postc - .2)
  )
  dev.off()
  
  jpeg(paste(i, "f", "l", ".jpeg", sep = ""),
       height = 300,
       width = 480)
  makeplot(
    alldata = dats[[i]],
    slope = slopes[i],
    tpred = prepoints + postf,
    xlim = c(0, prepoints + postf - .3)
  )
  dev.off()
  
  jpeg(paste(i, "c", "n", ".jpeg", sep = ""),
       height = 300,
       width = 300)
  makeplot(
    alldata = dats[[i]],
    slope = slopes[i],
    treat = 0,
    tpred = prepoints + postc,
    xlim = c(0, prepoints + postc - .2)
  )
  dev.off()
  
  jpeg(paste(i, "f", "n", ".jpeg", sep = ""),
       height = 300,
       width = 480)
  makeplot(
    alldata = dats[[i]],
    slope = slopes[i],
    treat = 0,
    tpred = prepoints + postf,
    xlim = c(0, prepoints + postf - .3)
  )
  dev.off()
}


# Right answers for each problem, first just mean and sd
# making a list which will hold data for each problem
rightlist <- list()
for (i in 1:6) {
  # setting up the df
  rightlist[[i]] <- data.frame(
    problem = rep(i, 4),
    distance = c("c", "f", "c", "f"),
    line = c("l", "l", "n", "n"),
    mean = NA,
    sd = NA,
    mbestsd = NA,
    mavgsd = NA
  )
  # population mean and sd
  # for line/close
  rightlist[[i]]$mean[1] <-
    trudist(alldat = datsclose[[i]],
            slope = slopes[i],
            line = 1)$mean
  rightlist[[i]]$sd[1] <-
    trudist(alldat = datsclose[[i]],
            slope = slopes[i],
            line = 1)$sd
  # for line/far
  rightlist[[i]]$mean[2] <- trudist(alldat = dats[[i]],
                                    slope = slopes[i],
                                    line = 1)$mean
  rightlist[[i]]$sd[2] <- trudist(alldat = dats[[i]],
                                  slope = slopes[i],
                                  line = 1)$sd
  # for no/close
  rightlist[[i]]$mean[3] <-
    trudist(alldat = datsclose[[i]],
            slope = slopes[i],
            line = 0)$mean
  rightlist[[i]]$sd[3] <-
    trudist(alldat = datsclose[[i]],
            slope = slopes[i],
            line = 0)$sd
  rightlist[[i]]$mbestsd[3] <-
    trudist(alldat = datsclose[[i]],
            slope = slopes[i],
            line = 0)$bestsd
  rightlist[[i]]$mavgsd[3] <-
    trudist(alldat = datsclose[[i]],
            slope = slopes[i],
            line = 0)$avgsd
  # for no/far
  rightlist[[i]]$mean[4] <- trudist(alldat = dats[[i]],
                                    slope = slopes[i],
                                    line = 0)$mean
  rightlist[[i]]$sd[4] <- trudist(alldat = dats[[i]],
                                  slope = slopes[i],
                                  line = 0)$sd
  rightlist[[i]]$mbestsd[4] <-
    trudist(alldat = datsclose[[i]],
            slope = slopes[i],
            line = 0)$bestsd
  rightlist[[i]]$mavgsd[4] <-
    trudist(alldat = datsclose[[i]],
            slope = slopes[i],
            line = 0)$avgsd
  
}

# Combinine to one data frame
ans <- rightlist[[1]]
for (i in 2:N)
  ans <- rbind(ans, rightlist[[i]])

write.csv(ans, "ans.csv")

# writing data for posterity
for (i in 1:6)
  write.csv(dats[[i]], paste("data", i, ".csv", sep = ""))

## For 7-12 the randomization is no line/line and near/far
# AND we scale the variance so the true answer is the same across these
# Number of sims
# Real SDs across sims
sds <- seq(8, 22, length.out = 6)

set.seed(242143)
bins <- seq(-2, 2, length.out = 7)
slopestrat <- rep(NA, 6)
# shuffling
for (i in 1:6)
  slopestrat[i] <- runif(1, bins[i], bins[i + 1])

# Simulating data
# Randomizing the order of the slopes
slopes <- slopestrat[sample(1:6, 6)]
# Empty lists for data
dats_lf <- list()
dats_lc <- list()
dats_nf <- list()
dats_nc <- list()
for (i in 1:6) {
  # Baseline noise vector for the line problems
  basenoise <- rnorm(prepoints + postf, 0, sds[i])
  # Creating line data, first far
  dats_lf[[i]] <-
    makedata(
      slope = slopes[i],
      pre = prepoints,
      post = postf,
      noisevec <- basenoise
    )
  # now chopping off the final points for the close version
  dats_lc[[i]] <- dats_lf[[i]][1:(prepoints + postc),]
  # Making the "guess" normalization factors
  linesd <- trudist(alldat = dats_lf[[i]],
                    slope = slopes[i],
                    line = 1)$sd
  farsd <- trudist(alldat = dats_lf[[i]],
                   slope = slopes[i],
                   line = 0)$sd
  closesd <- trudist(alldat = dats_lc[[i]],
                     slope = slopes[i],
                     line = 0)$sd
  # Some guesses for the right factor
  n_tests <- 20
  testfactors_f <-
    seq(.8 * linesd / farsd, 1.2 * linesd / farsd, length.out = n_tests)
  testfactors_c <-
    seq(.8 * linesd / closesd, 1.2 * linesd / closesd, length.out = n_tests)
  testsds_f <- rep(NA, n_tests)
  testsds_c <- rep(NA, n_tests)
  for (test in 1:n_tests) {
    testsds_f[test] <-
      trudist(
        alldat = makedata(
          slope = slopes[i],
          pre = prepoints,
          post = postf,
          noisevec <-
            basenoise * testfactors_f[test]
        ),
        slope = slopes[i],
        line = 0
      )$sd
    testsds_c[test] <-
      trudist(
        alldat = makedata(
          slope = slopes[i],
          pre = prepoints,
          post = postc,
          noisevec <-
            basenoise[1:(prepoints + postc)] * testfactors_c[test]
        ),
        slope = slopes[i],
        line = 0
      )$sd
  }
  adjfactor_f <- testfactors_f[which.min(abs(testsds_f - linesd))]
  adjfactor_c <- testfactors_c[which.min(abs(testsds_c - linesd))]
  # Close data with normalized noise vector
  dats_nf[[i]] <-
    makedata(
      slope = slopes[i],
      pre = prepoints,
      post = postf,
      noisevec <- basenoise * adjfactor_f
    )
  # For close version, need to truncate the noise vector
  dats_nc[[i]] <-
    makedata(
      slope = slopes[i],
      pre = prepoints,
      post = postc,
      noisevec <-
        basenoise[1:(prepoints + postc)] * adjfactor_c
    )
}

# Making plots
# setwd("~/Dropbox/Apps/Overleaf/Thinking and Confidence/_img/stratv2")
for (i in 1:N) {
  jpeg(paste(6 + i, "c", "l", ".jpeg", sep = ""),
       height = 300,
       width = 300)
  makeplot(
    alldata = dats_lc[[i]],
    slope = slopes[i],
    tpred = prepoints + postc,
    xlim = c(0, prepoints + postc - .2)
  )
  dev.off()
  
  jpeg(paste(6 + i, "f", "l", ".jpeg", sep = ""),
       height = 300,
       width = 480)
  makeplot(
    alldata = dats_lf[[i]],
    slope = slopes[i],
    tpred = prepoints + postf,
    xlim = c(0, prepoints + postf - .3)
  )
  dev.off()
  
  jpeg(paste(6 + i, "c", "n", ".jpeg", sep = ""),
       height = 300,
       width = 300)
  makeplot(
    alldata = dats_nc[[i]],
    slope = slopes[i],
    treat = 0,
    tpred = prepoints + postc,
    xlim = c(0, prepoints + postc - .2)
  )
  dev.off()
  
  jpeg(paste(6 + i, "f", "n", ".jpeg", sep = ""),
       height = 300,
       width = 480)
  makeplot(
    alldata = dats_nf[[i]],
    slope = slopes[i],
    treat = 0,
    tpred = prepoints + postf,
    xlim = c(0, prepoints + postf - .3)
  )
  dev.off()
}


# Right answers for each problem, first just mean and sd
# making a list which will hold data for each problem
rightlist <- list()
for (i in 1:6) {
  # setting up the df
  rightlist[[i]] <- data.frame(
    problem = rep(i + 6, 4),
    distance = c("c", "f", "c", "f"),
    line = c("l", "l", "n", "n"),
    mean = NA,
    sd = NA,
    mbestsd = NA,
    mavgsd = NA
  )
  # population mean and sd
  # for line/close
  rightlist[[i]]$mean[1] <-
    trudist(alldat = dats_lc[[i]],
            slope = slopes[i],
            line = 1)$mean
  rightlist[[i]]$sd[1] <-
    trudist(alldat = dats_lc[[i]],
            slope = slopes[i],
            line = 1)$sd
  # for line/far
  rightlist[[i]]$mean[2] <-
    trudist(alldat = dats_lf[[i]],
            slope = slopes[i],
            line = 1)$mean
  rightlist[[i]]$sd[2] <-
    trudist(alldat = dats_lf[[i]],
            slope = slopes[i],
            line = 1)$sd
  # for no/close
  rightlist[[i]]$mean[3] <-
    trudist(alldat = dats_nc[[i]],
            slope = slopes[i],
            line = 0)$mean
  rightlist[[i]]$sd[3] <-
    trudist(alldat = dats_nc[[i]],
            slope = slopes[i],
            line = 0)$sd
  rightlist[[i]]$mbestsd[3] <-
    trudist(alldat = dats_nc[[i]],
            slope = slopes[i],
            line = 0)$bestsd
  rightlist[[i]]$mavgsd[3] <-
    trudist(alldat = dats_nc[[i]],
            slope = slopes[i],
            line = 0)$avgsd
  # for no/far
  rightlist[[i]]$mean[4] <-
    trudist(alldat = dats_nf[[i]],
            slope = slopes[i],
            line = 0)$mean
  rightlist[[i]]$sd[4] <-
    trudist(alldat = dats_nf[[i]],
            slope = slopes[i],
            line = 0)$sd
  rightlist[[i]]$mbestsd[4] <-
    trudist(alldat = dats_nf[[i]],
            slope = slopes[i],
            line = 0)$bestsd
  rightlist[[i]]$mavgsd[4] <-
    trudist(alldat = dats_nf[[i]],
            slope = slopes[i],
            line = 0)$avgsd
  
}

# Combinine to one data frame
ans2 <- rightlist[[1]]
for (i in 2:N)
  ans2 <- rbind(ans2, rightlist[[i]])
ans2

allans <- rbind(ans, ans2)
allans
allans$scaled <- ifelse(allans$problem > 6, 1, 0)

allans
write.csv(allans, "ans.csv")

# writing data for posterity
for (i in 1:6)
  write.csv(dats[[i]], paste("data", i, ".csv", sep = ""))

# Checking coverage rates with line. Ideally the first should be
# about .9 and the second about .95

mean(replicate(10000, simhit90line()))
mean(replicate(10000, simhit95line()))

# Same w/o the line
mean(replicate(10000, simhit90no()))
mean(replicate(10000, simhit95no()))


mean(replicate(1000, simhit90line(
  pre = 15, noisevec = rnorm(20, 0, 15)
)))
mean(replicate(1000, simhit95line(
  pre = 15, noisevec = rnorm(20, 0, 15)
)))
mean(replicate(1000, simhit90no(
  pre = 15, noisevec = rnorm(20, 0, 15)
)))
mean(replicate(1000, simhit95no(
  pre = 15, noisevec = rnorm(20, 0, 15)
)))


mean(replicate(1000, trudist(
  alldat = makedata(slope = runif(1, -1, 1)), tpred = 15
)$hit95))

mean(replicate(1000, trudist(
  alldat = makedata(slope = runif(1, -1, 1)), tpred = 15
)$hit90))
mean(replicate(1000, trudist(
  alldat = makedata(slope = runif(1, -1, 1)), tpred = 15
)$hit95))
