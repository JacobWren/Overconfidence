# Version for October 2023 "big pilot"
# 1) Generates figures & data (including correct answers) for each problem
# 2) Verifies coverage probability

# Load functions
source("fn_data.R")
source("fn_true_dist.R")
source("fn_plot.R")
source("fn_coverage.R")

# Parameters for all figured
params <- c(
  'n' = 20,
  'n_post' = 10
)

# Training examples 

# Set training figures parameters
params_train <- c(
  'mean' = 0,
  'var1' = 1.5,
  'var2' = 15,
  'var3' = 4,
  'slope1' = 1.6,
  'slope2' = -0.30,
  'slope3' = 0.40
)

set.seed(2445633)  # Set training seed
lowvarv2 <- makedata(slope = params_train['slope1'],
                     noisevec = rnorm(params['n'], params_train['mean'] , params_train['var1']),
                     post = params['n_post'])
morevarv2 <- makedata(slope = params_train['slope2'],
                      noisevec = rnorm(params['n'], params_train['mean'] , params_train['var2']),
                      post = params['n_post'])
nolinev2 <- makedata(slope = params['slope3'],
                     noisevec = rnorm(params['n'], params_train['mean'], params_train['var3']),
                     post = params['n_post'])


# Set practice figures parameters
params_prac <- c(
  'mean' = 0,
  'var' = 1.5,
  'var3' = 4,
  'var4' = 15,
  'slope1' = -1.1,
  'slope2' = -1.1,
  'slope3' = 0.5,
  'slope4' = -0.5,
  'scale1' = 3,
  'scale2' = 22
)
set.seed(24532)
scale_prac <- rnorm(params['n'], params_prac['mean'], params_prac['var'])
practice1 <- makedata(slope = params_prac['slope1'],
                      noisevec = params_prac['scale1'] * scale_prac,
                      post = params['n_post'])
practice2 <- makedata(slope = params_prac['slope2'],
                      noisevec = params_prac['scale2'] * scale_prac,
                      post = params['n_post'])
practice3 <- makedata(slope = params_prac['slope3'],
                      noisevec = rnorm(params['n'], params_prac['mean'], params_prac['var3']),
                      post = params['n_post'])
practice4 <- makedata(slope = params_prac['slope4'],
                      noisevec = rnorm(params['n'], params_prac['mean'], params_prac['var4']),
                      post = params['n_post'])

# Set comp figures parameters
params_comp <- c(
  'mean' = 0,
  'var' = 1,
  'slope' = 0.20,
  'scale1' = 2,
  'scale2' = 8
)
scale_comp <- rnorm(params['n'], params_comp['mean'], params_comp['var'])
comp1 <- makedata(slope = params_comp['slope'],
                  noisevec = params_comp['scale1'] * scale_comp,
                  post = params['n_post'])
comp2 <- makedata(slope = params_comp['slope'],
                  noisevec = params_comp['scale2'] * scale_comp,
                  post = params['n_post'])

fig_height <- 300
x_lim = c(0, 19.5)

jpeg("train1.jpeg", height = fig_height)
makeplot(
  alldata = lowvarv2,
  slope = params_train['slope1'],
  tpred = params['n'],
  xlim = x_lim
)
dev.off()

jpeg("train2.jpeg", height = fig_height)
makeplot(
  alldata = morevarv2,
  slope = params_train['slope2'],
  tpred = params['n'],
  xlim = x_lim
)
dev.off()

jpeg("train3.jpeg", height = fig_height)
makeplot(
  alldata = nolinev2,
  treat = 0,
  tpred = params['n'],
  xlim = x_lim 
)
dev.off()

jpeg("practice1.jpeg", height = fig_height)
makeplot(
  alldata = practice1,
  slope = params_prac['slope1'],
  tpred = params['n'],
  xlim = x_lim
)
dev.off()

jpeg("practice2.jpeg", height = fig_height)
makeplot(
  alldata = practice2,
  slope = params_prac['slope2'],
  tpred = params['n'],
  xlim = x_lim
)
dev.off()

jpeg("practice3.jpeg", height = fig_height)
makeplot(
  alldata = practice3,
  treat = 0,
  tpred = params['n'],
  xlim = x_lim
)
dev.off()

jpeg("practice4.jpeg", height = fig_height)
makeplot(
  alldata = practice4,
  treat = 0,
  tpred = params['n'],
  xlim = x_lim
)
dev.off()

jpeg("comp1.jpeg", height = fig_height)
makeplot(
  alldata = comp1,
  treat = 0,
  tpred = params['n'],
  xlim = x_lim
)
text(5, 135, "A", cex = 3)
dev.off()

jpeg("comp2.jpeg", height = fig_height)
makeplot(
  alldata = comp2,
  treat = 0,
  tpred = params['n'],
  xlim = x_lim
)
text(5, 135, "B", cex = 3)
dev.off()

jpeg("range.jpeg", height = fig_height)
par(
  mfrow = c(1, 1),
  mar = c(2.8, 2.8, .5, .5),
  mgp = c(1.6, .5, 0)
)
plot(
  0:params['n'],
  60:80,
  pch = 19,
  ylim = c(60, 140),
  xlab = "Day",
  ylab = "Sales",
  xlim = x_lim,
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
abline(v = params['n'], col = "red", lwd = 3)
abline(h = seq(60, 140, by = 10), col = rgb(0, 0, 0, alpha = .3))
lines(c(0, params['n']), c(100, 100 + params['n'] * 2), lwd = 3)
lines(c(0, params['n']), c(100, 100 + params['n'] * (-2)), lwd = 3)
for (rs in runif(6, -2, 2)) {
  lines(c(0, params['n']), c(100, 100 + params['n'] * (rs)), lwd = 1)
}
text(18, 100, "?", cex = 3, col = "dark grey")
dev.off()

num_practice <- 4
practiceans <- data.frame(problem = c(1:num_practice))
p1_dist <- trudist(
  alldat = practice1,
  slope = params_prac['slope1'],
  line = 1
)
p2_dist <- trudist(
  alldat = practice2,
  slope = params_prac['slope2'],
  line = 1
)
p3_dist <- trudist(
  alldat = practice3,
  slope = params_prac['slope3'],
  line = 0
)
p4_dist <- trudist(
  alldat = practice4,
  slope = params_prac['slope4'],
  line = 0
)
practiceans$mean <-
  c(p1_dist$mean,
    p2_dist$mean,
    p3_dist$mean,
    p4_dist$mean
  )
practiceans$sd <-
  c(p1_dist$sd,
    p2_dist$sd,
    p3_dist$sd,
    p4_dist$sd
  )
write.csv(practiceans, "practiceans.csv")

# DATA PLOTS FOR THE PILOT
# For 1-6 the randomization is no line/line and near/far
n_sims <- 6 # Number of simulations
# Real SDs across simulations
sds <- seq(1, 8, length.out = n_sims)

# Pre data points
prepoints <- params['n_post']
# post for close problems
postc <- 3
# post for long problems
postf <- 13

set.seed(2342423)
bins <- seq(-2, 2, length.out = 7)
slopestrat <- rep(NA, n_sims)
# shuffling
for (i in 1:n_sims)
  slopestrat[i] <- runif(1, bins[i], bins[i + 1])

# Simulating data
# Randomizing the order of the slopes
slopes <- slopestrat[sample(1:n_sims, n_sims)]
# Empty list for data
dats <- list()
# Also making a separate "close" data that stops earlier;
# for easier input to the trudist function
datsclose <- list()
for (i in 1:n_sims) {
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
c_lim = 0.20
f_lim = 0.30
for (i in 1:n_sims) {
  jpeg(paste(i, "c", "l", ".jpeg", sep = ""),
       height = fig_height,
       width = 300)
  makeplot(
    alldata = dats[[i]],
    slope = slopes[i],
    tpred = prepoints + postc,
    xlim = c(0, prepoints + postc - c_lim)
  )
  dev.off()
  
  jpeg(paste(i, "f", "l", ".jpeg", sep = ""),
       height = fig_height,
       width = 480)
  makeplot(
    alldata = dats[[i]],
    slope = slopes[i],
    tpred = prepoints + postf,
    xlim = c(0, prepoints + postf - f_lim)
  )
  dev.off()
  
  jpeg(paste(i, "c", "n", ".jpeg", sep = ""),
       height = fig_height,
       width = 300)
  makeplot(
    alldata = dats[[i]],
    slope = slopes[i],
    treat = 0,
    tpred = prepoints + postc,
    xlim = c(0, prepoints + postc - c_lim)
  )
  dev.off()
  
  jpeg(paste(i, "f", "n", ".jpeg", sep = ""),
       height = fig_height,
       width = 480)
  makeplot(
    alldata = dats[[i]],
    slope = slopes[i],
    treat = 0,
    tpred = prepoints + postf,
    xlim = c(0, prepoints + postf - f_lim)
  )
  dev.off()
}


# Right answers for each problem.
rightlist <- fn_solutions(datsclose,
                          datsclose,
                          dats,
                          dats,
                          slopes,
                          n_sims,
                          0,
                          num_practice)
# Combine to one data frame
ans <- rightlist[[1]]
for (i in 2:n_sims)
  ans <- rbind(ans, rightlist[[i]])
# Write answers
write.csv(ans, "ans.csv")

# For 7-12 the randomization is no line/line and near/far
# AND we scale the variance so the true answer is the same across these simulations

# Real SDs across simulations.
sds <- seq(8, 22, length.out = n_sims)

set.seed(242143)
bins <- seq(-2, 2, length.out = 7)
slopestrat <- rep(NA, n_sims)
# shuffling
for (i in 1:n_sims)
  slopestrat[i] <- runif(1, bins[i], bins[i + 1])

# Simulating data
# Randomizing the order of the slopes
slopes <- slopestrat[sample(1:n_sims, n_sims)]

# Empty lists for data storage
dats_lf <- list()
dats_lc <- list()
dats_nf <- list()
dats_nc <- list()
for (i in 1:n_sims) {
  # Baseline noise vector for the line problems.
  basenoise <- rnorm(prepoints + postf, params_train['mean'], sds[i])
  # Creating line data, first for far.
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
  n_tests <- params['n']
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
for (i in 1:n_sims) {
  jpeg(paste(n_sims + i, "c", "l", ".jpeg", sep = ""),
       height = fig_height,
       width = 300)
  makeplot(
    alldata = dats_lc[[i]],
    slope = slopes[i],
    tpred = prepoints + postc,
    xlim = c(0, prepoints + postc - .2)
  )
  dev.off()
  
  jpeg(paste(n_sims + i, "f", "l", ".jpeg", sep = ""),
       height = fig_height,
       width = 480)
  makeplot(
    alldata = dats_lf[[i]],
    slope = slopes[i],
    tpred = prepoints + postf,
    xlim = c(0, prepoints + postf - .3)
  )
  dev.off()
  
  jpeg(paste(n_sims + i, "c", "n", ".jpeg", sep = ""),
       height = fig_height,
       width = 300)
  makeplot(
    alldata = dats_nc[[i]],
    slope = slopes[i],
    treat = 0,
    tpred = prepoints + postc,
    xlim = c(0, prepoints + postc - .2)
  )
  dev.off()
  
  jpeg(paste(n_sims + i, "f", "n", ".jpeg", sep = ""),
       height = fig_height,
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

rightlist2 <- fn_solutions(dats_lc,
                          dats_nc,
                          dats_lf,
                          dats_nf,
                          slopes,
                          n_sims,
                          n_sims,
                          num_practice)

# Combinine to one data frame
ans2 <- rightlist2[[1]]
for (i in 2:n_sims)
  ans2 <- rbind(ans2, rightlist2[[i]])

allans <- rbind(ans, ans2)
allans$scaled <- ifelse(allans$problem > n_sims, 1, 0)
write.csv(allans, "ans.csv")

# Check coverage probability.
mean(replicate(1000, simhit90line(
  pre = 15, noisevec = rnorm(params['n'], params['mean'], 15)
)))
mean(replicate(1000, simhit95line(
  pre = 15, noisevec = rnorm(params['n'], params['mean'], 15)
)))
mean(replicate(1000, simhit90no(
  pre = 15, noisevec = rnorm(params['n'], params['mean'], 15)
)))
mean(replicate(1000, simhit95no(
  pre = 15, noisevec = rnorm(params['n'], params['mean'], 15)
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

# JAKE: see res; re-look over this mod, reformat, fix others (should be quick)