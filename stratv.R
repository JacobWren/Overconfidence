# Version for early October "big pilot"

# Function to make "all" of data
# period column marks what is observed vs not
# Make sure the first argument of noisevec matches pre + post
makedata <- function(slope,pre=10, noisevec=rnorm(15,0,5),  post=5){
  dates <- 1:(pre+post)
  sales <- 100 + slope*dates + noisevec
  period <- c(rep("Pre", pre), rep("Post", post))
  return(data.frame(time=dates, sales=sales, period=period))
}

test <- makedata(slope=.2)
test 



# Function to compute the correct predicted mean/sd for the last period
# using pre data
# With a line it just estimates the variance of the residuals 
# And uses the real trend for the point estimate
# Without a line it draws a best fit line and then bootstraps 
# a standard error
# also returns whether the truth is in a 90/95% confidence interval
# to check for correct coverage
# Need to think more carefully about the correct way to compute 
# the line variance since the coverage isn't 100%  correct here
trudist <- function(alldat, slope=.2, line=1, draws=10000){
  # time period to predict
  tpred <- nrow(alldat)
  # data we can use to predict
  dat <- subset(alldat, period=="Pre")
  sample_size <- nrow(dat)
  df = sample_size - 1
  # Only applies for normal distribution
  bias_correction = (sqrt(2/df)*gamma(sample_size/2)/gamma(df/2))
  # the "truth" of this simulation
  # Just used to correct coverage
  realsales <- alldat$sales[tpred]  # True point estimate (with noise)
  # Computing Bayesian mean/sd for those who get the line
  if (line==1){
    # the line
    realtrend <- 100 + slope*(1:nrow(dat))
    # old method of estimating sd as square root of mean residual
    # residual <- (dat$sales - realtrend)^2
    # sdest <- sqrt(mean(residual))
    sdest <- sd(dat$sales - realtrend)/bias_correction
    # point estimate which is trivially correct
    pointest <- 100 + slope*tpred # True point estimate (w/ out noise)
    # checking whether the real prediction point
    # is in a 90/95% confidence interval 
    cv = qt(0.10/2, df, lower.tail = FALSE)
    # lb90 = pointest - 1.64*sdest
    # ub90 = pointest + 1.64*sdest
    lb90 = pointest - cv*sdest
    ub90 = pointest + cv*sdest
    hit90 <- (realsales >= lb90)*(realsales <= ub90)
    
    cv = qt(0.05/2, df, lower.tail = FALSE)
    # lb95 = pointest - 1.96*sdest
    # ub95 = pointest + 1.96*sdest
    lb95 = pointest - cv*sdest
    ub95 = pointest + cv*sdest
    hit95 <- (realsales >= lb95)*(realsales <= ub95)
    return(list(mean=pointest, sd=sdest, hit90=hit90, hit95=hit95))
  }
  # Case where the trend line is not given
  if (line==0){
    # Drawing best fit line through the given data
    fixed_intercept <- 100
    # Subtract the explicit intercept from the regressand and then fit the 
    # intercept-free model:
    coeftable <- summary(lm(I(sales - fixed_intercept) ~ 0 + time, data=dat))$coef
    coeftable2 <- summary(lm(sales~time, data=dat))$coef
    pointest <- 100 + coeftable[1,1]*tpred
    # To get real variance, drawing a bunch of slopes with a 
    # mean equal to the point estimate 
    # and a sd equal to the standard error
    betaests <- rnorm(draws, coeftable[1,1], coeftable[1,2])
    # creating one prediction for each draw
    # equal to the mean prediction given this beta
    # plus an error term with a variance estimated from the
    # residuals given this beta
    preds <- rep(NA, draws)
    msres <- rep(NA, draws)
    for (i in 1:draws){
      drawest <- 100 + betaests[i]*tpred
      res <- (dat$sales - (100 + betaests[i]*(1:nrow(dat))))
      msres[i] <- sd(res)
      preds[i] <- drawest + rnorm(1, 0, msres[i])
    }
    sdest <- sd(preds)/bias_correction
    avgsd <- mean(msres)/bias_correction
    bestsd <- sd(dat$sales - (100 +coeftable[1,1]*(1:nrow(dat))))/bias_correction
    # checking confidence intervals
    cv = qt(0.10/2, df, lower.tail = FALSE)
    hit90 <- (realsales >= pointest - cv*sdest)*(realsales <= pointest + cv*sdest)
    cv = qt(0.05/2, df, lower.tail = FALSE)
    hit95 <- (realsales >= pointest - cv*sdest)*(realsales <= pointest + cv*sdest)
    return(list(mean=pointest, sd=sdest, avgsd=avgsd, bestsd =bestsd,
                hit90=hit90, hit95=hit95))
  }
  
}

# Test data set with slope .5
test <- makedata(slope=.5)
plot(test$time, test$sales)
trudist(alldat=test, slope=.5, line=1)
trudist(alldat=test, slope=.5, line=0)


# Function to create the plot
makeplot <- function(alldata, slope, tpred, treat=1, 
                     showbest=0, xlim=c(0, 28)){
  predates <- alldata$time[alldata$period=="Pre"]
  presales <- alldata$sales[alldata$period=="Pre"]
  par(mfrow=c(1,1), mar=c(2.8,2.8,.5,.5), mgp=c(1.6, .5, 0))
  plot(predates, presales, pch=19,
       ylim=c(60, 140), xlab="Day",
       ylab="Sales", xlim=xlim, axes=FALSE)
  axis(1)
  axis(2, at=seq(60, 140, by=10), labels=seq(60, 140, by=10),
       cex.axis=.8, las=2)
  box()
  abline(v=tpred, col="red", lwd=3)
  if (treat) abline(a=100, b=slope)
  if (showbest){
    coefs <- summary(lm(presales~predates))$coefficients
    # fixed_intercept <- 100
    # coefs <- summary(lm(I(presales - fixed_intercept) ~ 0 + predates))$coefficients
    abline(a=coefs[1], b=coefs[2], lty=3)
  }
  abline(h=seq(60,140, by=10), col=rgb(0,0,0, alpha=.5))
  # text(tpred + 1, seq(135, 65, length.out=8),
  #      c("A","B","C","D","E","F","G","H"))
  # arrows(tpred + 1, 138.5, tpred+1, 143, length=.02)
  # arrows(tpred + 1, 61, tpred+1, 57.5, length=.02)
}

makeplot(alldata=test, slope=.5, tpred=20, xlim=c(0, 19.5))


# Training examples v2
# setwd("~/Dropbox/Apps/Overleaf/Thinking and Confidence/_img/trainingv2")
set.seed(2445633)
lowvarv2 <- makedata(slope=1.6, noisevec=rnorm(20,0 ,1.5), post=10)
morevarv2 <- makedata(slope=-.3, noisevec=rnorm(20,0 ,15), post=10)
nolinev2 <- makedata(slope=.4, noisevec=rnorm(20, 0, 4), post=10)

data.frame(trainnum=c(1,2,3), 
      mean=c(trudist(alldat=lowvarv2, slope=1.6, line=1)$mean,
        trudist(alldat=morevarv2, slope=-.3, line=1)$mean,
        trudist(alldat=nolinev2, slope=.4, line=0)$mean),
      sd=c(trudist(alldat=lowvarv2, slope=1.6, line=1)$sd,
        trudist(alldat=morevarv2, slope=-.3, line=1)$sd,
        trudist(alldat=nolinev2, slope=.4, line=0)$sd)
      )
trudist(alldat=lowvarv2, slope=1.6, line=1)


set.seed(24532)
toscale <- rnorm(20, 0, 1)
practice1 <- makedata(slope=-1.1, noisevec=3*toscale, post=10)
practice2 <- makedata(slope=-1.1, noisevec=22*toscale, post=10)
practice3 <- makedata(slope=.5, noisevec=rnorm(20, 0, 4), post=10)
practice4 <- makedata(slope=-.5, noisevec=rnorm(20, 0, 15), post=10)

toscale_comp <- rnorm(20, 0, 1)
comp1 <- makedata(slope=.2, noisevec=2*toscale_comp, post=10)
comp2 <- makedata(slope=.2, noisevec=8*toscale_comp, post=10)


jpeg("train1.jpeg", height=300)
makeplot(alldata=lowvarv2, slope=1.6, tpred=20, xlim=c(0, 19.5))
dev.off()

jpeg("train2.jpeg", height=300)
makeplot(alldata=morevarv2, slope=-.3, tpred=20, xlim=c(0, 19.5))
dev.off()

jpeg("train3.jpeg", height=300)
makeplot(alldata=nolinev2, treat=0, tpred=20, xlim=c(0, 19.5))
dev.off()

jpeg("practice1.jpeg", height=300)
makeplot(alldata=practice1, slope=-1.1, tpred=20, xlim=c(0, 19.5))
dev.off()

jpeg("practice2.jpeg", height=300)
makeplot(alldata=practice2, slope=-1.1, tpred=20, xlim=c(0, 19.5))
dev.off()

jpeg("practice3.jpeg", height=300)
makeplot(alldata=practice3, treat=0, tpred=20, xlim=c(0, 19.5))
dev.off()

jpeg("practice4.jpeg", height=300)
makeplot(alldata=practice4, treat=0, tpred=20, xlim=c(0, 19.5))
dev.off()


jpeg("comp1.jpeg", height=300)
makeplot(alldata=comp1, treat=0, tpred=20, xlim=c(0, 19.5))
text(5, 135, "A", cex=3)
dev.off()

jpeg("comp2.jpeg", height=300)
makeplot(alldata=comp2, treat=0, tpred=20, xlim=c(0, 19.5))
text(5, 135, "B", cex=3)
dev.off()


jpeg("range.jpeg", height=300)
par(mfrow=c(1,1), mar=c(2.8,2.8,.5,.5), mgp=c(1.6, .5, 0))
plot(0:20, 60:80, pch=19,
     ylim=c(60, 140), xlab="Day",
     ylab="Sales", xlim=c(0, 19.5), col="white", axes=FALSE)
axis(1)
axis(2, at=seq(60, 140, by=10), labels=seq(60, 140, by=10),
     cex.axis=.8, las=2)
box()
abline(v=20, col="red", lwd=3)
abline(h=seq(60,140, by=10), col=rgb(0,0,0, alpha=.3))
lines(c(0, 20),c(100, 100+20*2), lwd=3)
lines(c(0, 20),c(100, 100+20*(-2)), lwd=3)
for (rs in runif(6, -2, 2)){
  lines(c(0, 20),c(100, 100+20*(rs)), lwd=1)
}
text(18, 100, "?", cex=3, col="dark grey")
dev.off()

practiceans <- data.frame(problem=c(1:4))
practiceans$mean <- c(trudist(alldat=practice1, slope=-1.1, line=1)$mean,
                      trudist(alldat=practice2, slope=-1.1, line=1)$mean,
                      trudist(alldat=practice3, slope=.5, line=0)$mean,
                      trudist(alldat=practice4, slope=-.5, line=0)$mean)

practiceans$sd <- c(trudist(alldat=practice1, slope=-1.1, line=1)$sd,
                      trudist(alldat=practice2, slope=-1.1, line=1)$sd,
                      trudist(alldat=practice3, slope=.5, line=0)$sd,
                      trudist(alldat=practice4, slope=-.5, line=0)$sd)
practiceans
write.csv(practiceans, "practiceans.csv")

####### CREATING DATA A PLOTS FOR THE PILOT
## For 1-6 the randomization is no line/line and near/far
# Number of sims
N <- 6
# Real SDs across sims
sds <- seq(1,8, length.out=6)

# Pre data points
prepoints <- 10
# post for close problems
postc <- 3
# post for long problems
postf <- 13


set.seed(2342423)
bins <- seq(-2, 2, length.out=7)
slopestrat <- rep(NA, 6)
# shuffling
for (i in 1:6) slopestrat[i] <- runif(1,bins[i], bins[i+1])

# Simulating data
# Randomizing the order of the slopes
slopes <- slopestrat[sample(1:6,6)]
# Empty list for data
dats <- list()
# Also making a separate "close" data that stops earlier
# For easier input to the trudist function
datsclose <- list()
for (i in 1:6){
dats[[i]] <- makedata(slope=slopes[i], pre=prepoints, post=postf,
                      noisevec <- rnorm(prepoints + postf, 0, sds[i]))
datsclose[[i]] <- dats[[i]][1:(prepoints + postc),]
}

# Making plots
#setwd("~/Dropbox/Apps/Overleaf/Thinking and Confidence/_img/stratv2")
for (i in 1:N){
  jpeg(paste(i, "c", "l", ".jpeg", sep=""), height=300, width=300)
  makeplot(alldata=dats[[i]], slope=slopes[i], tpred=prepoints + postc,
           xlim=c(0, prepoints + postc -.2))
  dev.off()
  
  jpeg(paste(i, "f", "l", ".jpeg", sep=""), height=300, width=480)
  makeplot(alldata=dats[[i]], slope=slopes[i], tpred=prepoints + postf,
           xlim=c(0, prepoints + postf -.3))
  dev.off()
  
  jpeg(paste(i, "c", "n", ".jpeg", sep=""), height=300, width=300)
  makeplot(alldata=dats[[i]], slope=slopes[i], treat=0, 
           tpred=prepoints + postc,
           xlim=c(0, prepoints + postc -.2))
  dev.off()
  
  jpeg(paste(i, "f", "n", ".jpeg", sep=""), height=300, width=480)
  makeplot(alldata=dats[[i]], slope=slopes[i], treat=0, 
           tpred=prepoints + postf,
           xlim=c(0, prepoints + postf -.3))
  dev.off()
}


# Right answers for each problem, first just mean and sd
# making a list which will hold data for each problem
rightlist <- list()
for (i in 1:6){
# setting up the df
rightlist[[i]] <- data.frame(problem=rep(i,4), 
                             distance=c("c", "f", "c", "f"),
                             line=c("l", "l", "n", "n"),
                             mean=NA,
                             sd=NA,
                             mbestsd=NA,
                             mavgsd=NA)
# population mean and sd
# for line/close
rightlist[[i]]$mean[1] <- trudist(alldat=datsclose[[i]],slope=slopes[i],
                                  line=1)$mean
rightlist[[i]]$sd[1] <- trudist(alldat=datsclose[[i]],slope=slopes[i],
                                  line=1)$sd
# for line/far
rightlist[[i]]$mean[2] <- trudist(alldat=dats[[i]],slope=slopes[i],
                                  line=1)$mean
rightlist[[i]]$sd[2] <- trudist(alldat=dats[[i]],slope=slopes[i],
                                line=1)$sd
# for no/close
rightlist[[i]]$mean[3] <- trudist(alldat=datsclose[[i]],slope=slopes[i],
                                  line=0)$mean
rightlist[[i]]$sd[3] <- trudist(alldat=datsclose[[i]],slope=slopes[i],
                                line=0)$sd
rightlist[[i]]$mbestsd[3] <- trudist(alldat=datsclose[[i]],slope=slopes[i],
                                line=0)$bestsd
rightlist[[i]]$mavgsd[3] <- trudist(alldat=datsclose[[i]],slope=slopes[i],
                                line=0)$avgsd
# for no/far
rightlist[[i]]$mean[4] <- trudist(alldat=dats[[i]],slope=slopes[i],
                                  line=0)$mean
rightlist[[i]]$sd[4] <- trudist(alldat=dats[[i]],slope=slopes[i],
                                line=0)$sd
rightlist[[i]]$mbestsd[4] <- trudist(alldat=datsclose[[i]],slope=slopes[i],
                                     line=0)$bestsd
rightlist[[i]]$mavgsd[4] <- trudist(alldat=datsclose[[i]],slope=slopes[i],
                                    line=0)$avgsd

}

# Combinine to one data frame
ans <- rightlist[[1]]
for (i in 2:N) ans <- rbind(ans, rightlist[[i]])

ans

write.csv(ans, "ans.csv")

# writing data for posterity
for (i in 1:6) write.csv(dats[[i]], paste("data", i, ".csv", sep=""))

## For 7-12 the randomization is no line/line and near/far
# AND we scale the variance so the true answer is the same across these
# Number of sims
# Real SDs across sims
sds <- seq(8,22, length.out=6)


set.seed(242143)
bins <- seq(-2, 2, length.out=7)
slopestrat <- rep(NA, 6)
# shuffling
for (i in 1:6) slopestrat[i] <- runif(1,bins[i], bins[i+1])

# Simulating data
# Randomizing the order of the slopes
slopes <- slopestrat[sample(1:6,6)]
# Empty lists for data
dats_lf <- list()
dats_lc <- list()
dats_nf <- list()
dats_nc <- list()
for (i in 1:6){
  # Baseline noise vector for the line problems
  basenoise <- rnorm(prepoints + postf, 0, sds[i])
  # Creating line data, first far
  dats_lf[[i]] <- makedata(slope=slopes[i], pre=prepoints, post=postf,
                        noisevec <- basenoise)
  # now chopping off the final points for the close version
  dats_lc[[i]] <- dats_lf[[i]][1:(prepoints + postc),]
  # Making the "guess" normalization factors
  linesd <- trudist(alldat=dats_lf[[i]],slope=slopes[i],
                    line=1)$sd
  farsd <- trudist(alldat=dats_lf[[i]],slope=slopes[i],
                     line=0)$sd
  closesd <- trudist(alldat=dats_lc[[i]],slope=slopes[i],
                   line=0)$sd
  # Some guesses for the right factor
  n_tests <- 20
  testfactors_f <- seq(.8*linesd/farsd, 1.2*linesd/farsd, length.out=n_tests)
  testfactors_c <- seq(.8*linesd/closesd, 1.2*linesd/closesd, length.out=n_tests)
  testsds_f <- rep(NA, n_tests)
  testsds_c <- rep(NA, n_tests)
  for (test in 1:n_tests){
    testsds_f[test] <- trudist(alldat=makedata(slope=slopes[i], pre=prepoints, 
                                         post=postf,
                                         noisevec <- basenoise*testfactors_f[test]),
                         slope=slopes[i],
                         line=0)$sd
    testsds_c[test] <- trudist(alldat=makedata(slope=slopes[i], pre=prepoints, 
                                               post=postc,
                                               noisevec <- basenoise[1:(prepoints + postc)]*testfactors_c[test]),
                               slope=slopes[i],
                               line=0)$sd
  }
  adjfactor_f <- testfactors_f[which.min(abs(testsds_f-linesd))]
  adjfactor_c <- testfactors_c[which.min(abs(testsds_c-linesd))]
  # Close data with normalized noise vector
  dats_nf[[i]] <- makedata(slope=slopes[i], pre=prepoints, post=postf,
                           noisevec <- basenoise*adjfactor_f)
  # For close version, need to truncate the noise vector
  dats_nc[[i]] <- makedata(slope=slopes[i], pre=prepoints, post=postc,
                           noisevec <- basenoise[1:(prepoints + postc)]*adjfactor_c)
}

# Making plots
# setwd("~/Dropbox/Apps/Overleaf/Thinking and Confidence/_img/stratv2")
for (i in 1:N){
  jpeg(paste(6+i, "c", "l", ".jpeg", sep=""), height=300, width=300)
  makeplot(alldata=dats_lc[[i]], slope=slopes[i], tpred=prepoints + postc,
           xlim=c(0, prepoints + postc -.2))
  dev.off()
  
  jpeg(paste(6+i, "f", "l", ".jpeg", sep=""), height=300, width=480)
  makeplot(alldata=dats_lf[[i]], slope=slopes[i], tpred=prepoints + postf,
           xlim=c(0, prepoints + postf -.3))
  dev.off()
  
  jpeg(paste(6+i, "c", "n", ".jpeg", sep=""), height=300, width=300)
  makeplot(alldata=dats_nc[[i]], slope=slopes[i], treat=0, 
           tpred=prepoints + postc,
           xlim=c(0, prepoints + postc -.2))
  dev.off()
  
  jpeg(paste(6+i, "f", "n", ".jpeg", sep=""), height=300, width=480)
  makeplot(alldata=dats_nf[[i]], slope=slopes[i], treat=0, 
           tpred=prepoints + postf,
           xlim=c(0, prepoints + postf -.3))
  dev.off()
}


# Right answers for each problem, first just mean and sd
# making a list which will hold data for each problem
rightlist <- list()
for (i in 1:6){
  # setting up the df
  rightlist[[i]] <- data.frame(problem=rep(i+6,4), 
                               distance=c("c", "f", "c", "f"),
                               line=c("l", "l", "n", "n"),
                               mean=NA,
                               sd=NA,
                               mbestsd=NA,
                               mavgsd=NA)
  # population mean and sd
  # for line/close
  rightlist[[i]]$mean[1] <- trudist(alldat=dats_lc[[i]],slope=slopes[i],
                                    line=1)$mean
  rightlist[[i]]$sd[1] <- trudist(alldat=dats_lc[[i]],slope=slopes[i],
                                  line=1)$sd
  # for line/far
  rightlist[[i]]$mean[2] <- trudist(alldat=dats_lf[[i]],slope=slopes[i],
                                    line=1)$mean
  rightlist[[i]]$sd[2] <- trudist(alldat=dats_lf[[i]],slope=slopes[i],
                                  line=1)$sd
  # for no/close
  rightlist[[i]]$mean[3] <- trudist(alldat=dats_nc[[i]],slope=slopes[i],
                                    line=0)$mean
  rightlist[[i]]$sd[3] <- trudist(alldat=dats_nc[[i]],slope=slopes[i],
                                  line=0)$sd
  rightlist[[i]]$mbestsd[3] <- trudist(alldat=dats_nc[[i]],slope=slopes[i],
                                       line=0)$bestsd
  rightlist[[i]]$mavgsd[3] <- trudist(alldat=dats_nc[[i]],slope=slopes[i],
                                      line=0)$avgsd
  # for no/far
  rightlist[[i]]$mean[4] <- trudist(alldat=dats_nf[[i]],slope=slopes[i],
                                    line=0)$mean
  rightlist[[i]]$sd[4] <- trudist(alldat=dats_nf[[i]],slope=slopes[i],
                                  line=0)$sd
  rightlist[[i]]$mbestsd[4] <- trudist(alldat=dats_nf[[i]],slope=slopes[i],
                                       line=0)$bestsd
  rightlist[[i]]$mavgsd[4] <- trudist(alldat=dats_nf[[i]],slope=slopes[i],
                                      line=0)$avgsd
  
}

# Combinine to one data frame
ans2 <- rightlist[[1]]
for (i in 2:N) ans2 <- rbind(ans2, rightlist[[i]])
ans2

allans <- rbind(ans,ans2)
allans
allans$scaled <- ifelse(allans$problem > 6, 1, 0)

allans
write.csv(allans, "ans.csv")

# writing data for posterity
for (i in 1:6) write.csv(dats[[i]], paste("data", i, ".csv", sep=""))





######### CHECKING THE CONFIDENCE INTERVALS

# functions to draw a slope, check 90/95% confidence intervals
# with the line
simhit90line <- function(pre=10,post=5, noisevec=rnorm(15,0,5)){
  slope <- runif(1, -1, 1)
  data <- makedata(slope=slope, 
                   pre=pre, post=post,noisevec=noisevec)
  return(trudist(slope=slope, alldat=data, line=1)$hit90)
  
}

simhit95line <- function(pre=10,post=5, noisevec=rnorm(15,0,5)){
  slope <- runif(1, -1, 1)
  data <- makedata(slope=slope)
  return(trudist(slope=slope, alldat=data, line=1)$hit95)
  
}

#... and without the line
simhit90no <- function(pre=10,post=5, noisevec=rnorm(15,0,5)){
  slope <- runif(1, -1, 1)
  data <- makedata(slope=slope)
  return(trudist(slope=slope, alldat=data, line=0)$hit90)
  
}

simhit95no <- function(pre=10,post=5, noisevec=rnorm(15,0,5)){
  slope <- runif(1, -1, 1)
  data <- makedata(slope=slope)
  return(trudist(slope=slope, alldat=data, line=0)$hit95)
  
}


simhit90line()

# Checking coverage rates with line. Ideally the first should be 
# about .9 and the second about .95

mean(replicate(100000, simhit90line()))

mean(replicate(100000, simhit95line()))

# Same w/o the line
mean(replicate(100000, simhit90no()))
mean(replicate(100000, simhit95no()))



mean(replicate(1000, simhit90line(pre=15, noisevec=rnorm(20,0,15))))
mean(replicate(1000, simhit95line(pre=15, noisevec=rnorm(20,0,15))))
mean(replicate(1000, simhit90no(pre=15, noisevec=rnorm(20,0,15))))
mean(replicate(1000, simhit95no(pre=15, noisevec=rnorm(20,0,15))))


mean(replicate(1000, trudist(alldat=makedata(slope=runif(1, -1, 1)), tpred=15)$hit95))

mean(replicate(1000, trudist(alldat=makedata(slope=runif(1, -1, 1)), tpred=15)$hit90))
mean(replicate(1000, trudist(alldat=makedata(slope=runif(1, -1, 1)), tpred=15)$hit95))




