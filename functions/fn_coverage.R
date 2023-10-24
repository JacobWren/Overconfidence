

# functions to draw a slope, check 90/95% confidence intervals
# with the line
simhit90line <- function(pre = 10,
                         post = 5,
                         noisevec = rnorm(15, 0, 5)) {
  slope <- runif(1, -1, 1)
  data <- makedata(
    slope = slope,
    pre = pre,
    post = post,
    noisevec = noisevec
  )
  return(trudist(
    slope = slope,
    alldat = data,
    line = 1
  )$hit90)
  
}

simhit95line <- function(pre = 10,
                         post = 5,
                         noisevec = rnorm(15, 0, 5)) {
  slope <- runif(1, -1, 1)
  data <- makedata(slope = slope)
  return(trudist(
    slope = slope,
    alldat = data,
    line = 1
  )$hit95)
  
}

#... and without the line
simhit90no <- function(pre = 10,
                       post = 5,
                       noisevec = rnorm(15, 0, 5)) {
  slope <- runif(1, -1, 1)
  data <- makedata(slope = slope)
  return(trudist(
    slope = slope,
    alldat = data,
    line = 0
  )$hit90)
  
}

simhit95no <- function(pre = 10,
                       post = 5,
                       noisevec = rnorm(15, 0, 5)) {
  slope <- runif(1, -1, 1)
  data <- makedata(slope = slope)
  return(trudist(
    slope = slope,
    alldat = data,
    line = 0
  )$hit95)
  
}