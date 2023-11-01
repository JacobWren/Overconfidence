# Function to make "all" (e.g., pre and post) of the data.
# Period column marks what is observed vs not observed.
# Make sure the first argument of noisevec matches pre + post
makedata <- function(slope,
                     pre = 10,
                     noisevec = rnorm(15, 0, 5),
                     post = 5) {
  dates <- 1:(pre + post)
  sales <- 100 + slope * dates + noisevec
  period <- c(rep("Pre", pre), rep("Post", post))
  return(data.frame(
    time = dates,
    sales = sales,
    period = period
  ))
}

test <- makedata(slope = .2)