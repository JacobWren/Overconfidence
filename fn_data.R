# Function to make "all" (e.g., pre_ and post_) of the data.
# Period column marks what is observed vs not observed.
# The first argument of noisevec_ should equal pre_ + post_.

fn_makedata <-
  function(slope_,
           pre_ = 10,
           noisevec_ = rnorm(15, 0, 5),
           post_ = 5)
  {
    dates <- 1:(pre_ + post_)
    sales <- 100 + slope_ * dates + noisevec_
    period <- c(rep("Pre", pre_), rep("Post", post_))
    return(data.frame(
      time = dates,
      sales = sales,
      period = period
    ))
  }


fn_sample_uniform <- 
  function(count, n_sims_, lb, ub)
  # Helper function to make uniform draws. 
  {
    bins <- seq(lb, ub, length.out = count)
    slopestrat <- rep(NA, n_sims_)
    # shuffling
    for (i in 1:n_sims_)
      slopestrat[i] <- runif(1, bins[i], bins[i + 1])
    return(slopestrat)
  }
