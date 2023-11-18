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