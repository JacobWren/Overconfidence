# Functions to draw a slope, check 90/95% confidence intervals with and without
# the line_.

fn_simhit <-
  function(pre_ = 10,
           post_ = 5,
           noisevec_ = rnorm(15, 0, 5),
           line_ = 1,
           var_ = "hit95") {
    slope <- runif(1, -2, 2)
    data <-
      fn_makedata(slope = slope,
                  post_ = post_,
                  noisevec_ = noisevec_)
    return(fn_trudist(
      slope = slope,
      alldat = data,
      line_ = 1
    )[[var_]])
  }
