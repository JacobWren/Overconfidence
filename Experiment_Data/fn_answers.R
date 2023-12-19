# Right answers for each problem (save in "ans.csv" file).
source("~/overprecision/fn_true_dist.R")

fn_solutions <- function(close_data_l_,
                         close_data_nl_,
                         all_data_l_,
                         all_data_nl_,
                         slopes_,
                         n_sims_,
                         idx_,
                         num_practice_) {
  solutions <- list()  # List to hold the data for each problem.
  for (i in 1:n_sims_) {
    # Setting up the df.
    solutions[[i]] <- data.frame(
      problem = rep(i + idx_, num_practice_),
      distance = c("c", "f", "c", "f"),
      # close/far
      line = c("l", "l", "n", "n"),
      # line/no line
      mean = NA,
      sd = NA,
      mbestsd = NA,
      mavgsd = NA
    )
    # Population mean and sd.
    # for line/close
    dist_feats_lc <-
      fn_trudist(alldat = close_data_l_[[i]],
                 slope = slopes_[i],
                 line = 1)
    solutions[[i]]$mean[1] <- dist_feats_lc$mean
    solutions[[i]]$sd[1] <- dist_feats_lc$sd
    
    # for line/far
    dist_feats_lf <-
      fn_trudist(alldat = all_data_l_[[i]],
                 slope = slopes_[i],
                 line = 1)
    solutions[[i]]$mean[2] <- dist_feats_lf$mean
    solutions[[i]]$sd[2] <- dist_feats_lf$sd
    
    # for no/close
    dist_feats_nlc <-
      fn_trudist(alldat = close_data_nl_[[i]],
                 slope = slopes_[i],
                 line = 0)
    solutions[[i]]$mean[3] <- dist_feats_nlc$mean
    solutions[[i]]$sd[3] <- dist_feats_nlc$sd
    
    solutions[[i]]$mbestsd[3] <- dist_feats_nlc$bestsd
    solutions[[i]]$mavgsd[3] <- dist_feats_nlc$avgsd
    
    # for no/far
    dist_feats_nlf <-
      fn_trudist(alldat = all_data_nl_[[i]],
                 slope = slopes_[i],
                 line = 0)
    solutions[[i]]$mean[4] <- dist_feats_nlf$mean
    solutions[[i]]$sd[4] <- dist_feats_nlf$sd
    
    solutions[[i]]$mbestsd[4] <- dist_feats_nlf$bestsd
    solutions[[i]]$mavgsd[4] <- dist_feats_nlf$avgsd
  }
  return(solutions)
}