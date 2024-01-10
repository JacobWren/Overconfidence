# Apply binning logic -- match a realization to a bin.
fn_bin_outcome <-
  function(df,
           var_name,
           year_thresholds,
           bin_limits,
           operators) {
    for (j in seq_along(year_thresholds)) {
      year_threshold <- year_thresholds[[j]]
      ops <- operators[[j]]
      bin_limit <- bin_limits[[j]]
      
      for (i in 1:(length(bin_limit) - 1)) {
        lower_limit <- bin_limit[i + 1]
        upper_limit <- bin_limit[i]
        
        # Nest years and limits.
        if (is.list(year_threshold)) {
          df$binOutcome <- ifelse(
            df[[var_name]] < upper_limit &
              df[[var_name]] >= lower_limit &
              ops[[1]](df$year, year_threshold[1]) &
              ops[[2]](df$year, year_threshold[2]),
            i,
            df$binOutcome
          )
        } else if (typeof(year_threshold) == "double") {
          df$binOutcome <- ifelse(
            df[[var_name]] < upper_limit &
              df[[var_name]] >= lower_limit &
              ops(df$year, year_threshold),
            i,
            df$binOutcome
          )
        }
        else if (is.na(year_threshold)) {
          # No applicable year
          df$binOutcome <- ifelse(df[[var_name]] < upper_limit &
                                    df[[var_name]] >= lower_limit,
                                  i, df$binOutcome)
        }
      }
    }
    return(df)
  }