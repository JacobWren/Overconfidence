# Helper grid "settings".

fn_bin_limits_time_bounds <- function(elems) {
  # Define the binning limits + year bounds.
  end_point <- 1000
  limits <- list(
    c(end_point, 10:-3,-end_point),
    c(end_point, 12:-1,-end_point),
    c(end_point, 16:3,-end_point),
    c(end_point, seq(12, 4, by = -2),-end_point),
    c(end_point, seq(10, 2, by = -2),-end_point),
    c(end_point, 8:0,-end_point),
    c(end_point, seq(4, 0, by = -0.5),-end_point),
    c(end_point, seq(6,-2, by = -2),-end_point),
    c(end_point, 6:-2,-end_point),
    c(end_point, 6:-3,-end_point),
    c(end_point, 16, 10, 7, 4, 2.5, 1.5, 0, -3, -6, -12,-end_point),
    c(end_point, 11, seq(10, 7, by = -0.5), 6,-end_point),
    c(end_point, 9, seq(8, 5, by = -0.5), 4,-end_point),
    c(end_point, 15, 12, 10, 8:3,-end_point)
  )
  ops <-
    list(`<`,
         `==`,
         c(`>=`, `<`),
         c(`>=`, `<`),
         c(`>=`, `<`),
         c(`>=`, `<`),
         `>=`)
  
  return(list(limits[elems], ops))
}


fn_idx_year_cutoffs <- function(var) {
  # Define the year thresholds for the bins by variable.
  if (var == "PRPGDP") {
    years <-
      list(
        1973,
        1973,
        list(1974, 1981),
        list(1981, 1985),
        list(1985, 1992),
        list(1992, 2014),
        2014
      )
    return(list(1:7, years))
  }
  else if (var == "PRGDP") {
    years <-
      list(
        1973,
        1973,
        list(1974, 1981),
        list(1981, 1992),
        list(1992, 2009),
        list(2009, 2020),
        2020
      )
    return(list(c(1:3, 8:11), years))
  }
  else if (var == "PRCCPI" | var == "PRCPCE") {
    years <- NA
    return(list(7, years))
  }
  else if (var == "PRUNEMP") {
    years <- list(2014, list(2014, 2020), 2020)
    return(list(c(12:14), years))
  }
}