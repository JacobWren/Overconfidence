library(dplyr)
library(data.table)
library(haven)
library(readxl)
library(tidyr)

# For comparing files.

vars <-
  c("PRPGDP", "RECESS", "PRCCPI", "PRCPCE", "PRUNEMP", "PRGDP")

for (var in vars) {
  path <-
    paste0("Data/SPFmicrodataCleanedAggOwnCalc_", var, ".csv")
  df1 <- read.csv(path)
  
  path <-
    paste0("Data/SPFmicrodataCleanedAggOwnCalc2_", var, ".csv")
  df2 <- read.csv(path)
}
  