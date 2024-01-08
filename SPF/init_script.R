# Initialization Script

# Common libraries
library(dplyr)
library(data.table)
library(haven)
library(readxl)
library(tidyr)
library(ggplot2)
library(purrr)
library(lmtest)
library(sandwich)
library(modelsummary)
library(broom) 
library(estimatr)
library(car)
library(multiwayvcov)
library(stargazer)
library(scales)
library(stats)
library(cowplot)
library(readxl)

# Commonly used variable names
# The survey’s six probability variables.
vars <-
  c("PRPGDP", "PRCCPI", "PRCPCE", "PRUNEMP", "PRGDP", "RECESS")
