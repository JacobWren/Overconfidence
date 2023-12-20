# Initialization Script

# Common libraries
library(dplyr)
library(data.table)
library(haven)
library(readxl)
library(tidyr)

# Get and set working directory
current_dir <- getwd()
if (!grepl("SPF$", current_dir)) {
  setwd("SPF")
}

# Commonly used variable names
# The survey’s six probability variables.
vars <-
  c("PRPGDP", "RECESS", "PRCCPI", "PRCPCE", "PRUNEMP", "PRGDP")
