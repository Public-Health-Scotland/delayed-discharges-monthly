#########################################################################
# Name of file - 03_trend_file.R
# Data release - Monthly Delayed Discharges publication
# Original Authors - Russell McCreath
# Orginal Date - September 2020
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - An update to the trend file with Delayed Discharges 
# records from most recent publication month.
#########################################################################


### 0 - Load setup_environment and functions ----

source(here::here("code", "00_setup_environment.R"))

walk(list.files(here("functions"), full.names = TRUE), source)


### 1 - Import latest Scotland file ----

datafile <- read_rds(here("data", format(start_month, "%Y-%m"), paste0(format(start_month, "%Y-%m"), "_scotland.rds")))
