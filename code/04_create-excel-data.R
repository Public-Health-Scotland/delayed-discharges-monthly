#########################################################################
# Name of file - 04_create-excel-data.R
# Data release - Monthly Delayed Discharges publication
# Original Authors - Alice Byers
# Orginal Date - November 2020
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Write out data sheets required for excel tables
#########################################################################


### 0 - Load setup environment ----

source(here::here("code", "00_setup-environment.R"))


### 1 - Read in trend file ----

trend <-
  read_rds(
    here("trend", 
         paste0(format(start_month, "%Y-%m"), "_trend.rds"))
  )


### END OF SCRIPT ###