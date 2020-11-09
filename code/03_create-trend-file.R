#########################################################################
# Name of file - 03_create-trend-file.R
# Data release - Monthly Delayed Discharges publication
# Original Authors - Alice Byers
# Orginal Date - November 2020
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Add Scotland file for latest month to trend file
#########################################################################


### 0 - Load setup environment ----

source(here::here("code", "00_setup-environment.R"))


### 1 - Read in Scotland file and previous trend file ----

scotland <-
  read_rds(
    here("data", format(start_month, "%Y-%m"),
         paste0(format(start_month, "%Y-%m"), "_scotland.rds"))
  )

trend <-
  read_rds(
    here("trend", 
         paste0(format(start_month - months(1), "%Y-%m"), "_trend.rds"))
  )


### 2 - Add latest month Scotland file to previous month trend ----

trend %<>%
  bind_rows(scotland)


### END OF SCRIPT ###