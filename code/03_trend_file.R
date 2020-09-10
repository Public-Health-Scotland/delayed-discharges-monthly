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


### 2 - Data wrangling

## Add specialty description
# Create lookup file
lookup_specialty <- read_csv("https://www.opendata.nhs.scot/dataset/688c7ea0-4845-4b03-9df0-4149c72cb7f0/resource/6f2e3da0-b1b5-46cc-ac04-78495daedfa3/download/specialty-reference.csv")

# Add specialty description to datafile
datafile %<>% left_join(lookup_specialty, by = c("specialty_code" = "Specialty"))


## Add hospital names
# Create lookup file
lookup_hospital <- read_csv("https://www.opendata.nhs.scot/dataset/cbd1802e-0e04-4282-88eb-d7bdcfb120f0/resource/c698f450-eeed-41a0-88f7-c1e40a568acc/download/current_nhs_hospitals_in_scotland_170820.csv")
  
# Add hospital names to datafile
datafile %<>% left_join(lookup_hospital, by = c("health_location_code" = "Location"))
