#########################################################################
# Name of file - 00_setup_environment.R
# Data release - Monthly Delayed Discharges publication
# Original Authors - Alice Byers
# Orginal Date - July 2020
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Sets up environment required for running publication RAP. 
# This is the only file which should require updating every time the RAP 
# process is run
#########################################################################


### 1 - Load packages ----

library(dplyr)        # For data manipulation in the "tidy" way
library(tidyr)        # For data manipulation in the "tidy" way
library(lubridate)    # For dealing with dates
library(stringr)      # For string manipulation and matching
library(janitor)      # For 'cleaning' variable names
library(purrr)        # For functional programming
library(readr)        # For reading files
library(haven)        # For reading in SPSS files
library(here)         # For the here() function
library(phsmethods)   # For internal PHS functions
library(magrittr)     # For the %<>%
library(glue)         # For working with strings
library(almanac)      # For working with recurring dates
library(usethis)      # For creating new folders


### 2 - Define month start date and derive end date ----

start_month <- dmy(01062020)

end_month <- ceiling_date(start_month, "month") - days(1)


### 3 - Create data folders ----

boards <- c("a&a", "borders", "d&g", "fife", "fv", 
            "glasgow", "grampian", "highland", "lanark", 
            "lothian", "orkney", "shetland", "tayside", "wi")

# Create submission folder for every board
paste0("data/", format(start_month, "%Y-%m"), "/submitted/", boards) %>%
  walk(use_directory)

# Create folder for trend files
use_directory("trend")


### 4 - Define filepaths dependent on whether running on server or desktop ----

stats <- case_when(
  sessionInfo()$platform == "x86_64-pc-linux-gnu (64-bit)" ~ "/conf",
  TRUE ~ "//stats"
)

cl_out <- case_when(
  sessionInfo()$platform == "x86_64-pc-linux-gnu (64-bit)" ~ 
    "/conf/linkage/output",
  TRUE ~ "//stats/cl-out"
)


### 5 - Define lookup files ----

postcode <- function(){
  glue("{cl_out}/lookups/Unicode/Geography/Scottish Postcode Directory/",
       "Scottish_Postcode_Directory_2020_1.rds") %>%
    read_rds() %>%
    clean_names() %>%
    select(pc7, datazone = datazone2011)
}
  
specialty <- function(){
  glue("{cl_out}/lookups/Unicode/National Reference Files/specialt.rds") %>%
    read_rds() %>%
    clean_names() %>%
    select(spec_code = speccode, spec_desc = description)
}
  
location <- function(){
  glue("{cl_out}/lookups/Unicode/National Reference Files/location.rds") %>%
    read_rds() %>%
    clean_names() %>%
    select(location, location_name = locname)
}
  
hscp_locality <- function(){
  glue("{cl_out}/lookups/Unicode/Geography/HSCP Locality/",
       "HSCP Localities_DZ11_Lookup_20191216.rds") %>%
    read_rds() %>%
    clean_names() %>%
    select(datazone = datazone2011,
           hscp = hscp2019name,
           locality = hscp_locality)
}


### END OF SCRIPT ###