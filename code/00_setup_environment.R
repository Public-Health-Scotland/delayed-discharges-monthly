#########################################################################
# Name of file - 00_setup_environment.R
# Data release - Monthly Delayed Discharges publication
# Original Authors - Alice Byers
# Orginal Date - July 2020
#
# Type - Reproducible Analytical Pipeline
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Sets up environment required for running publication RAP. 
# This is the only file which should require updating every
# time the RAP process is run
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


### 2 - Define month end date ----

end_date <- dmy(30042020)


### 3 - Define whether running or server or desktop

if(sessionInfo()$platform == "x86_64-pc-linux-gnu (64-bit)"){
  platform <- "server"
}else{
  platform <- "desktop"
}

# Define root directory for stats server based on whether script is running 
# locally or on server
filepath <- if_else(platform == "server",
                    "/conf/linkage/output/",
                    "//stats/cl-out/")


### END OF SCRIPT ###