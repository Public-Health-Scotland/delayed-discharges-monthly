#########################################################################
# Name of file - 00_setup-environment.R
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
library(phsmethods)   # For internal PHS functions
library(magrittr)     # For the %<>%
library(glue)         # For working with strings
library(almanac)      # For working with recurring dates
library(usethis)      # For creating new folders
library(eeptools)     # For calculating age
library(ggplot2)      # For creating charts
library(knitr)        # For creating tables in markdown
library(forcats)      # For dealing with factors
library(here)         # For the here() function
library(rmarkdown)    # For rendering markdown documents
library(zip)          # For archiving files


### 2 - Define month start date and derive end date ----

start_month <- dmy(01122020)

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

# Create folder for output
source(here("functions", "pub_date.R"))
use_directory(paste0("output/", pub_date(start_month)))


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

pc_lookup <- function(){
  glue("{cl_out}/lookups/Unicode/Geography/Scottish Postcode Directory/",
       "Scottish_Postcode_Directory_2020_2.rds") %>%
    read_rds() %>%
    clean_names() %>%
    select(pc7, data_zone = datazone2011)
}
  
spec_lookup <- function(){
  glue("{cl_out}/lookups/Unicode/National Reference Files/specialt.sav") %>%
    read_sav() %>%
    clean_names() %>%
    select(specialty_code = speccode, specialty_desc = description)
}
  
location_lookup <- function(){
  glue("{cl_out}/lookups/Unicode/National Reference Files/location.sav") %>%
    read_sav() %>%
    clean_names() %>%
    select(location_code = location, 
           location_name = locname)
}
  
hscp_locality_lookup <- function(){
  glue("{cl_out}/lookups/Unicode/Geography/HSCP Locality/",
       "HSCP Localities_DZ11_Lookup_20200825.rds") %>%
    read_rds() %>%
    clean_names() %>%
    select(data_zone = datazone2011,
           hscp = hscp2019name,
           locality = hscp_locality)
}

hb_lookup <- function(){
  paste0("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-",
         "d6e17229cc7b/resource/652ff726-e676-4a20-abda-435b98dd7bdc/",
         "download/hb14_hb19.csv") %>%
    {suppressMessages(read_csv(.))} %>%
    clean_names() %>%
    filter(is.na(hb_date_archived)) %>%
    mutate(hb_name = str_replace(hb_name, " and ", " & ")) %>%
    select(health_board_code = hb, 
           health_board = hb_name)
}

la_lookup <- function(){
  paste0("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-",
           "d6e17229cc7b/resource/967937c4-8d67-4f39-974f-fd58c4acfda5/",
           "download/ca11_ca19.csv") %>%
    {suppressMessages(read_csv(.))} %>%
    clean_names() %>%
    filter(is.na(ca_date_archived)) %>%
    mutate(ca_name = case_when(
      ca_name == "Na h-Eileanan Siar" ~ "Comhairle nan Eilean Siar",
      str_detect(ca_name, " Islands") ~ str_remove(ca_name, " Islands"),
      str_detect(ca_name, " and ") ~ str_replace(ca_name, " and ", " & "),
      TRUE ~ ca_name
    )) %>%
    select(local_authority_code = ca, 
           local_authority = ca_name) %>%
    distinct()
}


### END OF SCRIPT ###