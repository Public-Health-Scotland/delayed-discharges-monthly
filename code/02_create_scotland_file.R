#########################################################################
# Name of file - 02_create_scotland_file.R
# Data release - Monthly Delayed Discharges publication
# Original Authors - Alice Byers
# Orginal Date - July 2020
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Add all health board files to create one Scotland file
#               and add derived fields for analysis
#########################################################################


### 1 - Load setup environment and functions ----

source(here::here("code", "00_setup_environment.R"))

source(here("functions", "read_clean_data.R"))


### 1 - Read in csv file for each board and add together ----

# If a file doesn't exist or has no data, an error message will appear
# This doesn't mean that the code hasn't worked for other board files
# Please check the error messages only appear for files you know don't exist

# Ignore warning message: Missing column names filled in: 'X18'...

scotland <-
  paste0(here("data", format(start_month, "%Y-%m"), "submitted", boards, boards), 
         ".csv") %>%
  map(possibly(read_clean_data, otherwise = tibble(NA), quiet = FALSE)) %>%
  reduce(bind_rows) %>%
  remove_empty(c("rows", "cols"))


### END OF SCRIPT ###