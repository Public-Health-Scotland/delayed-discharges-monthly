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

walk(list.files(here("functions"), full.names = TRUE), source)


### 1 - Read in csv file for each board and add together ----

# If a file doesn't exist or has no data, an error message will appear
# This doesn't mean that the code hasn't worked for other board files
# Please check the error messages only appear for files you know don't exist

# Ignore warning message: Missing column names filled in: 'X18'...

scotland <-
  paste0(here("data", format(start_month, "%Y-%m"), 
              "submitted", boards, boards), 
         ".csv") %>%
  map(possibly(read_clean_data, otherwise = NULL, quiet = FALSE)) %>%
  reduce(bind_rows)


### 2 - Exclusions ----

# Remove records where Ready for Discharge date is same as Discharge Date
scotland %<>%
  filter(readyfordischargedate != date_discharge | is.na(date_discharge))


### 3 - Recoding ----

# Add monthflag if missing
scotland %<>%
  replace_na(list(monthflag = format(start_month, "%b-%y")))


# Recode Local Authority codes to names
scotland %<>%
  mutate(local_authority_area = 
           case_when(
             nchar(local_authority_area) == 1 ~ paste0("0", local_authority_area),
             local_authority_area == "Aberdeen City" ~ "Aberdeen",
             TRUE ~ local_authority_area
           )) %>%
  left_join(
    read_rds(here("lookups", "local-authority.rds")), 
    by = c("local_authority_area" = "la_code")
  ) %>%
  mutate(local_authority_area = 
           case_when(!is.na(la_desc) ~ la_desc,
                     TRUE ~ local_authority_area)) %>%
  select(-la_desc)


### END OF SCRIPT ###