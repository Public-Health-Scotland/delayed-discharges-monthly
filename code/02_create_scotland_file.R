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


### 0 - Load setup environment and functions ----

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

# Remove under 18s
scotland %<>%
  mutate(age_at_rdd = 
           interval(patient_dob, readyfordischargedate) %>%
           time_length("year") %>%
           floor()) %>%
  filter(age_at_rdd >= 18)

# Remove records where Ready for Discharge date is same as Discharge Date
scotland %<>%
  filter(readyfordischargedate != date_discharge | is.na(date_discharge))

# Remove records where Ready for Discharge date is last day of month
scotland %<>%
  filter(readyfordischargedate != end_month)
  

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

# Recode out of area flag
scotland %<>%
  mutate(outofareacaseindicator = 
           case_when(
             str_detect(outofareacaseindicator, "^(Y|y)") ~ 1,
             str_detect(outofareacaseindicator, "^(N|n)") ~ 0,
             is.na(outofareacaseindicator) ~ 0
           ))

# Recode gender
scotland %<>%
  mutate(gender = 
           case_when(
             str_detect(gender, "(1|M)") ~ "Male",
             str_detect(gender, "(2|F)") ~ "Female"
           ))

# Recode reason for delay
scotland %<>%
  
  # Ensure all codes upper case
  mutate(across(contains("reasonfordelay"), toupper)) %>%
  
  # Remove leading zero from code 9
  mutate(reasonfordelay = 
           if_else(reasonfordelay == "09", "9", reasonfordelay)
  ) %>%
  
  # Code missing reason for delay as 11A
  mutate(reasonfordelay = 
           if_else(is.na(reasonfordelay) & is.na(reasonfordelaysecondary),
                   "11A", 
                   reasonfordelay)
  ) 


### END OF SCRIPT ###