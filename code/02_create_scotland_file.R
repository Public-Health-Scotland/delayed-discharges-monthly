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

# Add month flag
scotland %<>%
  mutate(monthflag = format(start_month, "%b %Y"))

# Recode Local Authority codes to names
scotland %<>%
  left_join(
    read_rds(here("lookups", "local-authority.rds")), 
    by = c("local_authority_area" = "la_code")
  ) %>%
  mutate(local_authority_area = case_when(
    !is.na(la_desc) ~ la_desc,
    local_authority_area == "Aberdeen City" ~ "Aberdeen",
    TRUE ~ local_authority_area
  )) %>%
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
  # Code missing reason for delay as 11A
  mutate(reasonfordelay = 
           if_else(is.na(reasonfordelay) & is.na(reasonfordelaysecondary),
                   "11A", 
                   reasonfordelay)) 


### 4 - Derivations ----

# Add age groups
scotland %<>%
  mutate(age_grouping = case_when(
    age_at_rdd %in% 18:74 ~ "18-74",
    age_at_rdd >=   75    ~ "75+"
  ))

# Add Reason for Delay groupings
scotland %<>%
  mutate(reason_match =
           case_when(
             reasonfordelay == 9 ~ reasonfordelaysecondary,
             TRUE ~ reasonfordelay
           )) %>%
  left_join(
    read_rds(here("lookups", "reason-for-delay.rds")),
    by = c("reason_match" = "code")
  ) %>%
  rename(reas1 = group1,
         reas2 = group2,
         delay_description = description) %>%
  select(-reason_match)

# Add census flag
scotland %<>%
  mutate(
    census_date = census_date(start_month),
    census_flag = case_when(
      reasonfordelaysecondary %in% c("26X", "46X") ~ 0,
      readyfordischargedate < census_date & is.na(date_discharge) ~ 1,
      readyfordischargedate < census_date & date_discharge >= census_date ~ 1,
      TRUE ~ 0
    )
  )

# Add disch within 3 working days of census
scotland %<>%
  mutate(dischargewithin3days_census = case_when(
    census_flag == 1 & date_discharge <= census_date + days(5) ~ 1,
    TRUE ~ 0
  ))

# Calculate number of bed days in month
scotland %<>%
  mutate(
    start_date = if_else(readyfordischargedate < start_month, 
                         start_month - days(1),
                         readyfordischargedate),
    end_date   = if_else(date_discharge > end_month | is.na(date_discharge),
                         end_month, 
                         date_discharge),
    obds = time_length(interval(start_date, end_date), "days")
  ) %>%
  select(-start_date, -end_date)

# Calulcate length of delay at census
scotland %<>%
  mutate(delay_at_census = case_when(
    census_flag == 1 ~ 
      time_length(interval(readyfordischargedate, census_date), "days"),
    TRUE ~ 0
  ))

# Add delay length grouping
scotland %<>%
  mutate(delay_length_group = case_when(
    delay_at_census %in% 1:3     ~ "1-3 days",
    delay_at_census %in% 4:14    ~ "3-14 days",
    delay_at_census %in% 15:28   ~ "2-4 weeks",
    delay_at_census %in% 29:42   ~ "4-6 weeks",
    delay_at_census %in% 43:84   ~ "6-12 weeks",
    delay_at_census %in% 85:182  ~ "3-6 months",
    delay_at_census %in% 183:365 ~ "6-12 months",
    delay_at_census > 365        ~ "12+ months",
    census_flag == 0             ~ NA_character_
  ),
  delay_names = case_when(
    !is.na(delay_length_group) ~
      paste0("delay", 
             delay_length_group %>%
               str_remove_all(" ") %>%
               str_replace("-", "to") %>%
               str_replace("12\\+", "_over12")),
    TRUE ~ NA_character_
  ),
  delay_value = 1) %>%
  pivot_wider(names_from  = delay_names,
              values_from = delay_value,
              values_fill = list(delay_value = 0)) %>%
  select(-`NA`) %>%
  relocate(c("delay1to3days", "delay3to14days", "delay2to4weeks", "delay4to6weeks",
             "delay6to12weeks", "delay3to6months", "delay6to12months", "delay_over12months"),
           .after = delay_length_group) %>%
  mutate(
    delay_over3days = (delay_at_census > 3) * 1,
    delay_under2wks = (delay_at_census <= 14) * 1,
    delay_over2wks  = (delay_at_census > 14) * 1,
    delay_over4wks  = (delay_at_census > 28) * 1,
    delay_over6wks  = (delay_at_census > 42) * 1
  )

# Add hospital type
scotland %<>%
  left_join(
    read_rds(here("lookups", "acute-hospitals.rds")) %>% 
      select(-hospname) %>%
      mutate(acute = 1),
    by = c("health_location_code" = "hosp")
  ) %>%
  replace_na(list(acute = 0)) %>%
  mutate(
    gpled = (acute == 0 & specialty_code == "E12") * 1,
    notgpled = (acute == 0 & gpled == 0) * 1
  )

# Add discharged within month flag
scotland %<>%
  mutate(disch_in_month = case_when(
    between(date_discharge, start_month, end_month) ~ 1,
    TRUE ~ 0
  ))

# Add patient count
scotland %<>%
  mutate(no_of_patients = 1)

# Add weekdays
scotland %<>%
  mutate(rdd_day = weekdays(readyfordischargedate), 
         .after = readyfordischargedate) %>%
  mutate(disch_day = weekdays(date_discharge),
         .after = date_discharge)

# Add date information
scotland %<>%
  mutate(
    fin_yr = fin_year(start_month),
    cal_yr = year(start_month),
    cennum = census_number(start_month),
    .before = everything()
  )


### 5 - Save file ----

write_rds(
  scotland,
  here("data", format(start_month, "%Y-%m"),
       paste0(format(start_month, "%Y-%m"), "_scotland.rds")),
  compress = "gz"
)


### END OF SCRIPT ###