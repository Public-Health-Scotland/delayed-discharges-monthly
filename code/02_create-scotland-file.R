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

source(here::here("code", "00_setup-environment.R"))

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
  mutate(age = age_calc(date_of_birth, ready_for_discharge_date,
                        units = "years") %>%
               floor(),
         .after = date_of_birth) %>%
  filter(age >= 18)

# Remove records where Ready for Discharge date is same as Discharge Date
scotland %<>%
  filter(ready_for_discharge_date != discharge_date | is.na(discharge_date))

# Remove records where Ready for Discharge date is last day of month
scotland %<>%
  filter(ready_for_discharge_date != end_month)
  

### 3 - Recoding ----

# Add month flag
scotland %<>%
  mutate(month = format(start_month, "%b %Y"))

# Recode Local Authority codes to names
scotland %<>%
  left_join(
    read_rds(here("lookups", "local-authority.rds")), 
    by = c("local_authority" = "la_code")
  ) %>%
  mutate(local_authority = case_when(
    !is.na(la_desc) ~ la_desc,
    TRUE ~ local_authority
  )) %>%
  select(-la_desc)

# Recode out of area flag
scotland %<>%
  mutate(out_of_area = 
           case_when(
             str_detect(out_of_area, "^(Y|y)") ~ 1,
             str_detect(out_of_area, "^(N|n)") ~ 0,
             is.na(out_of_area) ~ 0
           ))

# Recode sex
scotland %<>%
  mutate(sex = 
           case_when(
             str_detect(sex, "(1|M)") ~ "Male",
             str_detect(sex, "(2|F)") ~ "Female"
           ))

# Code missing reason for delay as 11A
scotland %<>%
  mutate(delay_reason_1 = 
           if_else(is.na(delay_reason_1) & is.na(delay_reason_2),
                   "11A", 
                   delay_reason_1))


### 4 - Derivations ----

# Add age groups
scotland %<>%
  mutate(age_group = case_when(
    age %in% 18:74 ~ "18-74",
    age >=   75    ~ "75+"
  ),
  .after =  age)

# Add Reason for Delay groupings
scotland %<>%
  mutate(reason_match =
           case_when(
             delay_reason_1 == 9 ~ delay_reason_2,
             TRUE ~ delay_reason_1
           )) %>%
  left_join(
    read_rds(here("lookups", "reason-for-delay.rds")),
    by = c("reason_match" = "code")
  ) %>%
  rename(reason_group_1 = group1,
         reason_group_2 = group2,
         reason_description = description) %>%
  relocate(starts_with("reason"), .after = delay_reason_2) %>%
  select(-reason_match)

# Calculate number of bed days in month
scotland %<>%
  mutate(
    start_date = if_else(ready_for_discharge_date < start_month, 
                         start_month - days(1),
                         ready_for_discharge_date),
    end_date   = if_else(discharge_date > end_month | is.na(discharge_date),
                         end_month, 
                         discharge_date),
    bed_days = time_length(interval(start_date, end_date), "days")
  ) %>%
  select(-start_date, -end_date)

# Add census flag
scotland %<>%
  mutate(census_date = census_date(start_month), .before = everything()) %>%
  mutate(
    census_flag = case_when(
      delay_reason_2 %in% c("26X", "46X") ~ 0,
      ready_for_discharge_date < census_date & is.na(discharge_date) ~ 1,
      ready_for_discharge_date < census_date & 
        discharge_date >= census_date ~ 1,
      TRUE ~ 0
    )
  )

# Calulcate length of delay at census
scotland %<>%
  mutate(delay_at_census = case_when(
    census_flag == 1 ~ 
      time_length(interval(ready_for_discharge_date, census_date), "days"),
    TRUE ~ 0
  ))

# Add delay length grouping
scotland %<>%
  mutate(
    delay_length_group = case_when(
      delay_at_census %in% 1:3     ~ "1-3 days",
      delay_at_census %in% 4:14    ~ "4-14 days",
      delay_at_census %in% 15:28   ~ "2-4 weeks",
      delay_at_census %in% 29:42   ~ "4-6 weeks",
      delay_at_census %in% 43:84   ~ "6-12 weeks",
      delay_at_census %in% 85:182  ~ "3-6 months",
      delay_at_census %in% 183:365 ~ "6-12 months",
      delay_at_census > 365        ~ "12+ months",
      census_flag == 0             ~ NA_character_
    ),
    delay_over3days = (delay_at_census > 3) * 1,
    delay_over2wks  = (delay_at_census > 14) * 1,
    delay_over4wks  = (delay_at_census > 28) * 1,
    delay_over6wks  = (delay_at_census > 42) * 1
  )

# Add hospital type
scotland %<>%
  left_join(
    read_rds(here("lookups", "acute-hospitals.rds")) %>% 
      select(-hospname) %>%
      mutate(location_type = "Acute"),
    by = c("location_code" = "hosp")
  ) %>%
  mutate(location_type = case_when(
    location_type == "Acute" ~ "Acute",
    specialty_code == "E12" ~ "GP Led",
    TRUE ~ "Not GP Led"
  )) %>%
  relocate(location_type, .after = location_code)

# Add discharged within month flag
scotland %<>%
  mutate(disch_in_month = case_when(
    between(discharge_date, start_month, end_month) ~ 1,
    TRUE ~ 0
  ),
  .after = discharge_reason)

# Add weekdays
scotland %<>%
  mutate(rdd_day = weekdays(ready_for_discharge_date), 
         .after = ready_for_discharge_date) %>%
  mutate(disch_day = weekdays(discharge_date),
         .after = discharge_date)

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