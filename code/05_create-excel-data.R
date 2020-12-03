#########################################################################
# Name of file - 04_create-excel-data.R
# Data release - Monthly Delayed Discharges publication
# Original Authors - Alice Byers
# Orginal Date - November 2020
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Write out data sheets required for excel tables
#########################################################################


### 0 - Load setup environment ----

source(here::here("code", "00_setup-environment.R"))


### 1 - Read in trend file and remove Code 100's ----

trend <-
  read_rds(
    here("trend", 
         paste0(format(start_month, "%Y-%m"), "_trend.rds"))
  ) %>%
  filter(delay_reason_1 != 100)


### 2 - Create Bed Days data sheets ----

bed_days <-
  
  trend %>%
  
  # Select current month only
  filter(between(census_date, start_month, end_month)) %>%

  # Aggregate by required breakdowns
  group_by(health_board, local_authority, age_group, reason_group_1) %>%
  summarise(bed_days = sum(bed_days), .groups = "drop") %>%
  
  # Add Scotland rows
  group_by(age_group, reason_group_1) %>%
  group_modify(
    ~ bind_rows(.x,
                summarise(.x,
                          health_board = "Scotland",
                          bed_days = sum(bed_days)))
  ) %>%
  
  # Add All Ages rows
  group_by(health_board, local_authority, reason_group_1) %>%
  group_modify(
    ~ bind_rows(.x,
                summarise(.x,
                          age_group = "All",
                          bed_days = sum(bed_days)))
  ) %>%
  
  # Add All reason rows
  group_by(health_board, local_authority, age_group) %>%
  group_modify(
    ~ bind_rows(.x,
                summarise(.x,
                          reason_group_1 = "All",
                          bed_days = sum(bed_days)))
  ) %>%
  
  # Add 'Standard' reason rows
  group_by(health_board, local_authority, age_group) %>%
  group_modify(
    ~ bind_rows(.x,
                summarise(.x %>% filter(!reason_group_1 %in% c("All", "Code 9")),
                          reason_group_1 = "Standard",
                          bed_days = sum(bed_days)))
  ) %>%
  
  # Complete rows for all breakdowns
  ungroup() %>%
  complete(health_board = c(unique(trend$health_board), "Scotland"),
           local_authority = unique(trend$local_authority),
           age_group = c(unique(trend$age_group), "All"),
           reason_group_1 = c(unique(trend$reason_group_1), "All", "Standard"),
           fill = list(bed_days = 0)) %>%
  
  mutate(across(health_board, ~ str_remove(., "NHS ")))

# Health Board data sheet
bed_days_hb <-
  bed_days %>%
  group_by(health_board, age_group, reason_group_1) %>%
  summarise(bed_days = sum(bed_days), .groups = "drop")

# Local Authority data sheet
bed_days_la <-
  bed_days %>%
  filter(!is.na(local_authority)) %>%
  group_by(local_authority, age_group, reason_group_1) %>%
  summarise(bed_days = sum(bed_days), .groups = "drop")


### 3 - Create census data sheet ----

census_scot <-
  
  trend %>%
  
  # Select census records in current month only
  filter(between(census_date, start_month, end_month)) %>%
  filter(census_flag == 1) %>%
  
  # Aggregate to Scotland level
  mutate(delay_reason = case_when(
    delay_reason_1 == 9 ~ delay_reason_2,
    TRUE ~ delay_reason_1
  )) %>%
  group_by(fin_yr, month, age_group, delay_reason, reason_group_1,
           reason_group_2, delay_length_group, location_type) %>%
  summarise(
    level = 1,
    areaname = "Scotland",
    census_delays = sum(census_flag),
    .groups = "drop"
  ) %>%
  
  # Restructure to get rows for various delay reason breakdowns
  pivot_longer(
    cols = delay_reason:reason_group_2,
    names_to = "reason_breakdown",
    values_to = "delay_reason"
  )

census_hb_la <-
  
  trend %>%
  
  # Select census records in current month only
  filter(between(census_date, start_month, end_month)) %>%
  filter(census_flag == 1) %>%
  
  # Restructure and aggregate to HB/LA level
  pivot_longer(
    cols = c(health_board, local_authority),
    names_to = "level",
    values_to = "areaname"
  ) %>%
  group_by(fin_yr, month, level, areaname, age_group, 
           reason_group_1, reason_group_2,
           delay_length_group, location_type) %>%
  summarise(census_delays = sum(census_flag), .groups = "drop") %>%
  mutate(level = case_when(
    level == "health_board" ~ 2,
    level == "local_authority" ~ 3
  )) %>%
  
  # Restructure to get rows for various delay reason breakdowns
  pivot_longer(
    cols = reason_group_1:reason_group_2,
    names_to = "reason_breakdown",
    values_to = "delay_reason"
  )

census <-
  
  # Join Scotland/HB/LA rows
  bind_rows(census_scot, census_hb_la) %>%
  
  # Add 'All' ages
  group_by(fin_yr, month, level, areaname, reason_breakdown, delay_reason,
           delay_length_group, location_type) %>%
  group_modify(
    ~ bind_rows(.x,
                summarise(.x,
                          age_group = "All",
                          census_delays = sum(census_delays)))
  ) %>%
  ungroup() %>%
  
  # Remove age breakdown for reason codes
  filter(!(age_group != "All" & reason_breakdown == "delay_reason")) %>%
  
  # Add All reasons breakdown
  group_by(fin_yr, month, level, areaname, delay_length_group,
           location_type, age_group) %>%
  group_modify(
    ~ bind_rows(.x,
                summarise(.x %>% filter(reason_breakdown == "reason_group_1"),
                          reason_breakdown = "reason_group_1",
                          delay_reason = "All",
                          census_delays = sum(census_delays))
    )
  ) %>%
  
  # Add Standard reasons breakdown
  group_modify(
    ~ bind_rows(.x,
                summarise(.x %>% filter(reason_breakdown == "reason_group_1" &
                                          !delay_reason %in% c("All", "Code 9")),
                          reason_breakdown = "reason_group_1",
                          delay_reason = "All Delays excl. Code 9",
                          census_delays = sum(census_delays))
    )
  ) %>%
  ungroup() %>%
  
  # Aggregate
  group_by(across(-census_delays)) %>%
  summarise(census_delays = sum(census_delays), .groups = "drop") %>%
  
  # Add columns for delay length groupings
  pivot_wider(names_from = delay_length_group, 
              values_from = census_delays,
              values_fill = list(census_delays = 0))%>%
  mutate(census_delays = rowSums(select(., -c(fin_yr:delay_reason)))) %>%
  
  # Calculate total delays across all location types
  group_by(across(c(fin_yr:delay_reason, -location_type, census_delays))) %>%
  mutate(across(matches("^[1-9]"), sum)) %>%
  ungroup() %>%
  
  # Add columns for location type
  pivot_wider(names_from = location_type,
              values_from = census_delays,
              values_fill = list(census_delays = 0)) %>%
  mutate(census_delays = rowSums(select(., Acute:`Not GP Led`)), 
         .after = delay_reason) %>%
  
  # Add extra delay length breakdowns
  rowwise() %>%
  mutate(
    DelayOver3days = sum(`4-14 days`, `2-4 weeks`, `4-6 weeks`, `6-12 weeks`, 
                         `3-6 months`, `6-12 months`, `12+ months`),
    DelayUnder2wks = sum(`1-3 days`, `4-14 days`),
    DelayOver6wks  = sum(`6-12 weeks`, `3-6 months`, `6-12 months`, 
                         `12+ months`),
    DelayOver4wks  = sum(`4-6 weeks`, `6-12 weeks`, `3-6 months`, `6-12 months`, 
                         `12+ months`),
    DelayOver2wks  = sum(`2-4 weeks`, `4-6 weeks`, `6-12 weeks`, `3-6 months`, 
                         `6-12 months`, `12+ months`)
  ) %>%
  ungroup() %>%
  
  # Set some breakdowns to zero where too low level for publication
  mutate(
    across(c(Acute:`Not GP Led`), 
           ~ case_when(
             reason_breakdown == "reason_group_1" & age_group == "All" ~ .,
             TRUE ~ 0
           )),
    across(c(matches("^[1-9]"), DelayUnder2wks),
           ~ case_when(
             reason_breakdown == "reason_group_1" ~ .,
             TRUE ~ 0
           )),
    across(matches("^Delay", ignore.case = FALSE),
           ~ case_when(
             reason_breakdown == "reason_group_1" | age_group == "All" ~ .,
             TRUE ~ 0
           )),
    across(c(`6-12 weeks`, `3-6 months`, `6-12 months`, `12+ months`),
           ~ case_when(
             delay_reason != "All" ~ 0,
             TRUE ~ .
           ))
  ) %>%
  
  # Recoding to match SPSS output
  mutate(
    fin_yr = str_replace(fin_yr, "/", "-"),
    month_year = month,
    month = word(month),
    delay_reason = case_when(
      delay_reason == "Assessment" ~ "H&SC - Community Care Assessment",
      delay_reason %in% c("Care Arrangements", "Place Availability") ~ 
        paste0("H&SC - ", delay_reason),
      str_starts(delay_reason, "Patient/") ~ 
        str_replace(delay_reason, "Reasons", "reasons"),
      delay_reason %in% c("Disagreements", "Legal/Financial", "Other") ~
        paste0("Patient/Carer/Family-related reasons: ", delay_reason),
      str_starts(delay_reason, "Other Code 9") ~ 
        "Other code 9 reasons (not AWI)",
      TRUE ~ delay_reason
    )
  ) %>%
  
  rename_with(
    ~ paste0("Delay", str_replace(word(.), "-", "to"), word(., 2)),
    matches("^[1-9]")
  ) %>%
  rename_with(
    ~ tolower(str_replace_all(., " ", "")),
    Acute:`Not GP Led`
  ) %>%
  rename(DelayOver12months = `Delay12+months`) %>%
  
  # Remove rows with no delays
  filter(census_delays != 0) %>%
  
  # Final aggregate
  group_by(across(c(!where(is.numeric), level))) %>%
  summarise(across(where(is.numeric), sum), .groups = "drop") %>%
  
  # Reorder columns
  select(fin_yr, month, month_year, level, areaname, age_group, delay_reason,
         census_delays, Delay1to3days, Delay4to14days, Delay2to4weeks,
         Delay4to6weeks, Delay6to12weeks, Delay3to6months, Delay6to12months,
         DelayOver12months, DelayOver3days, DelayUnder2wks, DelayOver6wks,
         DelayOver4wks, DelayOver2wks, acute, gpled, notgpled)


### 4 - Save data sheets ----

write_csv(
  bed_days_hb,
  here("output", format(start_month, "%Y-%m"),
       paste0(format(start_month, "%Y-%m"), "_bed-days-hb.csv"))
)

write_csv(
  bed_days_la,
  here("output", format(start_month, "%Y-%m"),
       paste0(format(start_month, "%Y-%m"), "_bed-days-la.csv"))
)

write_csv(
  census,
  here("output", format(start_month, "%Y-%m"),
       paste0(format(start_month, "%Y-%m"), "_census.csv"))
)


### END OF SCRIPT ###