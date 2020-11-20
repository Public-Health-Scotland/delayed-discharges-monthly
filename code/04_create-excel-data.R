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
  
  # Select current month only
  filter(between(census_date, start_month, end_month)) %>%
  
  filter(census_flag == 1) %>%
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
  
  # Add 'All' ages
  group_by(fin_yr, month, level, areaname, delay_reason, reason_group_1,
           reason_group_2, delay_length_group, location_type) %>%
  group_modify(
    ~ bind_rows(.x,
                summarise(.x,
                          age_group = "All",
                          census_delays = sum(census_delays)))
  ) %>%
  ungroup() %>%
  
  pivot_longer(
    cols = delay_reason:reason_group_2,
    names_to = "reason_breakdown",
    values_to = "delay_reason"
  ) %>%
  
  # Remove age breakdown for reason codes
  filter(!(age_group != "All" & reason_breakdown == "delay_reason")) %>%
  
  group_by(across(-census_delays)) %>%
  summarise(census_delays = sum(census_delays), .groups = "drop") %>%
  
  group_by(across(fin_yr:age_group)) %>%
  group_modify(
    ~ bind_rows(.x,
                summarise(.x %>% filter(reason_breakdown == "reason_group_1"),
                          delay_reason = "All",
                          census_delays = sum(census_delays))
    )
  ) %>%
  
  group_modify(
    ~ bind_rows(.x,
                summarise(.x %>% filter(reason_breakdown == "reason_group_1" &
                                          delay_reason != "Code 9"),
                          delay_reason = "Standard",
                          census_delays = sum(census_delays))
    )
  ) %>%
  ungroup() %>%
  
  pivot_wider(names_from = delay_length_group,
              values_from = census_delays) %>%
  
  mutate(across(`1-3 days`:`6-12 weeks`, ~ replace_na(., 0))) %>%
  mutate(census_delays = reduce(select(., `1-3 days`:`6-12 weeks`), `+`)) %>%
  pivot_wider(names_from = location_type,
              values_from = census_delays) %>%
  
  mutate(across(`1-3 days`:`6-12 weeks`, ~ replace_na(., 0))) %>%
  mutate(census_delays = reduce(select(., `1-3 days`:`6-12 weeks`), `+`)) %>%
  group_by(across(c(!where(is.numeric), level))) %>%
  summarise(across(where(is.numeric), ~ sum(., na.rm = TRUE)),
            .groups = "drop") %>%
  ungroup() %>%
  relocate(level, .before = areaname)


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


### END OF SCRIPT ###