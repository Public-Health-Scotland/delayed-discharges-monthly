#########################################################################
# Name of file - 05_create-discharges-tables.R
# Data release - Monthly Delayed Discharges publication
# Original Authors - Alice Byers
# Original Date - April 2020
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Create discharges excel tables
#########################################################################


### 0 - Load setup environment ----

source(here::here("code", "00_setup-environment.R"))


### 1 - Read in trend file ----

trend <-
  read_rds(
    here("trend", 
         paste0(format(start_month, "%Y-%m"), "_trend.rds"))
  ) %>%
  
  # Remove Code 100's and data prior to April 2019
  filter(delay_reason_1 != 100 &
           month_date >= dmy(01042018))


### 2 - Data wrangling ----

discharges_hb <-
  
  trend %>%
  
  # Select discharges within month
  filter(disch_in_month == 1) %>%
  
  # Exclude deaths and not fit for discharge
  filter(!discharge_reason %in% 4:5) %>%
  
  # Add discharge reason description
  mutate(discharge_reason = 
           case_when(
             discharge_reason == 1 ~ "placement",
             discharge_reason %in% 2:3 ~ "home"
           )) %>%
  
  # Aggregate and restructure
  group_by(month_date, health_board, discharge_reason) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(values_from = n, names_from = discharge_reason, 
              values_fill = list(n = 0)) %>%
  mutate(total = reduce(select(., home:placement), `+`)) %>%
  
  # Complete for all health boards
  complete(month_date, health_board = unique(trend$health_board), 
           fill = list(home = 0, placement = 0, total = 0)) %>%
  
  # Add Scotland rows
  group_by(month_date) %>%
  group_modify(~ adorn_totals(., name = "Scotland")) %>%
  ungroup() %>%
  
  # Add formatted date and lookup for excel vlookups
  mutate(month_date = format(month_date, "%B %Y"),
         lookup = paste0(month_date, health_board), .before = everything())


### 3 - Save data ----

write_csv(
  discharges_hb,
  here("output", year(pub_date(start_month)),
       pub_date(start_month), "publication",
       paste0(pub_date(start_month), "_discharges.csv"))
)


### END OF SCRIPT ###