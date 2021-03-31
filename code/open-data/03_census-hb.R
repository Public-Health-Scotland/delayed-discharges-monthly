#########################################################################
# Name of file - 03_census-hb.R
# Data release - Monthly Delayed Discharges Open Data
# Original Authors - Alice Byers
# Original Date - March 2020
#
# Written/run on - RStudio server
# Version of R - 3.6.1
#
# Description - Creates Census file by Health Board
#########################################################################


### 1 - Load setup-environment ----

source(here::here("code", "00_setup-environment.R"))


### 2 - Load trend file ----

trend <-
  
  read_rds(
    here("trend", paste0(format(start_month, "%Y-%m"), "_trend.rds"))
  ) %>%
  
  # Remove Code 100 delays
  filter(delay_reason_1 != 100)


### 3 - Aggregate data for open data platform ----

census_hb <-
  trend %>%
  group_by(MonthOfDelay = format(month_date, "%Y%m"),
           HBT = health_board_code,
           AgeGroup = age_group,
           ReasonForDelay = reason_group_1) %>%
  summarise(NumberOfCensusDelays = sum(census_flag),
            .groups = "drop") %>%
  mutate(ReasonForDelay = case_when(
    ReasonForDelay == "Code 9" ~ "Code 9 Reasons",
    str_detect(ReasonForDelay, "^Patient") ~ "Patient and Family Related Reasons",
    TRUE ~ ReasonForDelay
  )) %>%
  complete(MonthOfDelay, HBT, AgeGroup, ReasonForDelay,
           fill = list(NumberOfCensusDelays = 0))


### 4 - Add aggregate rows ----

census_hb %<>%
  bind_rows(
    census_hb %>%
      mutate(HBT = "S92000003",
             HBTQF = "d") %>%
      group_by_at(vars(-NumberOfCensusDelays)) %>%
      summarise_all(sum) %>%
      ungroup()
  )

census_hb %<>%
  bind_rows(
    census_hb %>%
      mutate(AgeGroup = "18+",
             AgeGroupQF = "d") %>%
      group_by_at(vars(-NumberOfCensusDelays)) %>%
      summarise_all(sum) %>%
      ungroup()
  )

census_hb %<>%
  bind_rows(
    census_hb %>%
      mutate(ReasonForDelay = "All Delay Reasons",
             ReasonForDelayQF = "d") %>%
      group_by_at(vars(-NumberOfCensusDelays)) %>%
      summarise_all(sum) %>%
      ungroup()
  )


### 5 - Recode Age Group and NAs

census_hb %<>%
  mutate_if(is.character, ~ replace_na(., "")) %>%
  mutate(AgeGroup = str_replace_all(AgeGroup, "\\+", "plus"))


### 6 - Order columns and sort rows ----

census_hb %<>%
  mutate(AgeGroup = 
           fct_relevel(as_factor(AgeGroup),
                       c("18plus", "18-74", "75plus")),
         ReasonForDelay = 
           fct_relevel(as_factor(ReasonForDelay),
                       c("All Delay Reasons",
                         "Code 9 Reasons",
                         "Health and Social Care Reasons",
                         "Patient and Family Related Reasons"))) %>%
  select(MonthOfDelay, contains("HBT"), contains("AgeGroup"),
         contains("ReasonForDelay"), everything()) %>%
  arrange(MonthOfDelay, HBT, AgeGroup, ReasonForDelay)


### 7 - Save file ----

write_csv(
  census_hb,
  here("output", year(pub_date(start_month)),
       pub_date(start_month), "open-data",
       glue("{format(start_month, '%Y-%m')}_",
            "delayed-discharge-census-health-board.csv"))
)
  

### END OF SCRIPT ###