#########################################################################
# Name of file - 04_census-ca.R
# Data release - Monthly Delayed Discharges Open Data
# Original Authors - Alice Byers
# Original Date - March 2020
#
# Written/run on - RStudio server
# Version of R - 3.6.1
#
# Description - Creates Census file by Council Area/Local Authority
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

census_ca <-
  trend %>%
  group_by(MonthOfDelay = format(month_date, "%Y%m"),
           CA = local_authority_code,
           AgeGroup = age_group,
           ReasonForDelay = reason_group_1) %>%
  summarise(NumberOfCensusDelays = sum(census_flag),
            .groups = "drop") %>%
  mutate(ReasonForDelay = case_when(
    ReasonForDelay == "Code 9" ~ "Code 9 Reasons",
    str_detect(ReasonForDelay, "^Patient") ~ "Patient and Family Related Reasons",
    TRUE ~ ReasonForDelay
  )) %>%
  complete(MonthOfDelay, CA, AgeGroup, ReasonForDelay,
           fill = list(NumberOfCensusDelays = 0)) %>%
  mutate(CAQF = if_else(is.na(CA), ":", NA_character_))


### 4 - Add aggregate rows ----

census_ca %<>%
  bind_rows(
    census_ca %>%
      mutate(CA = "S92000003",
             CAQF = "d") %>%
      group_by_at(vars(-NumberOfCensusDelays)) %>%
      summarise_all(sum) %>%
      ungroup()
  )

census_ca %<>%
  bind_rows(
    census_ca %>%
      mutate(AgeGroup = "18+",
             AgeGroupQF = "d") %>%
      group_by_at(vars(-NumberOfCensusDelays)) %>%
      summarise_all(sum) %>%
      ungroup()
  )

census_ca %<>%
  bind_rows(
    census_ca %>%
      mutate(ReasonForDelay = "All Delay Reasons",
             ReasonForDelayQF = "d") %>%
      group_by_at(vars(-NumberOfCensusDelays)) %>%
      summarise_all(sum) %>%
      ungroup()
  )


### 5 - Recode Age Group and NAs

census_ca %<>%
  mutate_if(is.character, ~ replace_na(., "")) %>%
  mutate(AgeGroup = str_replace_all(AgeGroup, "\\+", "plus"))


### 6 - Order columns and sort rows ----

census_ca %<>%
  mutate(AgeGroup = 
           fct_relevel(as_factor(AgeGroup),
                       c("18plus", "18-74", "75plus")),
         ReasonForDelay = 
           fct_relevel(as_factor(ReasonForDelay),
                       c("All Delay Reasons",
                         "Code 9 Reasons",
                         "Health and Social Care Reasons",
                         "Patient and Family Related Reasons")),
         CA = fct_relevel(as_factor(CA),
                          c("", "S92000003"), after = Inf)) %>%
  select(MonthOfDelay, contains("CA"), contains("AgeGroup"),
         contains("ReasonForDelay"), everything()) %>%
  arrange(MonthOfDelay, CA, AgeGroup, ReasonForDelay)


### 7 - Save file ----

write_csv(
  census_ca,
  here("output", year(pub_date(start_month)),
       pub_date(start_month), "open-data",
       glue("{format(start_month, '%Y-%m')}_",
            "delayed-discharge-census-council-area.csv"))
)
  

### END OF SCRIPT ###