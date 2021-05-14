#########################################################################
# Name of file - 02_msg.R
# Data release - Monthly Delayed Discharges Management Information
# Original Authors - Peter McClurg
# Original Date - December 2020
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Create MSG management information
#########################################################################


### 0 - Load setup environment ----

source(here::here("code", "00_setup-environment.R"))


### 1 - Read in trend file ----

trend <- 
  read_rds(
    here("trend", paste0(format(start_month, "%Y-%m"), "_trend.rds"))
  ) %>%
  
  # Remove Code 100's
  filter(delay_reason_1 != 100)


### 2 - Add month number ----

trend %<>% 
  mutate(month_num = month(month_date))


### 3 - Add age groups ----

trend %<>%
  mutate(
    age_group = age_group(age, from = 0, to = 100, by = 5),
    age_group = if_else(between(age, 18, 24), "18-24", age_group)
  )


### 4 - Recode out of area indicator ----
  
trend %<>%
  mutate(
    area_treated = if_else(out_of_area == 1, "Outwith HBres", "Within HBres")
  )


### 5 - Recode some local authorities and HSCPs ----

trend %<>% 
  mutate(
    local_authority = case_when(
      local_authority %in% c("Shetland", "Orkney") ~ 
        paste(local_authority, "Islands"),
      TRUE ~ local_authority
    ),
    hscp = case_when(
      str_detect(hscp, " and ") ~ str_replace(hscp, " and ", " & "),
      hscp == "Western Isles" ~ "Na h-Eileanan Siar",
      TRUE ~ hscp
    )
  )                    


### 6 - Aggregate  ----

msg <- 
  trend %>% 
  group_by(council = local_authority, area_treated, location = location_code, 
           locality, derived_partnership = hscp, specialty = specialty_code, 
           age_group, month, month_num, year = cal_yr, 
           reason_for_delay = reason_group_1) %>% 
  summarise(delays_at_census = sum(census_flag),
            delayed_bed_days = sum(bed_days),
            .groups = "drop")


### 7 - Save data files ----

write_sav(
  msg,
  here("output", year(pub_date(start_month)), pub_date(start_month),
       "management-info", paste0(format(start_month, "%Y-%m"), "_dd-msg.sav"))
)

write_csv(
  msg,
  here("output", year(pub_date(start_month)), pub_date(start_month), 
       "management-info", paste0(format(start_month, "%Y-%m"), "_dd-msg.csv"))
)


### END OF SCRIPT ###