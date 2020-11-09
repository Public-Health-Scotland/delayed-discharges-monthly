#########################################################################
# Name of file - 03_create-trend-file.R
# Data release - Monthly Delayed Discharges publication
# Original Authors - Alice Byers
# Orginal Date - November 2020
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Add Scotland file for latest month to trend file
#########################################################################


### 0 - Load setup environment ----

source(here::here("code", "00_setup-environment.R"))


### 1 - Read in Scotland file and previous trend file ----

scotland <-
  read_rds(
    here("data", format(start_month, "%Y-%m"),
         paste0(format(start_month, "%Y-%m"), "_scotland.rds"))
  )

trend <-
  read_rds(
    here("trend", 
         paste0(format(start_month - months(1), "%Y-%m"), "_trend.rds"))
  )


### 2 - Add latest month Scotland file to previous month trend ----

trend %<>%
  bind_rows(scotland)


### 3 - Match lookup files ----

trend %<>%
  
  # Remove existing matched variables
  select(-c(data_zone, hscp, locality, location_name,
            health_board_code, local_authority_code)) %>%
  
  # Match on new matched variables
  
  # Datazone
  left_join(pc_lookup(), by = c("patient_postcode" = "pc7")) %>%
  relocate(data_zone, .after = patient_postcode) %>%
  
  # HSCP and Locality
  left_join(hscp_locality_lookup(), by = "data_zone") %>%
  relocate(hscp, locality, .after = local_authority) %>%
  
  # Location
  left_join(location_lookup(), by = "location_code") %>%
  relocate(location_name, .after = location_code) %>%
  
  # Health Board and Local Authority codes
  left_join(hb_lookup(), by = "health_board") %>%
  left_join(la_lookup(), by = "local_authority") %>%
  relocate(health_board_code, .after = health_board) %>%
  relocate(local_authority_code, .after = local_authority)


### END OF SCRIPT ###