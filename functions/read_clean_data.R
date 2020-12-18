#########################################################################
# Name of file - read_clean_data.R
# Data release - Monthly Delayed Discharges publication
# Original Authors - Alice Byers
# Orginal Date - July 2020
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Function to read in health board data from csv file and
#               standardise variable names and formatting
#########################################################################


read_clean_data <- function(filepath){
  
  if(!file.exists(filepath)){
    stop(paste("File does not exist:", filepath))
  }
  
  data <-
    read_csv(filepath, col_types = cols(.default = "c")) %>%
    remove_empty("rows") %>%
    select(-starts_with("X"))
  
  if(nrow(data) == 0){
    stop(paste("File does not contain any rows of data:", filepath))
  }
  
  data %<>%
    
    # Standardise variable names
    rename_with(~ "health_board", matches(c("NHS Board", "Healthboard"))) %>%
    rename_with(~ "month", matches("monthflag")) %>%
    rename_with(~ "location_code", 
                matches(c("HealthLocationCode", 
                          "Discharge Hospital Nat Code"))) %>%
    rename_with(~ "chi", contains("CHI")) %>%
    rename_with(~ "patient_postcode", contains(c("pc", "postcode"))) %>%
    rename_with(~ "local_authority", 
                contains(c("Local Authority", "LocalAuthority"))) %>%
    rename_with(~ "date_of_birth", contains(c("DOB", "Date of Birth"))) %>%
    rename_with(~ "specialty_code", contains("specialty")) %>%
    rename_with(~ "referral_date", contains(c("referral", "referred"))) %>%
    rename_with(~ "ready_for_discharge_date", 
                contains(c("ready", "medically"))) %>%
    rename_with(~ "delay_reason_2", 
                contains(c("secondary", "DD_Code_2"))) %>%
    rename_with(~ "delay_reason_1", 
                contains(c("reasonfordelay", "DD_Code_1"))) %>%
    rename_with(~ "out_of_area", 
                contains(c("outofarea", "out of area"))) %>%
    rename_with(~ "admission_date", contains("admission")) %>%
    rename_with(~ "sex", contains(c("sex", "gender"))) %>%
    rename_with(~ "discharge_date", 
                contains(c("delay end date", 
                           "discharge date", 
                           "datedischarge"))) %>%
    rename_with(~ "discharge_reason", 
                contains(c("dischargereason", "discharge to code"))) %>%
    
    clean_names() %>%
    
    # Trim white space from all character variables
    mutate(across(where(is.character), ~ str_trim(., side = "both"))) %>%
    
    # Remove leading zero from coded variables
    mutate(across(c(local_authority, delay_reason_1, delay_reason_2,
                    sex, discharge_reason),
                  ~ str_remove_all(., "^0*"))) %>%
    
    # Recode Aberdeen to Aberdeen City 
    # (DQ issue to be addressed in review of validation process)
    mutate(local_authority = case_when(
      local_authority == "Aberdeen" ~ "Aberdeen City",
      TRUE ~ local_authority
    )) %>%
    
    # Code all blanks as NA
    mutate(across(everything(), ~ na_if(., ""))) %>%
    
    # Ensure all reason for delay codes upper case
    mutate(across(contains("delay_reason"), toupper)) %>%
    
    # Format dates
    mutate(across(contains("date"), dmy)) %>%
    
    # Pad CHI Number
    mutate(chi = chi_pad(chi)) %>%
    
    # Format postcode
    mutate(patient_postcode = postcode(patient_postcode)) %>%
    
    # Reorder columns
    select(month, chi, patient_postcode, sex, date_of_birth, 
           health_board, local_authority, out_of_area, location_code, 
           specialty_code, delay_reason_1, delay_reason_2,
           admission_date, referral_date, ready_for_discharge_date,
           discharge_date, discharge_reason)
  
  # Display error if incorrect number or names of variables
  if(ncol(data) != 17){
    stop(paste("Incorrect number of variables in file. \n",
               "Actual number:", ncol(data), "\n",
               "Expected number: 17"))
  }
  
  exp_names <-
    c("month", "chi", "patient_postcode", "sex", "date_of_birth",
      "health_board", "local_authority", "out_of_area", "location_code", 
      "specialty_code", "delay_reason_1", "delay_reason_2", 
      "admission_date", "referral_date", "ready_for_discharge_date",
      "discharge_date", "discharge_reason")
  
  if(!all(names(data) %in% exp_names)){
    stop(paste("At least one incorrect variable name. \n",
               "Expected name(s):", setdiff(exp_names, names(data)), "\n",
               "Actual name(s):", setdiff(names(data), exp_names)))
  }
  
  data
  
}


### END OF SCRIPT ###