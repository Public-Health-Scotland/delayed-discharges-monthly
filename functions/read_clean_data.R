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
  
  data %>%
    
    # Rename some incorrectly named variables
    rename_all(~ str_replace(., "pc7", "PatientPostcode")) %>%
    rename_all(~ str_replace(., "idicator", "indicator")) %>%
    
    # Rename old naming convention to new
    rename_all(~ str_replace(., "NHS Board", "Healthboard")) %>%
    rename_all(~ str_replace(., "Monthflag", "MONTHFLAG")) %>%
    rename_all(~ str_replace(., "Discharge Hospital Nat Code", 
                                "HealthLocationCode")) %>%
    rename_all(~ str_replace(., "CHI Number", "CHINo")) %>%
    rename_all(~ str_replace(., "^Postcode$", "PatientPostcode")) %>%
    rename_all(~ str_replace(., "Local Authority Code", 
                                "LocalAuthorityArea")) %>%
    rename_all(~ str_replace(., "Date of Birth", "PatientDOB")) %>%
    rename_all(~ str_replace(., "Discharge Specialty Nat Code", 
                                "SpecialtyCode")) %>%
    rename_all(~ str_replace(., "Date Referred for SW Assessment", 
                                "DateReferralReceived")) %>%
    rename_all(~ str_replace(., "Date Declared Medically Fit", 
                                "Readyfordischargedate")) %>%
    rename_all(~ str_replace(., "DD_Code_1", "REASONFORDELAY")) %>%
    rename_all(~ str_replace(., "DD_Code_2", "REASONFORDELAYSECONDARY")) %>%
    rename_all(~ str_replace(., "Out of Area Case Indicator", 
                                "Outofareacaseindicator")) %>%
    rename_all(~ str_replace(., "Admission Date", "OriginalAdmissionDate")) %>%
    rename_all(~ str_replace(., "Sex Code", "Gender")) %>%
    rename_all(~ str_replace(., "Delay End Date", "DateDischarge")) %>%
    rename_all(~ str_replace(., "Discharge Date", "DateDischarge")) %>%
    rename_all(~ str_replace(., "Discharge To Code", "DischargeReason")) %>%
    
    clean_names() %>%
    
    # Code all blanks as NA
    mutate_all(na_if, "") %>%
    
    # Trim white space from all character variables
    mutate_if(is.character, ~ str_trim(., side = "both")) %>%
    
    # Format dates
    mutate_at(vars(contains("date")), dmy) %>%
    
    # Pad CHI Number
    mutate(chi_no = chi_pad(chi_no)) %>%
    
    # Format postcode
    mutate(patient_postcode = phsmethods::postcode(patient_postcode))
  
}