# Validation script for Monthly delayed discharges file creation


# bring in environment
source("00_setup_environment.R")

census_date <- ymd("2020/02/27")
month_start <- ymd("2020/02/01")
month_end <- ymd("2020/02/29")

Monthflag <- ("Feb 2020")
nhs_board <- ("lanark")

filepath <- ("/conf/delayed_discharges/RAP development/2020_02/Outputs/")
filepath2 <- paste0("/conf/delayed_discharges/Data files/Single Submissions (July 2016 onwards)/2020_02/Data/",nhs_board,"/")

### Get data file ( csv ) -----

datafile <- read.csv(paste0(filepath2, nhs_board, "_original.csv"))

#convert all spaces to #N/A
datafile %<>% mutate_all(na_if, "")

datafile <-
  datafile %>% clean_names()

#Rename variables

datafile <- datafile %>% 
  rename(nhs_board=healthboard,
         chi_number=chi_no,
         postcode=patient_postcode,
         local_authority_code=local_authority_area,
         date_of_birth=patient_dob,
         discharge_hosp_nat_code=health_location_code,
         discharge_specialty_nat_code=specialty_code,
         date_referred_for_sw_assessment=date_referral_received,
         date_declared_medically_fit=readyfordischargedate,
         dd_code_1=reasonfordelay,
         dd_code_2=reasonfordelaysecondary,
         out_of_area_case_indicator=outofareacaseindicator,
         admission_date=original_admission_date,
         sex_code=gender,
         discharge_date=date_discharge,
         discharge_to_code=discharge_reason)

#convert all dates to the same format
datafile %<>% mutate_at(vars(contains("date")), dmy)

# REVIEW FREQUENCY TABLES
table(datafile$nhs_board)
table(datafile$local_authority_code)
table(datafile$discharge_specialty_nat_code)
table(datafile$dd_code_1)
table(datafile$dd_code_2)
table(datafile$sex_code)


                                
# trim the chi_number to ensure no rogue spaces at beginning or end

# str_pad(datafile$chi_number, 10, pad = "0") # could use this or the following script

datafile <- datafile %>%
  mutate(chi_number = trimws(chi_number)) %>% 
  mutate(chi_number = as.character(chi_number)) %>%
  mutate(chi_number = ifelse(nchar(chi_number) == 9, paste0("0", chi_number), chi_number))
View(datafile$chi_number)
table(nchar(datafile$chi_number))
# Change numeric to a string character for chi_number

datafile %>%
  mutate(chi_number = as.character(chi_number))

### . Change discharge reason from code to text

table(datafile$discharge_to_code)

datafile <- datafile %>% mutate(
  discharge_to_code =
    if_else(discharge_to_code %in% c("1", "01"), "Placement",
      if_else(discharge_to_code %in% c("2", "02"), "Discharge Home with Home Care",
        if_else(discharge_to_code %in% c("3", "03"), "Discharge Home",
          if_else(discharge_to_code %in% c("4", "04"), "Death",
            if_else(discharge_to_code %in% c("5", "05"), "Not Fit For Discharge", NA_character_))))))
  
table(datafile$discharge_to_code)
count(datafile,discharge_to_code)
# Check that variable length of postcode is 7 or less
datafile <- datafile %>% mutate(
  postcode_chars =
    if_else(nchar(datafile$postcode) > 7, 1, 0))

table(datafile$postcode_chars) # no postcodes with more than 7 characters

# Fix formatting of postcode variable
datafile <- datafile %>%

  # First remove all spaces from postcode variable
  mutate(
    postcode = gsub("\\s", "", postcode),

    # Then add space (or spaces) at appropriate juncture (depending on
    # the number of characters) to get the postcode into 7-character
    # format
    postcode = case_when(
      is.na(postcode) ~ NA_character_,
      str_length(postcode) == 5 ~ sub("(.{2})", "\\1  ", postcode),
      str_length(postcode) == 6 ~ sub("(.{3})", "\\1 ", postcode),
      TRUE ~ postcode
    )
  )


# datafile$FirstDoM <- month_start  # FirstDoM becomes month_start
# datafile$LastDoM <- month_end # LastDoM becomes month_end

# Create dob2 in yyyymmdd format
datafile <- datafile %>%
  mutate(dob2 = as.numeric(paste0(
    str_sub(date_of_birth, 1, 4), str_sub(date_of_birth, 6, 7),
    str_sub(date_of_birth, 9, 10)
  )))

# Create ready_medical_discharge_date as a string
datafile <- datafile %>%
  mutate(ready_medical_discharge_date = as.numeric(paste0(
    str_sub(date_declared_medically_fit, 1, 4), str_sub(date_declared_medically_fit, 6, 7),
    str_sub(date_declared_medically_fit, 9, 10)
  )))


# Compute Age at RMD Date ( Ready for Medical Discharge Date )

datafile <- datafile %>%
  mutate(age_at_rdd = trunc((ready_medical_discharge_date - dob2) / 10000))

# Check there are no cases with a missing age_at_rdd
df_missingage_at_rdd<-datafile %>% 
  filter(is.na(age_at_rdd))
# select if age_at_rdd>=18

table(datafile$age_at_rdd)

# amend dates to same formats
  
# datafile$date_declared_medically_fit <- format(as.Date(datafile$date_declared_medically_fit, "%d/%m/%Y"), "%Y/%m/%d")
# 
# datafile$admission_date <- format(as.Date(datafile$admission_date, "%d/%m/%Y"), "%Y/%m/%d")
# 
# datafile$discharge_date <- format(as.Date(datafile$discharge_date, "%d/%m/%Y"), "%Y/%m/%d")
# 
# datafile$date_of_birth <- format(as.Date(datafile$date_of_birth, "%d/%m/%Y"), "%Y/%m/%d")
# month_start <- format(as.Date(month_start, "%d/%m/%Y"), "%Y/%m/%d")
# month_end <- format(as.Date(month_end, "%d/%m/%Y"), "%Y/%m/%d")




#convert all dates to the same format
# datafile %<>% mutate_at(vars(contains("date")), dmy)


# # amend dates to same formats
# datafile$date_declared_medically_fit <- format(as.Date(datafile$date_declared_medically_fit, "%d/%m/%Y"), "%Y/%m/%d")
# datafile$admission_date <- format(as.Date(datafile$admission_date, "%d/%m/%Y"), "%Y/%m/%d")
# datafile$discharge_date <- format(as.Date(datafile$discharge_date, "%d/%m/%Y"), "%Y/%m/%d")
# datafile$date_of_birth <- format(as.Date(datafile$date_of_birth, "%d/%m/%Y"), "%Y/%m/%d")
# month_start <- format(as.Date(month_start, "%d/%m/%Y"), "%Y/%m/%d")
# month_end <- format(as.Date(month_end, "%d/%m/%Y"), "%Y/%m/%d")


# Keep only hospital locations or N465R (in Grampian) # Removed as we need to see all entries
# datafile <- datafile %>%
#   filter(str_sub(discharge_hospital_nat_code, 5, 5) == "H" | discharge_hospital_nat_code == "N465R")


### Check if any RDD=DD ( select if RDD<>DD)  ----
datafile <- filter(datafile, is.na(discharge_date) | date_declared_medically_fit != discharge_date)

###1. test: ensure no records where RDD=LastDOM ----

datafile <- filter(datafile, date_declared_medically_fit != month_end)

# compute Age Groupings ( ensures no-one aged under 18 is selected)
datafile <- datafile %>% mutate(
  age_grp =
    if_else(age_at_rdd < 75, "18-74",
      if_else(age_at_rdd >= 75, "75+", " ")
    )
)

# Check age_grp
table(datafile$age_grp)


datafile <- datafile %>% mutate(
  reason_group_high_level =
    if_else(dd_code_1 == "100", "Code 100",
      if_else(dd_code_1 == "9", "Code 9",
        if_else(dd_code_1 %in% c("11A", "11B", "23C", "23D", "24A", "24B", "24C", "24D", "24E", "24F", "27A", "25A", "25D", "25E", "25F", "44"), "Health and Social Care Reasons",
          if_else(dd_code_1 %in% c("51", "52", "61", "67", "71", "72", "73", "74"), "Patient/Carer/Family-related reasons", "")
        )
      )
    )
)

table(datafile$reason_group_high_level) # check no outliers that haven't been coded

# High Level Reason Code Grouping
datafile <- datafile %>% mutate(
  reason_group =
    if_else(dd_code_1 %in% c("11A", "11B"), "H&SC - Community Care Assessment",
      if_else(dd_code_1 %in% c("23C", "23D"), "H&SC - Funding",
        if_else(dd_code_1 %in% c("24A", "24B", "24C", "24D", "24E", "24F", "27A") | dd_code_2 %in% c("24DX", "24EX"), "H&SC - Place Availability",
          if_else(dd_code_1 %in% c("25A", "25D", "25E", "25F") | dd_code_2 %in% c("25X"), "H&SC - Care Arrangements",
            if_else(dd_code_1 == "44", "H&SC-Transport",
              if_else(dd_code_1 %in% c("51", "52"), "Patient/Carer/Family-related reasons:Legal/Financial",
                if_else(dd_code_1 %in% c("61", "67"), "Patient/Carer/Family-related reasons:Disagreements",
                  if_else(dd_code_1 %in% c("71", "72", "73", "74"), "Patient/Carer/Family-related reasons:Other",
                    if_else(dd_code_2 %in% c("71X", "25X", "24EX", "24DX"), "Other Code 9 reasons",
                      if_else(dd_code_2 == "51X", "Adults with Incapacity Act", " ")
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
)

# Check output
table(datafile$reason_group) # cant check with tables yet as this is record total ( not just census)

# table(datafile_check$dd_code_1)
# datafile_check<-datafile_check %>% filter(dd_code_1!=100)
datafile <- datafile %>% mutate(
  DELAY_DESCRIPTION =
    if_else(dd_code_1 %in% c("11A", "11B"), "Awaiting commencement of post-hospital social care assessment(including transfer to another area team). Social Care includes home care and social work OT",
      if_else(dd_code_1 %in% c("23C"), "Non-availability of statutory funding to purchase Care Home place",
        if_else(dd_code_1 %in% c("23D"), "Non-availability of statutory funding to purchase any Other Care Home Package",
          if_else(dd_code_1 %in% c("24A"), "Awaiting place availablity in Local Authority Residential Home",
            if_else(dd_code_1 %in% c("24B"), "Awaiting place availablity in Independent Residential Home",
              if_else(dd_code_1 %in% c("24A"), "Awaiting place availablity in Local Authority Residential Home",
                if_else(dd_code_1 %in% c("24C"), "Awaiting place availability in Nursing Home",
                  if_else(dd_code_1 %in% c("24D"), "Awaiting place availability in Specialist Residential Facility for younger age groups(<65)",
                    if_else(dd_code_1 == "9" & dd_code_2 == "24DX", "Awaiting place availability in Specialist Facility for high level younger age groups (<65) where the Facility is not currently available and no interim option is appropriate",
                      if_else(dd_code_1 == "24E", "Awaiting place availability in Specialist Residential Facility for older age groups(65+)",
                        if_else(dd_code_1 == "9" & dd_code_2 == "24EX", "Awaiting place availability in Specialist Residential Facility for older age groups(65+) where the Facility is not currently available and an interim option is not appropriate",
                          if_else(dd_code_1 == "24F", "Awaiting place availability in care home (EMI/Dementia bed required)",
                            if_else(dd_code_1 == "9" & dd_code_2 == "26X", "Care Home/Facility Closed",
                              if_else(dd_code_1 == "27A", "Awaiting place availability in an intermediate Care facility",
                                if_else(dd_code_1 == "9" & dd_code_2 == "46X", "Ward Closed - patient well but cannot be discharged due to closure",
                                  if_else(dd_code_1 == "25A", "Awaiting completion of arrangements for Care Home Placement",
                                    if_else(dd_code_1 == "25D", "Awaiting completion of arrangements - in order to live in their own home - awaiting social support(non-availability of service",
                                      if_else(dd_code_1 == "25E", "Awaiting completion of arrangements - in order to live in their own home - awaiting procurement/delivery of equipment/adaptations fitted",
                                        if_else(dd_code_1 == "25F", "Awaiting completion of arraangements - Re-housing provision(including sheltered housing and homeless patients)",
                                          if_else(dd_code_1 == "9" & dd_code_2 == "25X", "Awaiting completion of complex care arrangements - in order to live within their own home",
                                            if_else(dd_code_1 == "51", "Legal issues (including intervention by patient's lawyer) - e.g. informed consent and / or adult protection issues",
                                              if_else(dd_code_1 == "9" & dd_code_2 == "51X", "Adults with Incapacity Act",
                                                if_else(dd_code_1 == "52", "Financial and personal assets problem - e.g. confirming financial assessment",
                                                  if_else(dd_code_1 == "61", "Internal family dispute issues (including dispute between patient and carer)",
                                                    if_else(dd_code_1 == "67", "Disagreement between patient/carer/family and health and social care",
                                                      if_else(dd_code_1 == "71", "Patient exercising statutory right of choice",
                                                        if_else(dd_code_1 == "9" & dd_code_2 == "71X", "Patient exercising statutory right of choice - interim placement is not possible or reasonable",
                                                          if_else(dd_code_1 == "72", "Patient does not qualify for care",
                                                            if_else(dd_code_1 == "73", "Family/relatives arranging care",
                                                              if_else(dd_code_1 == "74", "Other patient/carer/family-related reason",
                                                                if_else(dd_code_1 == "44", "Awaiting availability of transport",
                                                                  if_else(dd_code_1 == "100", "Reprovisioning/Recommissioning(see data definitions manual section 2.3)", " ")
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
)

table(datafile$DELAY_DESCRIPTION) # check

# Compute new variable census flag - ignoring code 26X and 46X.

# compute new variable census flag - ignoring 26X and 46X
"%!in%" <- function(x, y) !("%in%"(x, y))
"%notin%" <- Negate("%in%")
# datafile<-datafile %>% mutate(census_date=census_date)

datafile <- datafile %>% mutate(
  census_flag =
    if_else(is.na(discharge_date) & date_declared_medically_fit < census_date & dd_code_1 != "100" & dd_code_2 %!in% c("26X", "46X"), "Y",
      if_else(discharge_date >= census_date & date_declared_medically_fit < census_date & dd_code_1 != "100" & dd_code_2 %!in% c("26X", "46X"), "Y",
        if_else(discharge_date <= census_date & dd_code_1 != "100" & dd_code_2 %!in% c("26X", "46X"), "",
          if_else(discharge_date == date_declared_medically_fit & dd_code_1 != "100" & dd_code_2 %!in% c("26X", "46X"), "",
            if_else(discharge_date >= census_date & dd_code_1 != "100" & dd_code_2 %!in% c("26X", "46X"), "", "")
          )
        )
      )
    )
)

table(datafile$census_flag) # OK here 209 matches census output table publication
# Flag those discharged up to 3 working days after census

# Add in variable census_datePlus3WorkingDays

datafile <- datafile %>% mutate(census_datePlus3WorkingDays = census_date + 5)

# Flag those with a dischargedate le census_datePlus3WorkingDays

datafile <- datafile %>% mutate(
  discharge_within_3_days_census =
    if_else(census_flag == "Y" & discharge_date <= census_datePlus3WorkingDays & dd_code_1 != "100" & dd_code_2 %!in% c("26X", "46X"), "Y", "N")
)

table(datafile$discharge_within_3_days_census)
# change "Y" to a count for DischargeWithin3Days
datafile$discharge_within_3_days_census[datafile$discharge_within_3_days_census == "Y"] <- 1
datafile$discharge_within_3_days_census[datafile$discharge_within_3_days_census == ""] <- 0
datafile$discharge_within_3_days_census[is.na(datafile$discharge_within_3_days_census)] <- 0


datafile <- datafile %>% mutate(discharge_within_3_days_census = as.numeric(discharge_within_3_days_census)) # change variable to numeric
datafile$discharge_within_3_days_census[is.na(datafile$discharge_within_3_days_census)] <- 0
datafile_check <- datafile %>% filter(discharge_within_3_days_census == 1 & is.na(census_flag))


# Calculate Bed Days in Current Month



# convert dates to same format
#datafile$census_datePlus3WorkingDays <- format(as.Date(datafile$census_datePlus3WorkingDays, "%Y-%m-%d"), "%Y/%m/%d")


# calculate bed days in current month
# datafile<-datafile %>% mutate(Currentmonth_start=month_start)
# datafile<-datafile %>% mutate(Currentmonth_end=month_end)

# convert dates to same format
# datafile$Currentmonth_start<-format(as.Date(datafile$Currentmonth_start,"%Y-%m-%d"),"%Y/%m/%d")
# datafile$Currentmonth_end<-format(as.Date(datafile$Currentmonth_end,"%Y-%m-%d"),"%Y/%m/%d")

# test commit works



# Flag if date_declared_medically_fit in current month
datafile <- datafile %>% mutate(
  date_rmd_in_month =
    if_else(date_declared_medically_fit >= month_start & date_declared_medically_fit <= month_end, "Y", "N")
)

# Flag if dischargedate in current month
# added in the !is.na element to ensure any N/A discharge_date is counted too in totals
datafile <- datafile %>%
  mutate(discharge_date_in_month = if_else((discharge_date >= month_start & discharge_date <= month_end)
  & !is.na(discharge_date), "Y", ""))


table(datafile$date_rmd_in_month) # check that
## add in NA for Date Discharge to be month_end

datafile <- datafile %>% mutate(
  obds_in_month =
    if_else(date_rmd_in_month == "Y" & discharge_date_in_month == "Y", difftime(discharge_date, date_declared_medically_fit, units = "days"),
      if_else(date_rmd_in_month == "N" & discharge_date_in_month == "Y", difftime(discharge_date, month_start, units = "days") + 1,
        if_else(date_rmd_in_month == "Y" & discharge_date_in_month != "Y", difftime(month_end, date_declared_medically_fit, units = "days"),
          if_else(date_rmd_in_month == "N" & discharge_date_in_month != "Y", difftime(month_end, month_start, units = "days") + 1, 0)
        )
      )
    )
)

table(datafile$date_rmd_in_month) # matches syntax output
table(datafile$discharge_date_in_month) # matches syntax output
table(datafile$obds_in_month) # matches syntax output
table(datafile$discharge_hospital_nat_code) # Checking wh
datafile <- datafile %>% mutate(num_pats = 1)
table(datafile$num_pats) # 525
table(datafile$dd_code_1)
# Create Query Flags
datafile_check_census<-datafile %>% filter(datafile$census_flag=="Y")

# Create ready_medical_discharge_date as a string
datafile <- datafile %>%
  mutate(
    query_chi_dob =
      if_else(paste0(str_sub(date_of_birth, 9, 10), str_sub(date_of_birth, 6, 7), str_sub(date_of_birth, 3, 4)) != str_sub(chi_number, 1, 6), "Y", "N")
  )

table(datafile$query_chi_dob)


### Issues with checking postcode reference file ( one has spaces one doesn't)
# Read in P0stcode directory file
Postcodedirectory <- read_sav("/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2019_2.sav")

# Remove spaces from postcode within datafile

# stringr::str_remove_all(datafile$postcode," ")

# add flag to file showing that postcode is in the Postcodedirectory ( don't flag NK010AA postcodes as these are homeless )

datafile <- datafile %>% dplyr::mutate(flag_pc = if_else(postcode %in% Postcodedirectory$pc7 | postcode == "NK010AA", 1, 0))

# create new flag for any invalid postcodes
datafile <- datafile %>% mutate(
  query_postcode_invalid =
    if_else(flag_pc == 1, "N", "Y")
)


# raed in specialty file

Specialtyfile <- read_sav(paste0("/conf/linkage/output/lookups/Unicode/National Reference Files/specialt.sav"))

# check specialties are correct

datafile <- datafile %>% dplyr::mutate(flag_spec = if_else(discharge_specialty_nat_code %in% Specialtyfile$speccode, 1, 0))

datafile <- datafile %>% mutate(
  query_specialty_code_invalid =
    if_else(flag_spec == 0, "Y", "N")
)


# Check data discharged is in current month

datafile <- datafile %>% mutate(
  query_month =
    if_else(is.na(discharge_date) & discharge_date < month_start, "Y", "N")
)

# Check RDD in current month

datafile <- datafile %>% mutate(
  query_ready_for_medical_discharge =
    if_else(date_declared_medically_fit > month_end, "Y", "N")
)

# Check for Missing Discharge Reason

datafile <- datafile %>% mutate(
  query_missing_disch_reas =
    if_else(is.na(discharge_date) & !is.na(discharge_to_code), "Y", "N")
)

# Check for Missing Discharge Date

datafile <- datafile %>% mutate(
  query_missing_disch_date =
    if_else(is.na(discharge_date) & !is.na(discharge_to_code), "Y", "N"))

table(datafile$query_missing_disch_reas)
# Check for DischReasonInvalid

datafile <- datafile %>% mutate(
  query_disch_reas_invalid =
    if_else(!is.na(discharge_date) & discharge_to_code %!in% c(
      "Death", "Discharge Home", "Discharge Home with Home Care",
      "Not Fit For Discharge", "Placement", " "
    ), "Y", "N")
)

# Check for DiscontinuedCode

datafile <- datafile %>% mutate(
  query_discontinued_code =
    if_else(dd_code_1 %in% c("42", "62", "63"), "Y",
      if_else(dd_code_2 == "42X", "Y", "N")
    )
)


# Check for missing 11A date referral codes ( existing 11A codes with no date of referral)

datafile <- datafile %>% mutate(
  query_missing_date_ref_rec_for_11A =
    if_else(dd_code_1 == "11A" & is.na(date_referred_for_sw_assessment), "Y", "N")
)

table(datafile$date_referred_for_sw_assessment)
table(datafile$dd_code_1) # check that codes haven't changed
table(datafile$dd_code_2) # check that codes haven't changed
#table(datafile$query_missing_date_ref_rec_for_11A)
# Set Blank Codes to 11A

datafile <- datafile %>% mutate(
  dd_code_1 =
    if_else(is.na(dd_code_1) & is.na(dd_code_2), "11A", dd_code_1)
)
table(datafile$dd_code_1) # check that codes haven't changed
table(datafile$dd_code_2) # check that codes haven't changed
# Ensure Primary codes do not end in an 'X'

datafile <- datafile %>% mutate(
  query_code_1_ends_in_x =
    if_else(paste0(str_sub(dd_code_1, 3, 3)) == "X", "Y", "N")
)

# If Reas1<>Code 9, ensure Reas2=blank

datafile <- datafile %>% mutate(
  query_code_2_invalid =
    if_else(dd_code_1 != "9" & dd_code_2 != "N", "Y", "N")
)

# If Reas1=Code 9, Reas2 cannot be blank

datafile <- datafile %>% mutate(
  query_code_2_invalid =
    if_else(dd_code_1 == "9" & dd_code_2 != "N", "Y", "N")
)

# If Reas1=Code 9, Reas2 must end with 'X'

datafile <- datafile %>% mutate(
  query_code_2_invalid =
    if_else(dd_code_1 == "9" & str_ends(dd_code_2, "X") == "FALSE", "Y", "N")
)

table(datafile$dd_code_1)
table(datafile$dd_code_2)
# Flag Local Codes
# datafile<-datafile %>%  mutate(query_local_delay_code=
#                                  if_else(dd_code_1 %!in% c("11A","11B","23C","24A","24B","24C","24D","24E","24F","27A",
#                                                            "25A","25D","25E","25F","43","51","52","61","67","71","72","73",
#                                                            "74","44"," ","9","09","41","100") &
#                                      (!is.na(dd_code_2) &
#                                         (dd_code_2 %!in% c("11A","11B","23C","24A","24B","24C","24D","24E","24F","27A",
#                                                                          "25A","25D","25E","25F","43","51","52","61","67","71","72","73",
#                                                                          "74","44"," ","9","09","41","100","24DX", "24EX","25X","51X","71X",
#                                                                          "26X","46X"))),
#                                    "Y"," "))

datafile <- datafile %>% mutate(
  query_local_delay_code =
    if_else(dd_code_1 %!in% c(
      "11A", "11B", "23C", "24A", "24B", "24C", "24D", "24E", "24F", "27A",
      "25A", "25D", "25E", "25F", "43", "51", "52", "61", "67", "71", "72", "73",
      "74", "44", " ", "9", "09", "41", "100"
    ), "Y", "N")
)
table(datafile$query_local_delay_code)
datafile <- datafile %>% mutate(
  query_local_delay_code =
    if_else((!is.na(dd_code_2) &
      !(dd_code_2 %in% c(
        "11A", "11B", "23C", "24A", "24B", "24C", "24D", "24E", "24F", "27A",
        "25A", "25D", "25E", "25F", "43", "51", "52", "61", "67", "71", "72", "73",
        "74", "44", "", "9", "09", "41", "100", "24DX", "24EX", "25X", "51X", "71X",
        "26X", "46X"
      ))), "Y", "N")
)


table(datafile$query_local_delay_code)
table(datafile$dd_code_2)
# update query flags for blanks

datafile <- datafile %>% mutate(
  query_location =
    if_else(str_ends(discharge_hosp_nat_code, "H") == "FALSE", "Y", "N")
)
table(datafile$query_location)
table(datafile$discharge_hosp_nat_code)

datafile <- datafile %>% mutate(
  query_chi =
    if_else(is.na(chi_number), "Y", "N")
)

datafile <- datafile %>% mutate(
  query_local_authority_code =
    if_else(local_authority_code %in% c("Missing", " "), "Y", "N")
)

datafile <- datafile %>% mutate(
  query_postcode =
    if_else(postcode == "N", "Y", "N")
)

datafile <- datafile %>% mutate(
  query_DOB =
    if_else(is.na(date_of_birth), "Y", "N")
)
table(datafile$query_DOB)

datafile <- datafile %>% mutate(
  query_sex_code =
    if_else(is.na(sex_code), "Y", "N")
)


datafile <- datafile %>% mutate(
  query_specialty_code =
    if_else(is.na(discharge_specialty_nat_code), "Y", "N")
)
table(datafile$discharge_specialty_nat_code)

datafile <- datafile %>% mutate(
  query_ready_for_medical_discharge =
    if_else(is.na(date_declared_medically_fit), "Y", "N")
)

datafile <- datafile %>% mutate(
  query_dd_code_1 =
    if_else(is.na(dd_code_1), "Y", "N")
)

datafile <- datafile %>% mutate(
  query_admission_date =
    if_else(is.na(admission_date), "Y", "N")
)



# Query flags for date errors

datafile <- datafile %>% mutate(
  query_ready_for_medical_discharge_lt_adm_date =
    if_else(date_declared_medically_fit < admission_date, "Y", "N")
)
table(datafile$query_ready_for_medical_discharge_lt_adm_date)

datafile <- datafile %>% mutate(
  query_discharge_date_lt_rdd =
    if_else(discharge_date < date_declared_medically_fit, "Y", "N")
)
table(datafile$query_discharge_date_lt_rdd)

datafile <- datafile %>% mutate(
  query_disch_date_lt_adm_date =
    if_else(discharge_date < admission_date, "Y", "N")
)
table(datafile$query_disch_date_lt_adm_date)

# OBDle0 error variable

datafile <- datafile %>% mutate(
  query_obds_ltequal_to_zero =
    if_else(census_flag == "Y" & obds_in_month <= 0, "Y", "N")
)
table(datafile$census_flag)
table(datafile$obds_in_month)

# query Hospital Code if not H in position 5

datafile <- datafile %>% mutate(
  query_hospital_code =
    if_else(substr(datafile$discharge_hosp_nat_code, 5, 5) != "H", "Y", "N")
)

table(datafile$query_hospital_code)

# check for duplicate chi_number and flag

datafile <- datafile %>%
  tidylog::group_by(chi_number) %>%
  tidylog::mutate(
    # duplicate_chi = dplyr::row_number(),
    duplicate_chi = if_else(max(dplyr::row_number()) > 1, "Y", "N")
  ) %>%
  dplyr::ungroup()

table(datafile$duplicate_chi)

# matches output from spss ( 17 duplicate chi records, 16 with two and one with 3)

# check for duplicate census records and flag
datafile <- arrange(datafile, chi_number, desc(census_flag))


# check for duplicate CENSUS RECORDS and flag

datafile <- datafile %>%
  tidylog::group_by(chi_number, census_flag) %>%
  tidylog::mutate(query_duplicate_chi_census = max(row_number()) > 1 & census_flag == "Y") %>%
  # query_duplicate_chi_census == if_else(max(row_number())>1 & census_flag="Y"),"Y"," ") %>%
  dplyr::ungroup()

table(datafile$query_duplicate_chi_census)


# need to capture paired records with errors to investigate reason for duplicate - so update Error code for both.
datafile <- arrange(datafile, chi_number, desc(query_duplicate_chi_census))

datafile <- datafile %>%
  tidylog::group_by(chi_number, query_duplicate_chi_census) %>%
  tidylog::mutate(query_duplicate_chi_census = max(row_number()) > 1 & census_flag == "Y") %>%
  # query_duplicate_chi_census == if_else(max(row_number())>1 & census_flag="Y"),"Y"," ") %>%
  dplyr::ungroup()

table(datafile$query_duplicate_chi_census)
# Where CHI number and Admission Date match - check same RDD date on multiple records?
datafile <- arrange(datafile, chi_number, admission_date, date_declared_medically_fit, discharge_date)

datafile <- datafile %>%
  tidylog::group_by(chi_number, admission_date, date_declared_medically_fit) %>%
  tidylog::mutate(query_overlapping_dates = if_else(max(row_number()) > 1, "Y", "N")) %>%
  dplyr::ungroup()

table(datafile$query_overlapping_dates)

# Where CHI number and Admission Date match - check same Death date on multiple records?
datafile <- arrange(datafile, chi_number, admission_date, date_declared_medically_fit, discharge_date)

datafile <- datafile %>%
  tidylog::group_by(chi_number, admission_date, date_declared_medically_fit) %>%
  tidylog::mutate(query_overlapping_dates = if_else(chi_number == lag(chi_number) & admission_date == lag(admission_date) &
    date_declared_medically_fit == lag(date_declared_medically_fit) & discharge_date != lag(discharge_date) & discharge_to_code == lag(discharge_to_code), "Y", "N")) %>%
  dplyr::ungroup()



# RDD before Discharge Date on previous record.
# if CHNO=lagchi_number and admission_date=lagadmission_date and date_declared_medically_fit<lagdate_declared_medically_fit and missing lagdischarge_date
datafile <- datafile %>%
  tidylog::group_by(chi_number, admission_date, date_declared_medically_fit) %>%
  tidylog::mutate(query_overlapping_dates = if_else(max(row_number()) > 1 & date_declared_medically_fit < lag(date_declared_medically_fit), "Y", "N")) %>%
  dplyr::ungroup()

table(datafile$query_overlapping_dates)

# Different RDDs but previous record missing a discharge_date.
datafile <- datafile %>%
  tidylog::group_by(chi_number, admission_date, date_declared_medically_fit) %>%
  tidylog::mutate(query_overlapping_dates = if_else(chi_number == lag(chi_number) & admission_date == lag(admission_date) &
    date_declared_medically_fit == lag(date_declared_medically_fit) & discharge_date != lag(discharge_date), "Y", "N")) %>%
  dplyr::ungroup()

table(datafile$query_overlapping_dates)

# Different admission and ready for discharge dates where RDD on second record before RDD on first record
datafile <- datafile %>%
  tidylog::group_by(chi_number, discharge_date, date_declared_medically_fit) %>%
  tidylog::mutate(query_overlapping_dates = if_else(chi_number == lag(chi_number) & date_declared_medically_fit < lag(date_declared_medically_fit)
  | discharge_date < lag(date_declared_medically_fit) | date_declared_medically_fit < lag(discharge_date), "Y", "N")) %>%
  dplyr::ungroup()

table(datafile$query_overlapping_dates)

# Need to capture paired records with errors to investigate reason for duplicate - so update Error code for both.

datafile <- arrange(datafile, chi_number, desc(query_overlapping_dates))


datafile <- datafile %>%
  tidylog::group_by(chi_number) %>%
  tidylog::mutate(query_overlapping_dates = if_else(chi_number == lag(chi_number), lag(query_overlapping_dates), " ")) %>%
  dplyr::ungroup()

table(datafile$query_overlapping_dates)

# sort cases by chi_number admission_date date_declared_medically_fit discharge_date

datafile <- arrange(datafile, chi_number, admission_date, date_declared_medically_fit, discharge_date)

# compute num_pats=1
datafile$num_pats == 1

# table(datafile$num_pats)

write_sav(datafile, paste0(filepath, nhs_board,"_temp.sav"))
write.xlsx(datafile, paste0(filepath, nhs_board,"_temp.xlsx"))



# Create Error file with row per CHI per ERROR

table(datafile$census_flag)

#Varstocases

datafile<-datafile %>% mutate(query_missing_date_ref_rec_for_11A=
                                if_else(dd_code_1=="11A" & is.na(date_referred_for_sw_assessment),"Y"," "))

datafile <- datafile %>% mutate(
  query =
    if_else(query_month == "Y" | query_location == "Y" | query_chi == "Y" | query_local_authority_code == "Y"
    | query_postcode == "Y" | query_postcode_invalid == "Y" | query_sex_code == "Y" | query_specialty_code == "Y"
    | query_specialty_code_invalid == "Y" | query_ready_for_medical_discharge == "Y" | query_dd_code_1 == "Y" | query_admission_date == "Y" | query_chi_dob == "Y"
    | query_local_delay_code == "Y" | query_discontinued_code == "Y" | query_code_1_ends_in_x == "Y" | query_code_2_invalid == "Y"
    | query_ready_for_medical_discharge_lt_adm_date == "Y" | query_discharge_date_lt_rdd == "Y" | query_disch_date_lt_adm_date == "Y" | query_missing_disch_reas == "Y" | query_missing_disch_date == "Y"
    | query_disch_reas_invalid == "Y" | query_hospital_code == "Y" | query_overlapping_dates == "Y" | query_duplicate_chi_census == "Y" | query_obds_ltequal_to_zero == "Y"
    | query_missing_date_ref_rec_for_11A == "Y", "Y", "N"))

# Checks on query file
table(datafile$query) #
table(datafile$query_month) # None
table(datafile$query_location) # None
table(datafile$query_chi) # None
table(datafile$query_local_authority_code) # None
table(datafile$query_postcode) # None
table(datafile$query_postcode_invalid) # None
table(datafile$query_sex_code) # None
table(datafile$query_specialty_code) # None
table(datafile$query_specialty_code_invalid) # None
table(datafile$query_ready_for_medical_discharge) # None
table(datafile$query_dd_code_1) # None
table(datafile$query_admission_date) # None
table(datafile$query_chi_dob) # None
table(datafile$query_local_delay_code) # None
table(datafile$query_discontinued_code) # None
table(datafile$query_code_1_ends_in_x) # None
table(datafile$query_code_2_invalid)
table(datafile$query_ready_for_medical_discharge_lt_adm_date) # None
table(datafile$query_discharge_date_lt_rdd) # None
table(datafile$query_disch_date_lt_adm_date) # None
table(datafile$query_disch_reas_invalid) # None
table(datafile$query_hospital_code)
table(datafile$query_overlapping_dates) # None
table(datafile$query_duplicate_chi_census) # 0 Check
table(datafile$query_obds_ltequal_to_zero) # None
table(datafile$query_missing_date_ref_rec_for_11A) # None
table(datafile$query_missing_disch_date) # None
table(datafile$query_missing_disch_reas) # None
table(datafile$query_hospital_code) # to be checked

# select if query is showing  - May not need this as it selects out all rows
query_list <- datafile %>% filter(datafile$query == "Y")

# Check missing discharge dates, discharge to code and query_missing_disch_date

dplyr::count(datafile,is.na(discharge_date), discharge_to_code, query_missing_disch_date)

# Note: No query_HospInBoard, query_DoB or query_Reas2noCode9 queries generated previously so ignored in next command.

query_list2 <- subset(query_list, select = c(
  monthflag, nhs_board, discharge_hosp_nat_code, local_authority_code,
  postcode, duplicate_chi, chi_number, date_of_birth, discharge_specialty_nat_code, census_flag,
  admission_date, date_referred_for_sw_assessment, date_declared_medically_fit, discharge_date,
  discharge_to_code, dd_code_1, dd_code_2, out_of_area_case_indicator,
  query_month, query_location, query_chi, query_local_authority_code, query_postcode,
  query_postcode_invalid, query_sex_code, query_specialty_code, query_specialty_code_invalid,
  query_ready_for_medical_discharge, query_dd_code_1, query_admission_date, query_chi_dob, query_local_delay_code,
  query_discontinued_code, query_code_1_ends_in_x, query_code_2_invalid,
  query_ready_for_medical_discharge_lt_adm_date, query_discharge_date_lt_rdd, query_disch_date_lt_adm_date, query_missing_disch_reas,
  query_missing_disch_date, query_disch_reas_invalid, query_overlapping_dates, query_duplicate_chi_census,
  query_obds_ltequal_to_zero, query_missing_date_ref_rec_for_11A, num_pats
))

write.sav(query_list2, paste0(filepath, nhs_board, "_Query_List.sav"))
write.xlsx(query_list2, paste0(filepath, nhs_board, "_Query_List.xlsx"))
# isn't saving out a blank file as no errors!
# If no Query_List.xlsx shows up this means there are no queries.


# recode query_month TO query_missing_date_ref_rec_for_11A where 'Y' becomes 1.
# need to get number of column for query_variables( columns 36 to 65 )

start_col<-grep("query_chi_dob", colnames(datafile))
end_col<-grep("query_overlapping_dates", colnames(datafile))

# recoding these queries with a 1 where there is a "Y" to enable aggregation later
datafile[, start_col:end_col][datafile[, start_col:end_col] == "Y"] <- as.numeric(1)

datafile <- datafile %>% mutate(query_dd_code_1 = as.numeric(query_dd_code_1))
datafile <- datafile %>% mutate(query_admission_date = as.numeric(query_admission_date))


# above includes alter type query_month TO query_missing_date_ref_rec_for_11A ( string becomes a numeric)
class(datafile$query_dd_code_1)
typeof(datafile$query_dd_code_1)

# aggregate file
table(datafile$query_dd_code_1)
datafile2 <- datafile %>%
  group_by(nhs_board, monthflag) %>%
  summarise(
    query_month = sum(as.numeric(query_month)),
    query_location = sum(as.numeric(query_location)),
    query_chi = sum(as.numeric(query_chi)),
    query_local_authority_code = sum(as.numeric(query_local_authority_code)),
    query_postcode = sum(as.numeric(query_postcode)),
    query_postcode_invalid = sum(as.numeric(query_postcode_invalid)),
    query_sex_code = sum(as.numeric(query_sex_code)),
    query_specialty_code = sum(as.numeric(query_specialty_code)),
    query_specialty_code_invalid = sum(as.numeric(query_specialty_code_invalid)),
    query_ready_for_medical_discharge = sum(as.numeric(query_ready_for_medical_discharge)),
    query_dd_code_1 = sum(as.numeric(query_dd_code_1)),
    query_admission_date = sum(as.numeric(query_admission_date)),
    query_chi_dob = sum(as.numeric(query_chi_dob)),
    query_local_delay_code = sum(as.numeric(query_local_delay_code)),
    query_discontinued_code = sum(as.numeric(query_discontinued_code)),
    query_code_1_ends_in_x = sum(as.numeric(query_code_1_ends_in_x)),
    query_code_2_invalid = sum(as.numeric(query_code_2_invalid)),
    query_ready_for_medical_discharge_lt_adm_date = sum(as.numeric(query_ready_for_medical_discharge_lt_adm_date)),
    query_discharge_date_lt_rdd = sum(as.numeric(query_discharge_date_lt_rdd)),
    query_disch_date_lt_adm_date = sum(as.numeric(query_disch_date_lt_adm_date)),
    query_missing_disch_reas = sum(as.numeric(query_missing_disch_reas)),
    query_missing_disch_date = sum(as.numeric(query_missing_disch_date)),
    query_disch_reas_invalid = sum(as.numeric(query_disch_reas_invalid)),
    query_overlapping_dates = sum(as.numeric(query_overlapping_dates)),
    query_hospital_code = sum(as.numeric(query_hospital_code)),
    query_duplicate_chi_census = sum(as.numeric(query_duplicate_chi_census)),
    query_obds_ltequal_to_zero = sum(as.numeric(query_obds_ltequal_to_zero)),
    query_missing_date_ref_rec_for_11A = sum(as.numeric(query_missing_date_ref_rec_for_11A))
  ) %>%
  ungroup()





# Restructure file

datafile2 %<>%
  pivot_longer(
    cols = starts_with("query"),
    names_to = "Query_Type",
    values_to = "Total"
  )



# Provisional Census / OBD figures

datafile <- read_sav(paste0(filepath, nhs_board,"_temp.sav"))

# Create a provsional HB census total - excl. Code 100.

datafile3 <- datafile %>% filter(dd_code_1 != "100" & (census_flag == "Y" | discharge_within_3_days_census == 1))

Census_hb <- datafile3 %>%
  group_by(nhs_board, discharge_within_3_days_census, reason_group_high_level) %>%
  summarise(census = n()) %>%
  ungroup()

# Create a Provisional HB/LA census total - excl Code 100.

Census_la <- datafile3 %>%
  group_by(nhs_board, local_authority_code, discharge_within_3_days_census, reason_group_high_level) %>%
  summarise(census = n()) %>%
  ungroup()


# Create a provisional HB OBD total - excl. Code 100.
datafile$obds_in_month[is.na(datafile$obds_in_month)] <- 0 # Need to change NA to 0 before aggregate


OBDs_HB <- datafile %>%
  filter(dd_code_1 != "100") %>%
  group_by(nhs_board, reason_group_high_level) %>%
  summarise(obds_in_month = sum(obds_in_month)) %>%
  ungroup()

# Create a provisional HB OBD total - excl. code 100.

OBDs_LA <- datafile %>%
  filter(dd_code_1 != "100") %>%
  group_by(nhs_board, local_authority_code, reason_group_high_level) %>%
  summarise(obds_in_month = sum(obds_in_month)) %>%
  ungroup()


# Combine provisional census / OBD files.

Prov <- bind_rows(Census_la, OBDs_HB, OBDs_LA)


Prov$reason_group_high_level[Prov$reason_group_high_level != "Code 9"] <- "HSC/PCF"

# Rename variable

Prov <- Prov %>%
  rename(DelayCategory = reason_group_high_level)


# Need to amend the NA in discharge_within_3_days_census and census to ensure it works in line 751 - 752.
Prov$discharge_within_3_days_census[is.na(Prov$discharge_within_3_days_census)] <- 0
Prov$census[is.na(Prov$census)] <- 0

Prov <- Prov %>% mutate(
  discharge_within_3_days_census =
    if_else(discharge_within_3_days_census == 1, census, discharge_within_3_days_census)
)


# Now set census value to 0 if there is a value in discharge_within_3_days_census column.

Prov <- Prov %>% mutate(
  census =
    if_else(discharge_within_3_days_census > 0, 0, census)
)
# rename census as census_total
Prov <- Prov %>%
  rename(census_total = census)

# aggregate

ProvCensusOBD <- Prov %>%
  group_by(nhs_board, local_authority_code, DelayCategory) %>%
  summarise(
    discharge_within_3_days_census = sum(discharge_within_3_days_census),
    census_total = sum(census_total),
    obds_in_month = sum(obds_in_month, na.rm = TRUE)
  ) %>%
  ungroup()
