#########################################################################
# Name of file - 04_census_and_bed_days.R
# Data release - Monthly Delayed Discharges publication
# Original Authors - Peter McClurg
# Orginal Date - August 2020
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Updates trendfile with DD records from most recent 
#               publication monthflag.
#########################################################################


### 0 - Load setup environment and functions ----

# Setup environment
source(here::here("code", "00_setup_environment.R"))

# Functions
walk(list.files(here("functions"), full.names = TRUE), source)

 

### 1 - Import Scotland_validated file ----

datafile <- readRDS(here::here("data", format(start_month, "%Y-%m"), 
                               paste0(format(start_month, "%Y-%m"), 
                                      "_scotland.rds")))

# Tidy data
datafile %<>% filter(reas1 != "Code 100") %>%   # Filter 'Code 100'
  mutate(areaname = " ")                  # Add variable 'areaname'


# Create census data file
datafile2 <- filter(datafile, !reasonfordelaysecondary %in% 
                      c("26X", "46X") & census_flag == 1)


# 1b. Create Reason 2 Groupings ----

##Sub grouping reason_grp for code 9s are currently being classed as H&SC or Pat/Fam reasons. Ensure they are just code 9s.
##Also bring in Transport as a separate category.

datafile3<-datafile2 %>% 
  mutate(reas2 = as.character(reas2))%>% 
  mutate(reas2=
           if_else(delay_description=="Adults with Incapacity Act", "Adults with Incapacity Act",reas2))

datafile3<-datafile2 %>% 
  mutate(reas2 = as.character(reas2))%>% 
  mutate(reas2=
           if_else(delay_description!="Adults with Incapacity Act" & reas1=="Code 9", "Other code 9 reasons(not AWI)",reas2))

datafile3<-datafile2 %>% 
  mutate(reas2 = as.character(reas2))%>% 
  mutate(reas2=
           if_else(delay_description=="Awaiting availability of transport","Transport",reas2))




### 2 - Get reason / age_grp breakdown for Scotland (Level = 2) ----

datafile2 %<>% mutate(level = "1", areaname = "Scotland")

Scotlandbymainreasongrouping <- datafile2 %>%
  group_by(fin_yr, monthflag, level, areaname, age_grouping, reas1) %>%
  summarise_at(vars(no_of_patients, delay1to3days:delay_over6wks, acute:notgpled),
  ~ sum(., na.rm = TRUE)) %>%
  ungroup()



Scotlandbysubreasongrouping <- datafile2 %>%
  group_by(fin_yr, monthflag, level, areaname, age_grouping, reas2) %>%
  summarise_at(vars(no_of_patients, delay1to3days:delay_over6wks, acute:notgpled),
               ~ sum(., na.rm = TRUE)) %>%
  ungroup()




### 3 - Breakdown for HBs (Level = 2) ----
datafile2 %<>% mutate(level = "2", areaname = healthboard)

HBbymainreasongrouping <- datafile2 %>%
  group_by(fin_yr, monthflag, level, areaname, age_grouping, reas1) %>%
  summarise_at(vars(no_of_patients, delay1to3days:delay_over6wks, acute:notgpled),
               ~ sum(., na.rm = TRUE)) %>%
  ungroup()




HBbysubreasongrouping <- datafile2 %>%
  group_by(fin_yr, monthflag, level, areaname, age_grouping, reas2) %>%
  summarise_at(vars(no_of_patients, delay1to3days:delay_over6wks, acute:notgpled),
               ~ sum(., na.rm = TRUE)) %>%
  ungroup()



### 4 - Breakdown for LA (Level=3) ----
datafile2 %<>% mutate(level = "3", areaname = local_authority_area)


labymainreasongrouping <- datafile %>%
  group_by(fin_yr, monthflag, level, areaname, age_grouping, reas1) %>%
  summarise_at(vars(no_of_patients, delay1to3days:delay_over6wks, acute:notgpled),
               ~ sum(., na.rm = TRUE)) %>%
  ungroup()


labysubreasongrouping <- datafile %>%
  group_by(fin_yr, monthflag, level, areaname, age_grouping, reas2) %>%
  summarise_at(vars(no_of_patients, delay1to3days:delay_over6wks, acute:notgpled),
               ~ sum(., na.rm = TRUE)) %>%
  ungroup()



### 5 - Combine summary dataframes ----

Scot_HB_la <- bind_rows(Scotlandbymainreasongrouping, Scotlandbysubreasongrouping, HBbymainreasongrouping, HBbysubreasongrouping, labymainreasongrouping, labysubreasongrouping)


Scot_HB_la %>% 
  mutate(reas1 = as.character(reas1)) %>% 
  mutate(reas1 = if_else(is.na(reas1), reas2, reas1))


Scot_HB_la %<>% mutate(age_grp = "All")

ScotHBlaallage_grps<- Scot_HB_la %>%
  group_by(fin_yr, monthflag, level, areaname, age_grouping, reas1) %>%
  summarise_at(vars(no_of_patients, delay1to3days:delay_over6wks, acute:notgpled),
               ~ sum(., na.rm = TRUE)) %>%
  ungroup()


ScotHBlaallreasonexcHSCPatFamtotal <- bind_rows(Scot_HB_la, ScotHBlaallage_grps)



###6 - Calculate the total number of delays excluding where code = 9 ----
Scot_HB_la %<>% 
  filter(reas1 %in% c("Health and Social Care Reasons", "Patient/Carer/Family-related reasons")) %>%
  mutate(reas1 = "All Delays excl. Code 9")


ScotHBlaallreasonsincHSCPatFamtotal<- Scot_HB_la %>%
  group_by(fin_yr, monthflag, level, areaname, age_grouping, reas1) %>%
  summarise_at(vars(no_of_patients, delay1to3days:delay_over6wks, acute:notgpled),
               ~ sum(., na.rm = TRUE)) %>%
  ungroup()

###7 - Calculate the number of delays for all reasons ----

Scot_HB_la %<>% filter(ScotHBlaallreasonexcHSCPatFamtotal, reas1 %in% c("Health and Social Care Reasons", "Code 9", "Patient/Carer/Family-related reasons"))

Scot_HB_la %>% mutate(reas1 = "All")

ScotHBlaallreasonsalldelaystotal<- Scot_HB_la %>%
  group_by(year, MONTHFLAG, level, areaname, age_grp, reas1) %>%
  summarise_at(vars(num_pats, delay_1_to_3_days:delay_over_6_weeks, acute:notgpled),
               ~ sum(., na.rm = TRUE)) %>%
  ungroup()

#Combine Scottish Health Boards All Reasons data frames


Tabs136 <- bind_rows(ScotHBlaallreasonsalldelaystotal, ScotHBlaallreasonsincHSCPatFamtotal, ScotHBlaallreasonexcHSCPatFamtotal) %>%
              arrange(areaname, age_grp, reas1)




###8 - Detailed reason breakdown ----
#Get main census file (datafile2)
datafile %>% mutate(level = "1", areaname = "Scotland", age_grp = "All")
#copy secondary Code 9 codes into delay reason - e.g. delay reason 9 becomes 51X etc..
#then update reas1 with individual delay codes in delayreason

#datafile5 %<>% mutate(REASONFORDELAY=
#                                 if_else(REASONFORDELAY==9,dd_code_2,REASONFORDELAY))

Scotlandindreasons<- datafile %>% 
  group_by(year, MONTHFLAG, level, areaname, age_grp, reason_grp_high_level) %>%
  summarise_at(vars(num_pats, delay_1_to_3_days:delay_over_6_weeks, acute:notgpled),
               ~ sum(., na.rm = TRUE)) %>%
  ungroup()



datafile6<- bind_rows(Tabs136, Scotlandindreasons)




### 9 - Save out census file ----

write.xlsx(datafile6, here::here("outputs", "All census data.xlsx"))



### 10. Bed days data ----


datafile %>% mutate(Healthboard = substring(Healthboard, 5)) # change nhs_board and remove the 'NHS_'

hbbeddaysdiffage_grp<- datafile %>% 
  group_by(Healthboard,age_grp,reas1) %>% 
  summarise(OBDs=sum(OBDs,na.rm=TRUE)) %>% 
  ungroup()



labeddaysdiffage_grp<- datafile %>% 
  group_by(LocalAuthorityArea,age_grp,reas1) %>% 
  summarise(OBDs=sum(OBDs,na.rm=TRUE)) %>% 
  ungroup()



datafile %<>% mutate(Healthboard="Scotland")

Scotbeddaysdiffage_grp<- datafile %>% 
  group_by(Healthboard,age_grp,reas1) %>% 
  summarise(OBDs=sum(OBDs,na.rm=TRUE)) %>% 
  ungroup()


# add files

Scotlandhbbeddays <- bind_rows(Scotbeddaysdiffage_grp, hbbeddaysdiffage_grp)

# Get Scotland and HB bed days for all ages

Scotlandhbbeddays %<>% mutate(age_grp="All") %>% 
  group_by(Healthboard,age_grp,reas1) %>% 
  summarise(OBDs=sum(OBDs,na.rm=TRUE)) %>% 
  ungroup()

#add files

Scotlandhbbeddaysallagesallreasons <- bind_rows(Scotlandhbbeddays, Scotbeddaysdiffage_grp, hbbeddaysdiffage_grp)
#write_sav(Scotandhbbeddaysallagesallreasons,paste0(filepath,"Scot and hb bed days_R.sav")) # save out file

datafile15<-Scotlandhbbeddaysallagesallreasons %>% mutate(reas1="All")

ScotlandhbAllage_grpsbeddays<- datafile15 %>% 
  group_by(Healthboard,age_grp,reas1) %>% 
  summarise(OBDs=sum(OBDs,na.rm=TRUE)) %>% 
  ungroup()


Scotlandhbbeddaysallagegrpsallreasons <- bind_rows(ScotlandhbAllage_grpsbeddays, Scotlandhbbeddaysallagesallreasons)



### 11 - LA Bed days ----
#use la bed days diff age file 
datafile17<-labeddaysdiffage_grp %>% mutate(age_grp="All")


labeddaysallage_grps<- datafile17 %>% 
  group_by(LocalAuthorityArea,age_grp,reas1) %>% 
  summarise(OBDs=sum(OBDs,na.rm=TRUE)) %>% 
  ungroup()

datafile18 <- bind_rows(labeddaysallage_grps, labeddaysdiffage_grp)


datafile19<-datafile18 %>% mutate(reas1="All")

labeddaysallreason_grp_high_level<- datafile19 %>% 
  group_by(LocalAuthorityArea,age_grp,reas1) %>% 
  summarise(OBDs=sum(OBDs,na.rm=TRUE)) %>% 
  ungroup()

datafile20 <- bind_rows(labeddaysallreason_grp_high_level, datafile18)


#### 12 - HB Bed days ----
#match in to hb data sheet template
hbbedstemplate<-read.csv(paste0(filepath2,"hb_template.csv"))

hbbedstemplate <- hbbedstemplate %>% 
  rename(Healthboard=hbname,
         age_grp = age)


hbbedstemplate<-hbbedstemplate%>% mutate(Healthboard=toupper(Healthboard),age_grp=toupper(age_grp),reas1=toupper(reas1))


#amend variables as necessary in order for matching to work

datafile21 <- left_join(Scotlandhbbeddaysallagegrpsallreasons,hbbedstemplate,
                        by = c(("Healthboard" = "Healthboard"), ("age_grp"="age_grp"),("reas1"="reas1")))

arrange(datafile21,Healthboard,age_grp,reas1) # arrange data in order for tables

datafile21 %<>% mutate(Healthboard = toupper(Healthboard), age_grp = toupper(age_grp), reas1 = toupper(reas1))


#Rename reason_grp_high_level as Standard where there isn't a Code 9
datafile22<-datafile21%>% mutate(reas1=
                                   if_else(reas1%in%c("HEALTH AND SOCIAL CARE REASONS","PATIENT/CARER/FAMILY-RELATED REASONS"),"STANDARD",reas1))


#select standard only.
datafile23<-filter(datafile22,reas1=="STANDARD")
# aggregate 

datafile24<- datafile23 %>% 
  group_by(Healthboard,age_grp,reas1) %>% 
  summarise(OBDs=sum(OBDs,na.rm=TRUE)) %>% 
  ungroup()

datafile24<-datafile24%>%rename(obds3=OBDs)

#match files
datafile25 <- left_join(datafile24,datafile21,
                        by = c(("Healthboard" = "Healthboard"), ("age_grp"="age_grp"),("reas1"="reas1")))

datafile25a<-bind_rows(datafile24,datafile21)


#replace obds in correct column for all reason groups
datafile26<-datafile25a %>% mutate(OBDs=
                                     if_else(!is.na(obds3),obds3,OBDs))

#remove surplus columns
datafile26<-select(datafile26,-obds3,-obds2)

#match back to template to ensure every row is populated

datafile27 <- left_join(hbbedstemplate,datafile26,
                        by = c(("Healthboard" = "Healthboard"), ("age_grp"="age_grp"),("reas1"="reas1")))

#replace obds in correct column for all reason groups
datafile27<-datafile27 %>% mutate(OBDs=
                                    if_else(is.na(OBDs),0L,OBDs))

#remove surplus columns
datafile27<-select(datafile27,-obds2)

datafile27<-arrange(datafile27,datafile27$Healthboard,datafile27$age_grp,datafile27$reas1)


### 13 - Save out bed days HB file ----

write.xlsx(datafile27, here::here("outputs", "HB bed days data sheet_R.xlsx"))


### 14. LA bed days  ----

labedstemplate<-read.csv(paste0(filepath2,"la_template.csv"))

labedstemplate <- labedstemplate %>% 
  rename(LocalAuthorityArea = LA,
         age_grp = age)

labedstemplate<-labedstemplate%>% mutate(LocalAuthorityArea=toupper(LocalAuthorityArea),age_grp=toupper(age_grp),reas1=toupper(reas1))
datafile20<-datafile20%>% mutate(LocalAuthorityArea=toupper(LocalAuthorityArea),age_grp=toupper(age_grp),reas1=toupper(reas1))



#amend variables as necessary in order for matchi_numberng to work

labeddaysdatasheetminusstandard <- left_join(datafile20,labedstemplate,
                                             by = c(("LocalAuthorityArea" = "LocalAuthorityArea"), ("age_grp"="age_grp"),("reas1"="reas1")) %>% 
arrange(labeddaysdatasheetminusstandard,LocalAuthorityArea,age_grp,reas1)) # arrange data in order for tables


labeddaysdatasheetminusstandard<-labeddaysdatasheetminusstandard%>% mutate(LocalAuthorityArea=toupper(LocalAuthorityArea),age_grp=toupper(age_grp),reas1=toupper(reas1))

#Add in total for Standard filter on any row that isn't all
datafile32a<-filter(labeddaysdatasheetminusstandard,reas1!="ALL") 
#Rename reason_grp_high_level as Standard where there isn't a Code 9
datafile32<-datafile32a%>% mutate(reas1=
                                    if_else(reas1%in%c("HEALTH AND SOCIAL CARE REASONS","PATIENT/CARER/FAMILY-RELATED REASONS"),"STANDARD","CODE 9"))


#select standard only.
datafile33<-filter(datafile32,reas1=="STANDARD")
# aggregate LA Standard delays

lastandard<- datafile33 %>% 
  group_by(LocalAuthorityArea,age_grp,reas1) %>% 
  summarise(OBDs=sum(OBDs,na.rm=TRUE)) %>% 
  ungroup()

#datafile24<-datafile24%>%rename(OBDs2=obds_in_month)

lastandardandothers<- bind_rows(lastandard, labeddaysdatasheetminusstandard)
lastandardandothers<-lastandardandothers%>% mutate(LocalAuthorityArea=toupper(LocalAuthorityArea),age_grp=toupper(age_grp),reas1=toupper(reas1) %>% 
arrange(lastandardandothers,LocalAuthorityArea,age_grp,reas1)) # issue here is that the rows with zeros don't appear so have to match to output file

#datafile26<-read.csv(paste0(filepath2,"hb_template.csv"))
#bhbbedstepmplate<-bhbbedstepmplate%>% mutate(nhs_board=toupper(nhs_board),age_grp=toupper(age_grp),reason_grp_high_level=toupper(reason_grp_high_level))

#match files


laallvariations <- left_join(labedstemplate,lastandardandothers,
                             by = c(("LocalAuthorityArea" = "LocalAuthorityArea"), ("age_grp"="age_grp"),("reas1"="reas1")))

#replace obds in correct column for all reason groups
laallvariations<-laallvariations %>% mutate(OBDs=
                                              if_else(is.na(OBDs),0L,OBDs) %>% 
arrange(laallvariations,laallvariations$LocalAuthorityArea,laallvariations$age_grp,laallvariations$reas1))

### 15. Save out LA file ----

write.xlsx(laallvariations, here::here("outputs", "LA bed days data sheet_R.xlsx"))





### END OF SCRIPT ###