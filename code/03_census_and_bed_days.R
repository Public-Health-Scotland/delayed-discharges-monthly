##########################################################
# Name of file: 04_Census_and_Bed_Days.R
# Data Release: Delayed Discharges monthflagly publication
# Original author(s): Peter McClurg (spss version: James Mc Nally)
# Original date: 09/10/19 (spss version: 30/11/2017)
# latest update author (if not using version control)
# latest update date (if not using version control)
# latest update description (if not using version control)
# Type of script: Preparation
# Written/run on: R Studio SERVER
# Version of R that the script was most recently run on: ?
# Description of content: Updates trendfile with DD records from most recent publication monthflag.
# Approximate run time: TBC
##########################################################
 

### 1.Housekeeping ----
# This section should be the only section of the script which requires manual changes 
# for future updates and includes:
#   loading package_grps
#   setting filepaths and extract dates
#   functions (defined here or sourced from another file)
#   setting plot parameter
#   specifying codes (e.g. ICD-10 codes)



### Filepaths for latest monthflag # may NOT NEEDED as now within set-up_environment ( check with Russell)

filepath<-("/conf/delayed_discharges/RAP development/2020_02/Outputs/")
filepath2<-("/conf/delayed_discharges/RAP development/2020_02/Data/scotland/")

### 2. Bring in environment ----
#( ran separately in code folder - how do i bring it in without copying / pasting as ?)
source("00_setup_environment.R")

#add in new library needed that isn't in setup.
library(xlsx)

# monthflagflag<-("Apr 2020")

### 3. Get Scotland_validated file for latest monthflag ----

datafile<-read.csv(paste0(filepath2,"SCOTLAND_validated.csv"))
#sum(datafile$obds_in_month) check this matches publication when testing prevous data.

#strip out code 100s


datafile<-datafile %>% filter(datafile$reas1!="Code 100")
#table(datafile$reas1) # check code 100 has been removed

#Create new variables

datafile<-datafile %>% mutate(areaname=" ")
datafile<-datafile %>% mutate(year="2019-20")

### 4a. Rename variables ----
#( may not be needed once full RAP programs used as variables will have been renamed in earlier process)
# datafile <- datafile %>%
# rename(nhs_board=Healthboard,
#          chi_number=CHINo,
#          age_at_rdd=AGEATRDD,
#          postcode=PatientPostcode,
#          local_authority_code=LocalAuthorityArea,
#          date_of_birth=PatientDOB,
#          discharge_specialty_nat_code=SpecialtyCode,
#          date_referred_for_sw_assessment=DateReferralReceived,
#          date_declared_medically_fit=Readyfordischargedate,
#          REASONFORDELAY=REASONFORDELAY,
#          dd_code_2=REASONFORDELAYSECONDARY,
#          out_of_area_case_indicator=Outofareacaseindicator,
#          admission_date=OriginalAdmissionDate,
#          sex_code=Gender,
#          discharge_date=DateDischarge,
#          discharge_to_code=DischargeReason,
#          census_flag=CENSUSFLAG,
#          delay_description=DELAY_DESCRIPTION)

#Create age_grpgGrouping

datafile<-datafile%>% mutate(age_grp=
                               if_else(AGEATRDD<75, "18-74",
                                       if_else(AGEATRDD>=75, "75+", " ")))





#save out file for main bed days file
 

#write_sav(datafile,paste0(filepath,"main bed days file.sav")) # save out file

#set function
'%!in%' <- function(x,y)!('%in%'(x,y))

#Save census data file


datafile2 <- filter(datafile, REASONFORDELAYSECONDARY %!in%c("26X","46X") & CENSUSFLAG=="Y")


#write_sav(datafile2,paste0(filepath,"main census file.sav")) # save out file



# 5. Create Reason 2 Groupings ----

##Sub grouping reason_grp for code 9s are currently being classed as H&SC or Pat/Fam reasons. Ensure they are just code 9s.
##Also bring in Transport as a separate category.

datafile3<-datafile2 %>% 
  mutate(reas2 = as.character(reas2))%>% 
  mutate(reas2=
           if_else(DELAY_DESCRIPTION=="Adults with Incapacity Act", "Adults with Incapacity Act",reas2))

datafile3<-datafile2 %>% 
  mutate(reas2 = as.character(reas2))%>% 
  mutate(reas2=
           if_else(DELAY_DESCRIPTION!="Adults with Incapacity Act" & reas1=="Code 9", "Other code 9 reasons(not AWI)",reas2))

datafile3<-datafile2 %>% 
  mutate(reas2 = as.character(reas2))%>% 
  mutate(reas2=
           if_else(DELAY_DESCRIPTION=="Awaiting availability of transport","Transport",reas2))


#write_sav(datafile3,paste0(filepath,"main census file Tabs 1 3 6.sav")) # save out file

### 6. Get reason / age_grp breakdown for Scotland ----

datafile4<-datafile3
datafile4$level<-"1"

datafile4$areaname<-"Scotland"

Scotlandbymainreasongrouping <- datafile4 %>% 
  group_by(year,MONTHFLAG,level,areaname,age_grp,reas1) %>% 
  summarise(NoofPatients=sum(NoofPatients,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE)) %>% 
  ungroup()

#write_sav(Scotlandbymainreasongrouping,paste0(filepath,"Scotland by main reason grouping.sav")) # save out file


Scotlandbysubreasongrouping <- datafile4 %>% 
  group_by(year,MONTHFLAG,level,areaname,age_grp,reas2) %>% 
  summarise(NoofPatients=sum(NoofPatients,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE)) %>% 
  ungroup()




#write_sav(Scotlandbysubreasongrouping,paste0(filepath,"Scotland by sub reason grouping.sav")) # save out file

### 7. Breakdown for HBs (Level=2) ----
datafile4$level<-"2"
datafile4<-datafile4 %>% mutate(areaname=Healthboard)


HBbymainreasongrouping <- datafile4 %>% 
  group_by(year,MONTHFLAG,level,areaname,age_grp,reas1) %>% 
  summarise(NoofPatients=sum(NoofPatients,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE)) %>% 
  ungroup()

#write_sav(HBbymainreasongrouping,paste0(filepath,"HB by main reason grouping.sav")) # save out file


HBbysubreasongrouping <- datafile4 %>% 
  group_by(year,MONTHFLAG,level,areaname,age_grp,reas2) %>% 
  summarise(NoofPatients=sum(NoofPatients,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE)) %>% 
  ungroup()

#write_sav(HBbysubreasongrouping,paste0(filepath,"HB by sub reason grouping.sav")) # save out file

### 8. Breakdown for LA ----
datafile4$level<-"3"
datafile4<-datafile4 %>% mutate(areaname=LocalAuthorityArea)


labymainreasongrouping <- datafile4 %>% 
  group_by(year,MONTHFLAG,level,areaname,age_grp,reas1) %>% 
  summarise(NoofPatients=sum(NoofPatients,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE)) %>% 
  ungroup()



labysubreasongrouping <- datafile4 %>% 
  group_by(year,MONTHFLAG,level,areaname,age_grp,reas1) %>% 
  summarise(NoofPatients=sum(NoofPatients,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE)) %>% 
  ungroup()

#write_sav(labysubreasongrouping,paste0(filepath,"la by sub reason grouping.sav")) # save out file


### 9. Add files together ----

Scot_HB_la<-bind_rows(Scotlandbymainreasongrouping,Scotlandbysubreasongrouping,HBbymainreasongrouping,HBbysubreasongrouping,
                      labymainreasongrouping,labysubreasongrouping)



#if reas1 is blank(N/A), reas1 = reas2

datafile6<-Scot_HB_la%>% 
  mutate(reas1 = as.character(reas1))%>% 
  mutate(reas1=
           if_else(is.na(reas1), reas2,reas1))

#remove reason_grp
datafile6<-select(datafile6,-reas2)

datafile6a<-datafile6
datafile6a$age_grp<-"All"
ScotHBlaallage_grps<- datafile6a %>% 
  group_by(year,MONTHFLAG,level,areaname,age_grp,reas1) %>% 
  summarise(NoofPatients=sum(NoofPatients,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE)) %>% 
  ungroup()

#write_sav(ScotHBlaallage_grps,paste0(filepath,"ScotHBla all age_grps.sav")) # save out file


ScotHBlaallreasonexcHSCPatFamtotal<-bind_rows(datafile6,ScotHBlaallage_grps)
table(ScotHBlaallreasonexcHSCPatFamtotal$reas1) # check on reason_grp_high_level

#write_sav(ScotHBlaallreasonexcHSCPatFamtotal,paste0(filepath,"ScotHBla all reasons exc HSC PatFam total.sav")) # save out file


###10. Calculate total number of delays excluding code9s ----
datafile7<-filter(datafile6, reas1%in%c("Health and Social Care Reasons","Patient/Carer/Family-related reasons"))
datafile7$reas1<-"All Delays excl. Code 9"

ScotHBlaallreasonsincHSCPatFamtotal<- datafile7 %>% 
  group_by(year,MONTHFLAG,level,areaname,age_grp,reas1) %>% 
  summarise(NoofPatients=sum(NoofPatients,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE)) %>% 
  ungroup()

#write_sav(ScotHBlaallreasonsincHSCPatFamtotal,paste0(filepath,"ScotHBla all reasons inc HSC PatFam total.sav")) # save out file


###11. Calculate the number of delays for all reasons ----

datafile8<-filter(ScotHBlaallreasonexcHSCPatFamtotal, reas1%in%c("Health and Social Care Reasons","Code 9",
                                                                                 "Patient/Carer/Family-related reasons"))
table(datafile8$reas1) # check totals against the syntax output 

datafile8$reas1<-"All"
ScotHBlaallreasonsalldelaystotal<- datafile8 %>% 
  group_by(year,MONTHFLAG,level,areaname,age_grp,reas1) %>% 
  summarise(NoofPatients=sum(NoofPatients,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE)) %>% 
  ungroup()

#add files
Tabs136<-
  bind_rows(ScotHBlaallreasonsalldelaystotal,ScotHBlaallreasonsincHSCPatFamtotal,ScotHBlaallreasonexcHSCPatFamtotal)

arrange(Tabs136,areaname,age_grp,reas1) # arrange data in order for tables


#write_sav(Tabs136,paste0(filepath,"Tabs 1 3 6_R.sav"))

#Tab 4 data - Mean and Media Length of delay IGNORED AS NO LONGER REQUIRED
## datafile2 is the main census file

#datafile2$level<-"1"
#datafile2$areaname<-"Scotland"
#datafile2$age_grp<-"All"

#Create mean/median delay for following high level groupings in reason_grp_high_level
#Code 9 / Health and Social Care Reasons/Patient/Carer/Family-related reasons

#Scotlandhighlevelgrouping <- datafile2 %>% 
#  group_by(year,monthflag,level,areaname,age_grp,reason_grp_high_level) %>% 
#  summarise(mean_delay=mean(delay_at_census),
#            median_delay=median(delay_at_census)) %>% 
#  ungroup()


#write_sav(Scotlandhighlevelgrouping,paste0(filepath,"Scotland high level groupings.sav")) # save out file

#code
#datafile3<-datafile2 %>% mutate(reason_grp_high_level<-"All")


#Scotlandallreasons <- datafile3 %>% 
#  group_by(year,monthflag,level,areaname,age_grp,reason_grp_high_level) %>% 
#  summarise(mean_delay=mean(delay_at_census),
#            median_delay=median(delay_at_census)) %>% 
#  ungroup()


#Recode Reas2 to group Code 9 non-AWI patients into 'Other Code9'.


#datafile3<-datafile3 %>% mutate(reason_grp=
#              if_else(delay_description!="Adults with Incapacity Act" & reason_grp_high_level=="Code 9", "Other code 9 reasons(not AWI)",reason_grp))


#Create Mean / Median delay for sub groupings in reas1 ( which is reason_grp_high_level)

#reas1=reas2
#datafile4<-datafile3 %>% mutate(reason_grp_high_level=reason_grp)

#Scotlandsublevelgroupings <- datafile4 %>% 
#  group_by(year,monthflag,level,areaname,age_grp,reason_grp_high_level) %>% 
#  summarise(mean_delay=mean(delay_at_census),
#            median_delay=median(delay_at_census)) %>% 
#  ungroup()

###12. Now get detailed reason breakdown ----
#get main census file ( datafile2)
datafile5<-datafile2

datafile5$level<-"1"
datafile5$areaname<-"Scotland"
datafile5$age_grp<-"All"
table(datafile5$REASONFORDELAY) # check REASONFORDELAY
#copy secondary Code 9 codes into delay reason - e.g. delay reason 9 becomes 51X etc..
#then update reas1 with individual delay codes in delayreason

#datafile5<-datafile5 %>% mutate(REASONFORDELAY=
#                                 if_else(REASONFORDELAY==9,dd_code_2,REASONFORDELAY))

Scotlandindreasons<- datafile5 %>% 
  group_by(year,MONTHFLAG,level,areaname,age_grp,reas1) %>% 
  summarise(NoofPatients=sum(NoofPatients,na.rm=TRUE),Delay1to3days=sum(Delay1to3days,na.rm=TRUE),
            Delay3to14days=sum(Delay3to14days,na.rm=TRUE),Delay2to4weeks=sum(Delay2to4weeks,na.rm=TRUE),
            Delay4to6weeks=sum(Delay4to6weeks,na.rm=TRUE),Delay6to12weeks=sum(Delay6to12weeks,na.rm=TRUE),
            Delay3to6months=sum(Delay3to6months,na.rm=TRUE),Delay6to12months=sum(Delay6to12months,na.rm=TRUE),
            DelayOver12months=sum(DelayOver12months,na.rm=TRUE),DelayOver3days=sum(DelayOver3days,na.rm=TRUE),
            DelayUnder2wks=sum(DelayUnder2wks,na.rm=TRUE),
            DelayOver6wks=sum(DelayOver6wks,na.rm=TRUE),DelayOver4wks=sum(DelayOver4wks,na.rm=TRUE),
            DelayOver2wks=sum(DelayOver2wks,na.rm=TRUE), acute=sum(acute,na.rm=TRUE),
            gpled=sum(gpled,na.rm=TRUE),notgpled=sum(notgpled,na.rm=TRUE)) %>% 
  ungroup()

# datafile5a<-datafile2
# 
# datafile5a$level<-"1"
# datafile5a$areaname<-"Scotland"
# datafile5a$age_grp<-"All"
# datafile5a$reason_grp_high_level<-"All"
# 
# #copy secondary Code 9 codes into delay reason - e.g. delay reason 9 becomes 51X etc..
# #then update reas1 with individual delay codes in delayreason
# 
# datafile5a<-datafile5a %>% mutate(REASONFORDELAY=
#                                   if_else(REASONFORDELAY==9,dd_code_2,REASONFORDELAY))
# 
# ScotlandindreasonsALL<- datafile5a %>% 
#   group_by(year,monthflag,level,areaname,age_grp,reason_grp_high_level) %>% 
#   summarise(num_pats=sum(num_pats,na.rm=TRUE),delay_1_to_3_days=sum(delay_1_to_3_days,na.rm=TRUE),
#             delay_3_to_14_days=sum(delay_3_to_14_days,na.rm=TRUE),delay_2_to_4_weeks=sum(delay_2_to_4_weeks,na.rm=TRUE),
#             delay_4_to_6_weeks=sum(delay_4_to_6_weeks,na.rm=TRUE),delay_6_to_12_weeks=sum(delay_6_to_12_weeks,na.rm=TRUE),
#             delay_3_to_6_months=sum(delay_3_to_6_months,na.rm=TRUE),delay_6_to_12_months=sum(delay_6_to_12_months,na.rm=TRUE),
#             delay_over_12_months=sum(delay_over_12_months,na.rm=TRUE),delay_over_3_days=sum(delay_over_3_days,na.rm=TRUE),
#             delay_under_2_weeks=sum(delay_under_2_weeks,na.rm=TRUE),
#             delay_over_6_weeks=sum(delay_over_6_weeks,na.rm=TRUE),delay_over_4_weeks=sum(delay_over_4_weeks,na.rm=TRUE),
#             delay_over_2_weeks=sum(delay_over_2_weeks,na.rm=TRUE)) %>% 
#   ungroup()

#match all Scotland reasons and high level reason groupings back to main file and then add on individual reason breakdown


#dataset6test <- rbind(Scotlandhighlevelgrouping, Tabs136, c(year, monthflag, level, areaname, age_grp, reason_grp_high_level))


#datafile6 <- full_join(Tabs136,Scotlandhighlevelgrouping,
#                         by = c(("year" = "year"), ("monthflag"="monthflag"),("level"="level"),("areaname"="areaname"), ("age_grp"="age_grp"),("reason_grp_high_level"="reason_grp_high_level")))

#datafile6a <- full_join(datafile6,Scotlandallreasons,
#                        by = c(("year" = "year"), ("monthflag"="monthflag"),("level"="level"),("areaname"="areaname"), ("age_grp"="age_grp"),("reason_grp_high_level"="reason_grp_high_level")))

#datafile6b <- full_join(datafile6a,Scotlandsublevelgroupings,
#                         by = c(("year" = "year"), ("monthflag"="monthflag"),("level"="level"),("areaname"="areaname"), ("age_grp"="age_grp"),("reason_grp_high_level"="reason_grp_high_level")))


#match files ( excluding files with mean and media - no longer needed )


datafile6<- bind_rows(Tabs136,Scotlandindreasons)

#datafile6 <- left_join(Tabs136, Scotlandindreasons,
#                       by = c(("year" = "year"), ("monthflag"="monthflag"),("level"="level"),("areaname"="areaname"), ("age_grp"="age_grp"),("reason_grp_high_level"="reason_grp_high_level")))



datafile7<- bind_rows(datafile6,Scotlandindreasons)

###ignored the add files to the previous trend file ( prior to July 2016) as the census and bed days files are added to the current latest file

#exclude data that is at too low a level for publication

#a
# datafile7<-datafile7 %>% mutate(delay_1_to_3_days==
#                       if_else(age_grp!="All" & 
#                       reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                       "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons"),0,delay_1_to_3_days))
# 
# datafile7<-datafile7 %>% mutate(delay_3_to_14_days,
#                       if_else(age_grp!="All" & 
#                       reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                       "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,delay_3_to_14_days)))
# 
# datafile7<-datafile7 %>% mutate(delay_2_to_4_weeks,
#                       if_else(age_grp!="All" & 
#                       reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                       "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,delay_2_to_4_weeks)))
# 
# datafile7<-datafile7 %>% mutate(delay_6_to_12_weeks,
#                       if_else(age_grp!="All" & 
#                       reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                       "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,delay_6_to_12_weeks)))
# 
# datafile7<-datafile7 %>% mutate(delay_3_to_6_months,
#                       if_else(age_grp!="All" & 
#                       reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                       "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,delay_3_to_6_months)))
# 
# datafile7<-datafile7 %>% mutate(delay_6_to_12_months,
#                       if_else(age_grp!="All" & 
#                       reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                       "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,delay_6_to_12_months)))
# 
# datafile7<-datafile7 %>% mutate(delay_over_12_months,
#                       if_else(age_grp!="All" & 
#                       reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                       "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,delay_over_12_months)))
# 
# datafile7<-datafile7 %>% mutate(delay_over_3_days,
#                       if_else(age_grp!="All" & 
#                       reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                       "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,delay_over_3_days)))
# 
# datafile7<-datafile7 %>% mutate(delay_under_2_weeks,
#                       if_else(age_grp!="All" & 
#                       reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                       "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,delay_under_2_weeks)))
# 
# datafile7<-datafile7 %>% mutate(delay_over_6_weeks,
#                         if_else(age_grp!="All" & 
#                         reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                         "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,delay_over_6_weeks)))
# 
# datafile7<-datafile7 %>% mutate(delay_over_4_weeks,
#                       if_else(age_grp!="All" & 
#                       reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                       "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,delay_over_4_weeks)))
# 
# datafile7<-datafile7 %>% mutate(median_delay,
#                       if_else(age_grp!="All" & 
#                       reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                       "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,median_delay)))
# 
# datafile7<-datafile7 %>% mutate(mean_delay,
#                       if_else(age_grp!="All" & 
#                       reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                       "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,mean_delay)))
# 
# 
# #b
# 
# datafile7<-datafile7 %>% mutate(delay_1_to_3_days,
#                                 if_else(age_grp=="All" & 
#                                           reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                                                                    "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,delay_1_to_3_days)))
# 
# 
# datafile7<-datafile7 %>% mutate(delay_3_to_14_days,
#                                 if_else(age_grp=="All" & 
#                                           reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                                                                    "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,delay_3_to_14_days)))
# 
# datafile7<-datafile7 %>% mutate(delay_2_to_4_weeks,
#                                 if_else(age_grp=="All" & 
#                                           reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                                                                    "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,delay_2_to_4_weeks)))
# 
# datafile7<-datafile7 %>% mutate(delay_6_to_12_weeks,
#                                 if_else(age_grp=="All" & 
#                                           reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                                                                    "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,delay_6_to_12_weeks)))
# 
# datafile7<-datafile7 %>% mutate(delay_3_to_6_months,
#                                 if_else(age_grp=="All" & 
#                                           reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                                                                    "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,delay_3_to_6_months)))
# 
# datafile7<-datafile7 %>% mutate(delay_6_to_12_months,
#                                 if_else(age_grp=="All" & 
#                                           reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                                                                    "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,delay_6_to_12_months)))
# 
# datafile7<-datafile7 %>% mutate(delay_over_12_months,
#                                 if_else(age_grp=="All" & 
#                                           reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                                                                    "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,delay_over_12_months)))
# 
# datafile7<-datafile7 %>% mutate(delay_over_3_days,
#                                 if_else(age_grp=="All" & 
#                                           reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                                                                    "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,delay_over_3_days)))
# 
# datafile7<-datafile7 %>% mutate(delay_under_2_weeks,
#                                 if_else(age_grp=="All" & 
#                                           reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                                                                    "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,delay_under_2_weeks)))
# 
# datafile7<-datafile7 %>% mutate(acute,
#                                 if_else(age_grp=="All" & 
#                                 reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                                 "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,acute)))
#                                                                       
# datafile7<-datafile7 %>% mutate(gpled,
#                                 if_else(age_grp=="All" & 
#                                 reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                                 "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,gpled)))
#                                                                       
#                                                                       
#                                                                                                                                          
# 
# datafile7<-datafile7 %>% mutate(notgpled,
#                                 if_else(age_grp=="All" & 
#                                 reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                                 "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,notgpled)))
# 
# datafile7<-datafile7 %>% mutate(acute,
#                                 if_else(age_grp!="All" | 
#                                 reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                                 "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,acute)))
# 
# datafile7<-datafile7 %>% mutate(gpled,
#                                 if_else(age_grp!="All" | 
#                                 reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                                 "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,notgpled)))
# 
# 
# datafile7<-datafile7 %>% mutate(notgpled,
#                                 if_else(age_grp!="All" | 
#                                 reason_grp_high_level%!in%c("All","Health and Social Care Reasons",
#                                 "Code 9","All Delays excl.Code 9","Patient/Carer/Family-related reasons",0,notgpled)))

#Note: used datafile6 as the creation of datafile7 doesn't add anything new - PMc: check .

### 13. Save out census file ----

write_sav(datafile6,paste0(filepath,"All census data.sav"))

write.xlsx(datafile6,paste0(filepath,"All census data.xlsx"))

table(datafile6$areaname)






################################################################################
### 14. bed days data ----
################################################################################

#Get filespath+main bed days file.SAV

#datafile11<-read_spss(paste0(filepath,"main bed days file.sav"))


datafile11<-datafile %>% mutate(Healthboard=substring(Healthboard, 5)) # change nhs_board and remove the 'NHS_'

hbbeddaysdiffage_grp<- datafile11 %>% 
  group_by(Healthboard,age_grp,reas1) %>% 
  summarise(OBDs=sum(OBDs,na.rm=TRUE)) %>% 
  ungroup()


#write_sav(hbbeddaysdiffage_grp,paste0(filepath,"hb bed days diff age_grp_R.sav")) # save out file

labeddaysdiffage_grp<- datafile11 %>% 
  group_by(LocalAuthorityArea,age_grp,reas1) %>% 
  summarise(OBDs=sum(OBDs,na.rm=TRUE)) %>% 
  ungroup()


#write_sav(labeddaysdiffage_grp,paste0(filepath,"la bed days diff age_grp_R.sav")) # save out file

datafile12<-datafile11 %>% mutate(Healthboard="Scotland")

Scotbeddaysdiffage_grp<- datafile12 %>% 
  group_by(Healthboard,age_grp,reas1) %>% 
  summarise(OBDs=sum(OBDs,na.rm=TRUE)) %>% 
  ungroup()


#write_sav(Scotbeddaysdiffage_grp,paste0(filepath,"Scot bed days diff age_grp_R.sav")) # save out file


# add files

Scotlandhbbeddays<-rbind(Scotbeddaysdiffage_grp,hbbeddaysdiffage_grp)

# Get Scotland and HB bed days for all ages

datafile13<-Scotlandhbbeddays %>% mutate(age_grp="All")

Scotlandhbbeddays<- datafile13 %>% 
  group_by(Healthboard,age_grp,reas1) %>% 
  summarise(OBDs=sum(OBDs,na.rm=TRUE)) %>% 
  ungroup()

#add files
Scotlandhbbeddaysallagesallreasons<-rbind(Scotlandhbbeddays,Scotbeddaysdiffage_grp,hbbeddaysdiffage_grp)

#write_sav(Scotandhbbeddaysallagesallreasons,paste0(filepath,"Scot and hb bed days_R.sav")) # save out file

datafile15<-Scotlandhbbeddaysallagesallreasons %>% mutate(reas1="All")

ScotlandhbAllage_grpsbeddays<- datafile15 %>% 
  group_by(Healthboard,age_grp,reas1) %>% 
  summarise(OBDs=sum(OBDs,na.rm=TRUE)) %>% 
  ungroup()

Scotlandhbbeddaysallagegrpsallreasons<-rbind(ScotlandhbAllage_grpsbeddays,Scotlandhbbeddaysallagesallreasons)

#write_sav(Scotlandhbbeddaysallagegrpsallreasons,paste0(filepath,"Scot and hb bed days all age_grps all reasons.sav")) # save out file

### 15. Bed days LAs ----
#use la bed days diff age file 
datafile17<-labeddaysdiffage_grp %>% mutate(age_grp="All")
table(datafile17$OBDs)

labeddaysallage_grps<- datafile17 %>% 
  group_by(LocalAuthorityArea,age_grp,reas1) %>% 
  summarise(OBDs=sum(OBDs,na.rm=TRUE)) %>% 
  ungroup()

datafile18<-rbind(labeddaysallage_grps,labeddaysdiffage_grp)

#write_sav(datafile18,paste0(filepath,"la bed days.sav")) # save out file

datafile19<-datafile18 %>% mutate(reas1="All")

labeddaysallreason_grp_high_level<- datafile19 %>% 
  group_by(LocalAuthorityArea,age_grp,reas1) %>% 
  summarise(OBDs=sum(OBDs,na.rm=TRUE)) %>% 
  ungroup()

datafile20<-rbind(labeddaysallreason_grp_high_level,datafile18)

#write_sav(datafile20,paste0(filepath,"la bed days all age_grps all reasons.sav")) # save out file

#### 16. Bad Days HB ----
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

#write_sav(datafile21,paste0(filepath,"hb bed days data sheet minus standard.sav")) # save out file
datafile21<-datafile21%>% mutate(Healthboard=toupper(Healthboard),age_grp=toupper(age_grp),reas1=toupper(reas1))

#Need to ensure there is a total for standard delays (HSC/PFR added together). This is due to the format of the publication data sheet.
#datafile22a<-filter(datafile21,reason_grp_high_level!="ALL") 
#Rename reason_grp_high_level as Standard where there isn't a Code 9
datafile22<-datafile21%>% mutate(reas1=
                                   if_else(reas1%in%c("HEALTH AND SOCIAL CARE REASONS","PATIENT/CARER/FAMILY-RELATED REASONS"),"STANDARD",reas1))
table(datafile22$reas1)

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


### 17. Save out bed days HB file ----

write.xlsx(datafile27,paste0(filepath,"HB bed days data sheet_R.xlsx"))

#########################################
### 18. LA bed days  ----
#########################################
labedstemplate<-read.csv(paste0(filepath2,"la_template.csv"))

labedstemplate <- labedstemplate %>% 
  rename(LocalAuthorityArea = LA,
         age_grp = age)

labedstemplate<-labedstemplate%>% mutate(LocalAuthorityArea=toupper(LocalAuthorityArea),age_grp=toupper(age_grp),reas1=toupper(reas1))
datafile20<-datafile20%>% mutate(LocalAuthorityArea=toupper(LocalAuthorityArea),age_grp=toupper(age_grp),reas1=toupper(reas1))

##################################################################

#amend variables as necessary in order for matchi_numberng to work

labeddaysdatasheetminusstandard <- left_join(datafile20,labedstemplate,
                                             by = c(("LocalAuthorityArea" = "LocalAuthorityArea"), ("age_grp"="age_grp"),("reas1"="reas1")))

arrange(labeddaysdatasheetminusstandard,LocalAuthorityArea,age_grp,reas1) # arrange data in order for tables

#write_sav(labeddaysdatasheetminusstandard,paste0(filepath,"la bed days data sheet minus standard.sav")) # save out file
labeddaysdatasheetminusstandard<-labeddaysdatasheetminusstandard%>% mutate(LocalAuthorityArea=toupper(LocalAuthorityArea),age_grp=toupper(age_grp),reas1=toupper(reas1))

#Add in total for Standard filter on any row that isn't all
datafile32a<-filter(labeddaysdatasheetminusstandard,reas1!="ALL") 
#Rename reason_grp_high_level as Standard where there isn't a Code 9
datafile32<-datafile32a%>% mutate(reas1=
                                    if_else(reas1%in%c("HEALTH AND SOCIAL CARE REASONS","PATIENT/CARER/FAMILY-RELATED REASONS"),"STANDARD","CODE 9"))
table(datafile32$reas1)

#select standard only.
datafile33<-filter(datafile32,reas1=="STANDARD")
# aggregate LA Standard delays

lastandard<- datafile33 %>% 
  group_by(LocalAuthorityArea,age_grp,reas1) %>% 
  summarise(OBDs=sum(OBDs,na.rm=TRUE)) %>% 
  ungroup()

#datafile24<-datafile24%>%rename(OBDs2=obds_in_month)

lastandardandothers<- bind_rows(lastandard, labeddaysdatasheetminusstandard)
lastandardandothers<-lastandardandothers%>% mutate(LocalAuthorityArea=toupper(LocalAuthorityArea),age_grp=toupper(age_grp),reas1=toupper(reas1))


arrange(lastandardandothers,LocalAuthorityArea,age_grp,reas1) # issue here is that the rows with zeros don't appear so have to match to output file

#datafile26<-read.csv(paste0(filepath2,"hb_template.csv"))
#bhbbedstepmplate<-bhbbedstepmplate%>% mutate(nhs_board=toupper(nhs_board),age_grp=toupper(age_grp),reason_grp_high_level=toupper(reason_grp_high_level))

#match files


laallvariations <- left_join(labedstemplate,lastandardandothers,
                             by = c(("LocalAuthorityArea" = "LocalAuthorityArea"), ("age_grp"="age_grp"),("reas1"="reas1")))

#replace obds in correct column for all reason groups
laallvariations<-laallvariations %>% mutate(OBDs=
                                              if_else(is.na(OBDs),0L,OBDs))



laallvariations<-arrange(laallvariations,laallvariations$LocalAuthorityArea,laallvariations$age_grp,laallvariations$reas1)

### 19. Save out LA file ----

write.xlsx(laallvariations,paste0(filepath,"LA bed days data sheet_R.xlsx"))





### END OF SCRIPT ###