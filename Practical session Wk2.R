library(tidyverse)
library(lubridate)
##### Path of working directory #####
FALSE_DATA_2018_exclusions_week2 <- readRDS("~/course_materials/data/FALSE_DATA_2018_exclusions_week2.rds")
### rename the pathname #####
exclusions <- readRDS("~/course_materials/data/FALSE_DATA_2018_exclusions_week2.rds")
#### Ensuring the both start and end dates have normal dates. ######
exclusions <- exclusions %>% 
  mutate(startdate = lubridate::dmy(startdate),
         finishdate = lubridate::dmy(finishdate))
##########
exclusions %>% 
  mutate(startyear = lubridate::year(startdate))
############## checking thier no cases after 2011 ####
exclusions %>% 
  mutate(startyear = lubridate::year(startdate)) %>% 
  group_by(startyear) %>% 
  count()
### filter for any dates after the census date 27 March 2011 ####
exclusions <- exclusions %>% 
  filter(startdate < ymd("2011-03-27"))

exclusions <- exclusions %>% 
  mutate(startmonth = month(startdate))
#### frequency by month ###
exclusions %>% 
  group_by(startmonth) %>% 
  count()
#### create a summer interval for the holidays ###
summer_09 <- interval(ymd("2009-06-28"), ymd("2009-08-18"))
exclusions %>% 
  mutate(hols_09 = ifelse(startdate %within% summer_09, 1, 0)) %>%
  filter(hols_09 == 1)
### find the day of the week ####
exclusions <- exclusions %>% 
  mutate(startdow = lubridate::wday(startdate, label = TRUE))
#### same for Saturday, this time adding two days.#######
exclusions <- exclusions %>% 
  mutate(startdate = if_else(startdow == "Sun", startdate + days(1), startdate)) %>% 
  mutate(startdate = if_else(startdow == "Sat", startdate + days(2), startdate))

#### repeat for finish dates #####
exclusions <- exclusions %>% 
  mutate(finishdate = if_else(startdow == "Sun", finishdate + days(1), finishdate)) %>% 
  mutate(finishdate = if_else(startdow == "Sat", finishdate + days(2), finishdate))
### Removing duplicates ###
dplyr::glimpse(exclusions)
View(exclusions)
#####flag for multiple start dates by person####
exclusions <- exclusions %>% 
  group_by(synid, startdate) %>% 
  mutate(multiple_start = duplicated(synid)) %>% 
  ungroup()
##### checking for duplicates in start dates & end dates #####
exclusions <- exclusions %>% 
  group_by(synid, startdate, finishdate) %>% 
  mutate(multiple_start_end = duplicated(synid)) %>% 
  ungroup()
#### comparing to indicators #######
exclusions %>% 
  count(multiple_start, multiple_start_end)
#####these exclusions using filter() and View()####
exclusions %>% 
  filter(multiple_start != multiple_start_end) %>% 
  View()
#####it's person xxxx, so we can look at all their exclusions####
exclusions %>% 
  filter(synid == xxxx) %>% 
  View()
#############
exclusions <- exclusions %>% 
  group_by(synid) %>% # for each person
  mutate(finishdate_lead = if_else(multiple_start != multiple_start_end,
                                   lag(finishdate),
                                   finishdate))

# Now drop the original finishdate variable and rename the new finishdate_lead
# as finishdate
exclusions <- exclusions %>% 
  select(-finishdate) %>% 
  rename(finishdate = finishdate_lead)
##### flag for multiple reasons####
exclusions <- exclusions %>% 
  group_by(synid, startdate, finishdate) %>% 
  mutate(multiple_reasons = duplicated(synid))
#######
exclusions %>% 
  filter(multiple_reasons == FALSE)
##### we want to count the exclusions attirbutable to multiple reasons####
exclusions %>% 
  group_by(multiple_reasons) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(proportion = n / sum(n))
###########
exclusions %>% 
  group_by(synid, startdate, finishdate) %>% 
  mutate(reason_n = row_number()) %>% 
  mutate(multiple_reason_exclusion = ifelse(max(reason_n > 1), 1, 0)) %>% 
  ungroup() %>% 
  count(multiple_reason_exclusion)
######################
exclusions <- 
  exclusions %>% 
  group_by(synid, startdate, finishdate) %>% 
  mutate(reason_n = row_number()) %>% 
  ungroup()
##### then we can keep only the highest reason_n for each exclusion ######
exclusions <- 
  exclusions %>% 
  group_by(synid, startdate, finishdate) %>% 
  filter(reason_n == max(reason_n)) %>% 
  ungroup() %>% 
  select(-incidenttype)
##### Calculating duration ########
exclusions <- exclusions %>% 
  mutate(excl_duration = finishdate - startdate)

#### check for missing values#####
exclusions %>% 
  mutate(excl_duration = finishdate - startdate) %>% 
  filter(is.na(excl_duration))
#####average duration? #######
exclusions %>% 
  mutate(excl_duration = finishdate - startdate) %>% 
  summarise(mean_duration = mean(excl_duration))
##### Better remove the NAs #####
exclusions %>% 
  mutate(excl_duration = finishdate - startdate) %>% 
  summarise(mean_duration = mean(excl_duration, na.rm = TRUE)) 
# finding the minimum and maximum durations 
exclusions %>% 
  ungroup() %>% 
  mutate(excl_duration = finishdate - startdate) %>% 
  filter(!is.na(excl_duration)) %>% 
  summarise(mean_duration = mean(excl_duration),
            min_duration = min(excl_duration),
            max_duration = max(excl_duration)) 
#####################################
exclusions %>%                
  group_by(reason_n) %>% 
  filter(!is.na(excl_duration)) %>% 
  summarise(mean_duration = mean(excl_duration))
#### calculating the cumulative duration of exclusions ######
exclusions %>% 
  group_by(synid) %>% 
  filter(!is.na(excl_duration)) %>% 
  summarise(sum_duration = sum(excl_duration)) %>% 
  mutate(sum_duration = as.numeric(sum_duration))
####### this gives us a dataset with one value per person ############
exclusions_link <- exclusions %>% 
  group_by(synid, flag) %>% 
  filter(!is.na(excl_duration)) %>% 
  summarise(sum_duration = sum(excl_duration)) %>% 
  mutate(sum_duration = as.numeric(sum_duration)) %>% 
  ungroup()
#### save the data####
saveRDS(exclusions_link,
        "FALSE_DATA_2018_duration_exclusion_week3.rds")

############### INDEX JOIN #################################
library(tidyverse)
######## load old data ########
exclusions <- readRDS("data/FALSE_DATA_2018_exclusions_week2.rds")
############ Knowing the type of variables #######
glimpse(exclusions)
##### Checking exclusion from start date ##########
exclusions <- 
  exclusions %>% 
  mutate(startdate = lubridate::dmy(startdate),
         finishdate = lubridate::dmy(finishdate))
##### Extracting a year from a start date ########
exclusions %>% 
  mutate(startyear = lubridate::year(startdate))
exclusions %>% 
  mutate(startyear = lubridate::year(startdate)) %>% 
  group_by(startyear) %>% 
  count()
#### Ensuring there is no case after 2011 ######
exclusions <- exclusions %>% 
  filter(startdate < lubridate::ymd("2011-03-27"))
##### finding a unique start and end date for exclusion ####
exclusions <- exclusions %>% 
  group_by(synid, startdate) %>% 
  mutate(id_date_ind = row_number()) %>% 
  group_by(synid, finishdate) %>% 
  mutate(id_date_ind2 = row_number())
##### where 2 index variables disagree #######
exclusions <- exclusions %>% 
  mutate(flag_identical = id_date_ind == id_date_ind2)

exclusions %>% 
  filter(flag_identical == FALSE)
###
exclusions %>% 
  filter(synid == xxxx)
#### convert to wide dataset by exclusion reason #####
exclusions <- 
  exclusions %>% 
  group_by(synid, 
           startdate, finishdate) %>% 
  mutate(reason_n = row_number()) %>% 
  ungroup()
######## transforming into wide using pivot_wider ####
install.packages("dplyr")
library(dplyr)
exclusions_wide <- exclusions %>%
  select(flag:incidenttype, noprovdays, intaltprov, reason_n) %>% 
  pivot_wider(names_prefix = "reason_", # gives a prefix to the column (variable) instead of just numbers
              names_from = reason_n, 
              values_from = incidenttype)

##Characterizing each exclusion episode by identical startdate and finishdate##.
exclusions_wide %>% 
  group_by(synid, startdate) %>% 
  mutate(finishdate_n = row_number()) %>% 
  filter(finishdate_n > 1)

exclusions_wide %>% 
  filter(synid == xxxx)
######replace reason_2 for the second exclusion record#######
exclusions_wide <- 
  exclusions_wide %>% 
  arrange(synid, startdate, finishdate) %>% 
  mutate(reason_2 = if_else(synid == xxxx &
                              startdate == lubridate::ymd("2008-02-19"),
                            dplyr::lag(reason_1), reason_2))
######remove the duplicate record ######
exclusions_wide <- 
  exclusions_wide %>% 
  filter(synid != xxxx |
           finishdate != lubridate::ymd("2008-02-20"))
#####make multi-reason exclusion variable#####
exclusions_wide <- 
  exclusions_wide %>% 
  mutate(multi_reason_exclusion = if_else(!is.na(reason_2),
                                          "Multiple reasons",
                                          "Single reason"))
######making multiple exclusions per child variable####
exclusions_wide <- 
  exclusions_wide %>% 
  group_by(synid) %>% 
  mutate(exclusion_number = row_number())
#####making one row per child#####
n_exclusions <- 
  exclusions_wide %>% 
  count(synid) %>% 
  rename(n_exclusions = n)
####merging with school census####
school_census <- readRDS("~/course_materials/data/FALSE_DATA_2018_school_census_week3.rds") 
#####remind ourselves what's in the school census###
glimpse(school_census)
###### join the two datasets together#####
linked_school_cen_excl_n <- left_join(school_census, n_exclusions, by = c("synid"))
#####What to do if TRUE is value "Exclusion" and if FALSE "No_exclusion"####
linked_school_cen_excl_n <- linked_school_cen_excl_n %>% 
  mutate(excl_flag = ifelse(!is.na(n_exclusions), "Exclusion", "No_exclusion"))
# Using left_join we'll link the school census with the exclusions_wide data
linked_school_cen_excl <- left_join(school_census, exclusions_wide, by = c("synid", "flag"))
glimpse(linked_school_cen_excl$multi_reason_exclusion)
# replace cases with no exclusion from NA to No exclusion
linked_school_cen_excl <- linked_school_cen_excl %>% 
  mutate(multi_reason_exclusion = replace_na(multi_reason_exclusion, "No Exclusion"))
# the summary variables we want for each child are the number of exclusions#####
####the mean number of noprovdays. Save this as a new object#####
school_cen_excl_by_child <- 
  linked_school_cen_excl %>% 
  mutate(excl_flag = ifelse(!is.na(exclusion_number), "Exclusion", "No_exclusion")) %>% 
  group_by(flag, synid, gender, freemeal, prop_abs_grp, examS4_grp, excl_flag) %>% 
  summarise(n_exclusions = sum(excl_flag == "Exclusion")) %>% 
  ungroup() 
######join this to the duration of exclusion dataset we cretaed in the dates ######
exclusion_duration <- readRDS("FALSE_DATA_2018_duration_exclusion_week3.rds") 
school_cen_excl_by_child <- left_join(school_cen_excl_by_child, exclusion_duration, by = c("synid", "flag"))
glimpse(school_cen_excl_by_child$sum_duration)
# replace NA with zero for duration
school_cen_excl_by_child <- school_cen_excl_by_child %>% 
  mutate(sum_duration = replace_na(sum_duration, 0)
######saving the dataset######
saveRDS(school_cen_excl_by_child, file = "FALSE_DATA_2018_school_census_and_exclusion_week3")
#####joining with census data###
census <- readRDS("~/course_materials/data/FALSE_DATA_2018_census_week3.rds") 
#as before, let's join the two datasets together using left_join, with census as the first dataset#####
census_education <- left_join(census, school_cen_excl_by_child, by = c("synid", "flag"))
saveRDS(census_education, "FALSE_DATA_2018_census_education_week4.rds")
#### see how it looks like###
glimpse(census_education$excl_flag)
#### first let's see if exclusions at all vary by gender#####
gen_excl_xtab <- census_education %>% 
  group_by(flag, synid, gender, freemeal, prop_abs_grp, examS4_grp, excl_flag) %>% 
  summarise(n_exclusions = sum(excl_flag == "Exclusion")) %>% 
  group_by(gender, excl_flag) %>% 
  count() %>% 
  filter(!is.na(gender))
#### model function test using chi square###
model1 <- chisq.test(gen_excl_xtab$n)
## see results#####
print(model1)
###### multiple reason exclusions#######
multi_gen_excl_xtab <- 
  linked_school_cen_excl %>% 
  group_by(gender, multi_reason_exclusion) %>% 
  count()

model2 <- chisq.test(multi_gen_excl_xtab$n)

print(model2)