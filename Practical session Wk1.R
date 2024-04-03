install.packages("tidyverse")
library(tidyverse)
install.packages("skimr")
install.packages("VIM")
library(skimr) 
library(VIM)
library(readr)
census <- read_csv("~/course_materials/raw_data/FALSE_DATA_2018_census_week1.csv")
View(census)
summary(census)
###### converting character values to factors#######
census %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  summary()
######################################################
skimr::skim(census) 
summary(census$agep0)
####### check the accuacy of the DOB from SLS################
census %>% 
  mutate(calc_age = 2001 - slsdobyr)
##################################################
census %>%  
  mutate(calc_age = 2001 - slsdobyr,
         age_diff = calc_age - agep0)
######################################
census %>% 
  mutate(calc_age = 2001 - slsdobyr,
         age_diff = calc_age - agep0) %>% 
  select(age_diff) %>% 
  table() 
######################################################
census %>% 
  mutate(calc_age = 2001 - slsdobyr,
         age_diff = calc_age - agep0) %>% 
  filter(age_diff > 1 | age_diff < -1)
###### Taking a closer look at our derived variable age_diff#####
census %>% 
  mutate(calc_age = 2001 - slsdobyr,
         age_diff = calc_age - agep0) %>% 
  filter(age_diff > 1 | age_diff < -1) %>% 
  select(age_diff, calc_age, agep0) %>% 
  arrange(desc(age_diff))
###### replace missing variables of age_diff with NA#######
census <- census %>% 
  mutate(agep0 = replace(agep0, agep0 == -999, NA))
##########################
census %>% 
  mutate(ademh0 = replace(ademh0, ademh0 == "Missing", NA)) %>% 
  count(ademh0)
### to get the structure of your missing data ######
census %>% 
  mutate(across(where(is.character), as.factor),
         across(where(is.factor), ~ factor(replace(., . == "Missing", NA)))) %>% 
  VIM::aggr(.)
#### Recoding Variables and changing intergers to factors ###
census <- census %>%  
  mutate(slsdobmt = as.factor(slsdobmt),
         slsdobmt = recode(slsdobmt,
                           "1" = "Jan",
                           "2" = "Feb",
                           "3" = "Mar",
                           "4" = "Apr",
                           "5" = "May",
                           "6" = "Jun",
                           "7" = "Jul",
                           "8" = "Aug",
                           "9" = "Sep",
                           "10" = "Oct",
                           "11" = "Nov",
                           "12" = "Dec"))
#### Doing same for no_sibs_grp ######
census <- census %>% 
  mutate(no_sibs_grp = as.factor(no_sibs_grp),
         no_sibs_grp = recode(no_sibs_grp,
                              "0" = "no siblings",
                              "1" = "1 sibling",
                              "2" = "2 siblings",
                              "3" = "3+ siblings"))
##### Summarize data excluding missing values ####
census %>% 
    summarise(mean_age = mean(agep0, na.rm = TRUE))
###############################################
census %>% 
  count()
##### Summarize the mean age by sex ####
census %>% 
  group_by(sex0) %>% 
  summarise(mean_age = mean(agep0, na.rm = TRUE))
#### Summarize the mean with a standard deviation ######
census %>% 
  group_by(sex0) %>% 
  summarise(mean_age = mean(agep0, na.rm = TRUE),
            sd_age = sd(agep0, na.rm = TRUE))
#### Summarize the mean age by sex & tenure #####
census %>% 
  group_by(sex0,newten) %>% 
  summarise(mean_age = mean(agep0, na.rm = TRUE),
            sd_age = sd(agep0, na.rm = TRUE))
####### Calculating different councils with different tenure status#####
census %>% 
  group_by(councilarea, newten) %>% 
  count()
####### to know the number of children by tenure & council area, then find the proportion of children in the council area.#####
census %>% 
  group_by(newten, councilarea) %>% 
  count() %>% 
  group_by(councilarea) %>% 
  mutate(proportion = n / sum(n)) %>% 
  arrange(councilarea)
############################
census %>% 
  mutate(ethnic_grp = recode(ethgrp0,
                             "African" = "BAME",
                             "Any Mixed Background" = "BAME",
                             "Bangladeshi" = "BAME",
                             "Black Scottish or Other Black" = "BAME",
                             "Caribbean"  = "BAME",
                             "Chinese" = "BAME",
                             "Indian" = "BAME",
                             "Missing" = "Missing",
                             "NCR (non-resident students)" = "Missing",
                             "Other Ethnic Group" = "BAME",
                             "Other South Asian" = "BAME",
                             "Other White" = "White",
                             "Other White British" = "White",
                             "Pakistani" = "BAME",
                             "White Irish" = "White",
                             "White Scottish" = "White")) %>% 
  count(ethnic_grp, ethgrp0)
##### so much typing this can be summarized #########
census %>% 
  mutate(ethnic_grp = fct_collapse(ethgrp0,
                                   BAME    = c("African", "Any Mixed Background", "Bangladeshi",
                                               "Black Scottish or Other Black", "Caribbean",
                                               "Chinese", "Indian", "Other Ethnic Group", 
                                               "Other South Asian", "Pakistani"),
                                   White   = c("Other White", "Other White British",
                                               "White Irish", "White Scottish"),
                                   Missing = c("Missing", "NCR (non-resident students)"))) %>% 
  count(ethnic_grp)
######### arranging them into categories######
census %>%
  mutate(ethnic_grp = fct_lump(ethgrp0)) %>%
  count(ethnic_grp)
####3# select the number of categories to display ########
census %>%
  mutate(ethnic_grp = fct_lump(ethgrp0, n = 5)) %>%
  count(ethnic_grp)
############
census %>% 
  mutate(help1 = as.factor(help1)) %>% 
  count(help1)
######### factor levels structure based on hours #########
census <- census %>% 
  mutate(help1 = as.factor(help1)) %>% 
  mutate(hours_help = fct_relevel(help1,
                                  "(1) No",
                                  "(2) Yes, 1-19 hours a week",
                                  "(5) Yes, 20-34 hours a week",
                                  "(6) Yes, 35-49 hours a week",
                                  "(4) Yes, 50+ hours a week"))

census %>% 
  count(hours_help)
##############
census <- census %>% 
  mutate(ademh0 = fct_collapse(ademh0,
                               "0" = "0",
                               "1" = "1",
                               "2" = "2",
                               "3+" = "3",
                               "3+" = "4",
                               NULL = "Missing"))
census %>% 
  count(ademh0)
census %>% 
  count(crsh0)

census <- census %>% 
  mutate(crsh0 = fct_collapse(crsh0,
                              "0 carers in household" = "0 carers in household",
                              "1+ carers in household" = c("1 carer in household",
                                                           "2 carers in household",
                                                           "3 carers in household",
                                                           "4 carers in household"),
                              NULL = "Missing"))
####
census %>% 
  count(crsh0)
################
census <- census %>% 
  mutate(sex0 = fct_recode(sex0, NULL = "Missing"))

census %>% 
  count(sex0)
#####################
census <- census %>% 
  mutate(neet = fct_collapse(ecop1,
                             non_neet = c("(1) Economically active (excluding full-time students): In employment: Employee, part-time", 
                                          "(2) Economically active (excluding full-time students): In employment: Employee, full-time",
                                          "(4) Economically active (excluding full-time students): In employment: Self-employed with employees, full-time",
                                          "(5) Economically active (excluding full-time students): In employment: Self-employed without employees, part-time",
                                          "(6) Economically active (excluding full-time students): In employment: Self-employed without employees, full-time",
                                          "(8) Economically active full-time students: In employment: Employee, part-time",
                                          "(9) Economically active full-time students: In employment: Employee, full-time",
                                          "(10) Economically active full-time students: In employment: Self-employed with employees, part-time",
                                          "(11) Economically active full-time students: In employment: Self-employed with employees, full-time",
                                          "(12) Economically active full-time students: In employment: Self-employed without employees, part-time",
                                          "(13) Economically active full-time students: In employment: Self-employed without employees, full-time",
                                          "(14) Economically active full-time students: Unemployed: Seeking work and available to start in 2 weeks or waiting to st",
                                          "(16) Economically inactive: Student"),
                             neet = c("(17) Economically inactive: Looking after home or family",
                                      "(18) Economically inactive: Long-term sick or disabled",
                                      "(19) Economically inactive: Other",
                                      "(7) Economically active (excluding full-time students): Unemployed: Seeking work and available to start in 2 weeks or wa"),
                             NULL = c("(-88) No code required")))
##################
saveRDS(census, "~/course_materials/raw_data/FALSE_DATA_2018_census_week3.rds") 

### Exploring school census###
school_census <- read_csv("raw_data/FALSE_DATA_2018_school_census_week1.csv") 

glimpse(school_census)

summary(school_census)

# Recode prop_abs_grp in the school_census dataset, reorder the levels of 

school_census <- school_census %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  mutate(prop_abs_grp = fct_relevel(prop_abs_grp,
                                    "< 5",
                                    ">= 5 & <10",
                                    ">= 10 & < 20",
                                    ">= 20"),
         examS4_grp = fct_relevel(examS4_grp,
                                  "0",
                                  "1-3",
                                  "4-6",
                                  ">6"))


saveRDS(school_census, "data/FALSE_DATA_2018_school_census_week3.rds")


exclusions <- read_csv("raw_data/FALSE_DATA_2018_exclusions_week1.csv")


str(exclusions)

str(census)

glimpse(exclusions)

summary(exclusions)

skimr::skim(exclusions)

exclusions <- exclusions %>% 
  mutate(noprovdays = as.numeric(noprovdays))


exclusions <- exclusions %>% 
  mutate(noprovdays = na_if(noprovdays, -999))

exclusions <- exclusions %>% 
  mutate(across(where(is.character), as.factor))


saveRDS(exclusions, "data/FALSE_DATA_2018_exclusions_week2.rds") 



census %>% 
  mutate(age_grp = ifelse(slsdobyr == 1994, "young", "not young"))


census %>% 
  mutate(age_grp = case_when(
    slsdobyr == 1994 ~ "young",
    slsdobyr == 1991 ~ "old",
    TRUE ~ "neither young nor old")) %>%  glimpse


exclusions %>% 
  arrange(startdate)


exclusions %>% 
  arrange(desc(startdate))

exclusions %>% 
  arrange(desc(noprovdays))



census <- census %>% 
  mutate(councilarea = as.character(councilarea),
         councilarea = ifelse(agep0 > 8, 
                              recode(councilarea, "Clackmannanshire" = "Clackmananshire"), 
                              councilarea),
         councilarea = ifelse(agep0 < 7 & councilarea == "Aberdeen City", 
                              str_to_lower(councilarea), 
                              councilarea))

# Looking at our factor levels
census %>% 
  count(councilarea)


census <- census %>% 
  mutate(councilarea = str_to_title(councilarea))


census %>% 
  mutate(councilarea = str_to_lower(councilarea))


census %>% 
  mutate(councilarea = str_to_upper(councilarea))


census <- census %>% 
  mutate(councilarea = recode(councilarea, "Clackmananshire" = "Clackmannanshire"))


census %>% 
  unite(council, c(councilarea, ctydis0)) %>%  
  count(council)

census <- census %>% 
  unite(council, c(councilarea, ctydis0), sep = " - ")

census %>% count(council)


census <- census %>% 
  separate(council, c("councilarea", "ctydis0"), sep = " - ")





