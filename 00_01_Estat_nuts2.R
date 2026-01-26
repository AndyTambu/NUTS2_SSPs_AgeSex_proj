
# Infos ####
# Auhtor: Andrea Tamburini 
# content: here we follow the download and manipulation of the data from eurostat but considering that now we have a 
#          TS available which is lomger than what we had before


#libraries ####
library(tidyverse)

library(viridis) # just for the colors 

library(raster)

library(eurostat)

library(sf)

library(terra)

library(sp)

library('wcde')

library (readr)

library(zoo)




# 1. Estat lookup ####

## 1.1 looking for the datasets ####

# 
# search_eurostat("Population", type = "dataset") %>% 
#   filter(grepl("NUTS 2", title)&grepl("age", title)) %>% 
#   distinct() %>% 
#   View()

## 1.2 exploratory data analysys of the dataset ####

# demo_r_pjangroup <-  eurostat::get_eurostat('demo_r_pjangroup')
# 
# demo_r_pjangroup %>% distinct(TIME_PERIOD) %>% pull() 
# 
# save(demo_r_pjangroup, file = './data/demo_r_pjangroup__with2024.RData')

load("./data/demo_r_pjangroup__with2024.RData")
demo_r_pjangroup <- 
  demo_r_pjangroup %>% 
  dplyr::rename('time' = 'TIME_PERIOD')

demo_r_pjangroup %>% 
  dplyr::filter(geo == "UK", sex == "T", age == "TOTAL") %>% 
  View()

demo_r_pjangroup_ageGroups <- 
  demo_r_pjangroup %>%
  filter(age %in% c(
    "TOTAL","UNK","Y10-14","Y5-9","Y_LT5",
    "Y15-19","Y20-24",
    "Y25-29","Y30-34",
    "Y35-39","Y40-44"
    ,"Y45-49","Y50-54"
    ,"Y55-59","Y60-64"
    ,"Y65-69","Y70-74"
    ,"Y75-79","Y80-84"
    ,"Y_GE75")) %>% 
  spread(age, values) %>% 
  mutate(
    'under15' = `Y10-14`+`Y5-9`+`Y_LT5`,
    '15_24' = `Y15-19`+`Y20-24`,
    '25_44' = `Y25-29`+`Y30-34`+`Y35-39`+`Y40-44`,
    '45_64' = `Y45-49`+`Y50-54`+`Y55-59`+`Y60-64`,
    '65+' = `Y65-69`+`Y70-74`+`Y_GE75`,
  ) %>% 
  dplyr::select('sex', 'geo', 'time', 'TOTAL', 'UNK', 'under15', '15_24', '25_44', '45_64', '65+') 


# these are the data before the time grouping (useful to understand the presence/absence of data)

# and now we fix the year groups 
time_intervals <- tibble(time = c(
  "1990-01-01","1991-01-01","1992-01-01","1993-01-01","1994-01-01",
  "1995-01-01","1996-01-01","1997-01-01","1998-01-01","1999-01-01",
  "2000-01-01","2001-01-01","2002-01-01","2003-01-01","2004-01-01",
  "2005-01-01","2006-01-01","2007-01-01","2008-01-01","2009-01-01",
  "2010-01-01","2011-01-01","2012-01-01","2013-01-01","2014-01-01",
  "2015-01-01","2016-01-01","2017-01-01","2018-01-01","2019-01-01",
  "2020-01-01","2021-01-01","2022-01-01","2023-01-01","2024-01-01"
),
interval = c(rep(c('1990-1994','1995-1999','2000-2004', '2005-2009', '2010-2014', '2015-2019'), each = 5),
             rep('2020-2024', 5)))


demo_r_pjangroup_ageGroups_timeIntervals_lessAgeGroups <- 
  demo_r_pjangroup_ageGroups %>% 
  mutate(time = as.character(time)) %>% 
  inner_join(time_intervals, by = 'time') %>% 
  gather(age.group, values, -c(sex,geo,time,interval)) %>% 
  group_by(sex,geo,interval,age.group) %>% 
  summarise(values = mean(values, na.rm=T)) %>% 
  spread(age.group, values) %>% 
  dplyr::select(sex,geo,interval,TOTAL,under15,`15_24`,`25_44`,`45_64`,`65+`,UNK) %>% 
  ungroup()
# demo_r_pjangroup_ageGroups_timeIntervals_lessAgeGroups %>% distinct(interval)



# 2. Dependent Variables ####
# this is the first one, which is just teh collection of the proportions

demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups <- 
  demo_r_pjangroup_ageGroups_timeIntervals_lessAgeGroups %>% 
  filter(str_length(geo) %in% c(2,4)) %>%  # here we want either length 2 or 4 because this is the length of the NUTS0 (ctry) or of the NUTS2
  mutate_at(vars('under15',`15_24`,`25_44`,`45_64`,`65+`,UNK), list(~ ./`TOTAL`)) %>% 
  mutate(interval = factor(interval, levels = c("1990-1994","1995-1999","2000-2004","2005-2009","2010-2014","2015-2019","2020-2024")))

# demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups %>% 
#   arrange(interval) %>% 
#   distinct(interval)


# 
save(demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups, file = './data/dep_variables/demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups_with2024.RData')

#load(file = './01_Age_Structure/AAA_longer_timeSeries/data/dep_variables/demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups.RData')

# this are now the pure proportions of the different age groups for a specified period and sex. 


always_present <- 
  demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups %>% 
  filter(TOTAL != 1 & interval != '2020-2024' ) %>% # when building the training set we do not really care about what we see in the years which are 
  # going to be used as test set!
  group_by(interval) %>% 
  summarise(codes = unique(geo)) %>% 
  filter(!str_length(codes) < 4) %>% 
  summarise(ctrs = paste0(codes, collapse = " ")) %>% 
  ungroup() %>%
  mutate(ctrs = str_split(ctrs, " ")) %>%
  summarise(col1 = toString(Reduce(intersect, ctrs))) %>% 
  mutate(col1 =  strsplit(col1, ', ')) %>% 
  pull(col1) %>% 
  unlist()

length(always_present) # this is of course exaclty the same we have int he other one.


always_present_ctry <- unique(str_sub(always_present, start = 1, end = 2))

ctry_lvl_prop <- 
  demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups %>% 
  # filter(geo %in% always_present_ctry) %>%  # I am not so sure why this is here at this stage. 
  dplyr::select(-c('UNK', 'TOTAL')) %>% 
  mutate(interval = fct_infreq(interval)) %>% 
  pivot_longer(cols = c('under15',`15_24`,`25_44`,`45_64`,`65+`),names_to='age.group', values_to = 'ctry_prop') %>% 
  group_by(sex,geo,age.group) %>% ### CORRECTION 23.11.2022
  mutate(ctry_lag1 = lag(ctry_prop, 1),
         ctry_lag2 = lag(ctry_prop, 2)) %>% 
  ungroup()


# dep variable ####
dep_vrbl <- 
  demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups %>% 
  dplyr::select(-c('UNK','TOTAL')) %>% 
  pivot_longer(cols = c('under15',`15_24`,`25_44`,`45_64`,`65+`),names_to='age.group', values_to = 'nuts2_prop') %>% 
  filter(geo %in% always_present) %>% 
  mutate(ctry = str_sub(geo, start = 1, end = 2)) %>% 
  inner_join(ctry_lvl_prop %>% 
               dplyr::select(-c(ctry_lag1,ctry_lag2)) %>% 
               rename(ctry = geo) ,
             by = c('sex', 'ctry', 'age.group','interval')) %>% 
  mutate(interval = fct_inorder(interval)) %>% 
  mutate(quot = nuts2_prop/ctry_prop, diff = nuts2_prop-ctry_prop) %>% 
  
  mutate(log.quot = log(quot)) %>% 
  
  group_by(sex,geo,age.group) %>% 
  mutate(diff_1 = lag(diff),diff_diffs = (diff-lag(diff)))

save(dep_vrbl, file = './data/dep_variables/dep_vrbl.RData')

# load( './01_Age_Structure/AAA_longer_timeSeries/data/dep_variables/dep_vrbl.RData')






