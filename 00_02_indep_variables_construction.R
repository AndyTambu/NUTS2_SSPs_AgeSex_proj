# Infos ####
# Auhtor: Andrea Tamburini 
# content: building of the independent variable. Pay attention, the naming is chose according to the one of Erich. 

# considerig the presence of different countries. 


#libraries ####
library(tidyverse)

library(eurostat)

library(zoo)

library(regions)

library(raster)

library(eurostat)

library(sp)


getwd()


# data ####
# since they are the same one we created in the 01_02 file of this folder we exploit the things we already did for the definition of the dependent variable. 

# considering that we have the necessity to define also the what is the training and the test set we have now to decide which is the correct set of NUTS2. 
# since we also need Laged variables, we need to coherently define the ones which are available in all the different years-groups.



load(file = './data/dep_variables/demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups_with2024.RData')

demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups %>% 
  distinct(interval)

always_present <- 
  demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups %>% 
  
  filter(TOTAL != 1&interval != '2020-2024') %>%
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

length(always_present)


always_present_ctry <- unique(str_sub(always_present, start = 1, end = 2))

length(always_present_ctry)

demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups %>% 
  filter(geo == 'AT'&sex == 'T')


ctry_lvl_prop <- 
  demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups %>% 
  filter(geo %in% always_present_ctry) %>% 
  dplyr::select(-c('UNK', 'TOTAL')) %>% 
  mutate(interval = fct_infreq(interval)) %>% 
  pivot_longer(cols = c('under15',`15_24`,`25_44`,`45_64`,`65+`),names_to='age.group', values_to = 'ctry_prop') %>% 
  group_by(sex,geo,age.group) %>% ### CORRECTION 23.11.2022
  mutate(ctry_lag1 = lag(ctry_prop, 1),
         ctry_lag2 = lag(ctry_prop, 2)) %>% 
  ungroup()


# dep variable ####



load(file = './data/dep_variables/dep_vrbl.RData')

dep_vrbl %>% ungroup %>% 
  dplyr::mutate(ctry = str_sub(geo,1,2)) %>% 
  distinct(ctry) %>% 
  pull() 

# dep_vrbl %>% 
#   ungroup() %>% 
#   distinct(interval)


# we use this tibble to produce the dependent variable and the country-specific dependent variable


# ctry_spec independent variables ####

# 
# ctry_spec_ind_var_old <- 
#   dep_vrbl %>% 
#   rename(y = diff_quot) %>% 
#   mutate(lag1_ctry_prop = lag(ctry_prop, 1),
#          lag2_ctry_prop = lag(lag1_ctry_prop, 1),
#          lag1_ctry_pop_g = lag1_ctry_prop/lag2_ctry_prop,
#          lag1_quot = lag(quot,1),
#          lag2_quot = lag(lag1_quot,1)) %>% 
#   dplyr::select(-c(ctry,nuts2_prop,y)) %>% 
#   ungroup() %>% 
#   pivot_wider(names_from = age.group, 
#               values_from = c(
#                 'quot', 'lag1_quot', 'lag2_quot',
#                 'ctry_prop', 'lag1_ctry_prop', 'lag2_ctry_prop', 'lag1_ctry_pop_g'),
#               names_glue = "{.value}_{age.group}")


ctry_spec_ind_var <- 
  dep_vrbl %>% 
  group_by(sex,geo,age.group) %>%  
  mutate(y = quot) %>% 
  mutate(lag1_ctry_prop = lag(ctry_prop, 1),
         lag2_ctry_prop = lag(lag1_ctry_prop, 1),
         lag1_ctry_pop_g = lag1_ctry_prop/lag2_ctry_prop,
         lag1_quot = lag(quot,1),
         lag2_quot = lag(lag1_quot,1)) %>% 
  dplyr::select(-c(ctry,nuts2_prop,y)) %>% 
  ungroup() %>% 
  mutate(age = age.group) %>% 
  pivot_wider(names_from = age,  values_from = c(
    'quot', 'lag1_quot', 'lag2_quot',
    'ctry_prop', 'lag1_ctry_prop', 'lag2_ctry_prop', 'lag1_ctry_pop_g'),
    names_glue = "{.value}_{age}") %>% 
  group_by(geo, interval,sex) %>% 
  mutate_at(vars(contains(c(
    'quot', 'lag1_quot', 'lag2_quot',
    'ctry_prop', 'lag1_ctry_prop', 'lag2_ctry_prop', 'lag1_ctry_pop_g'))), ~mean(.,na.rm = TRUE)) %>% 
  ungroup()



# 251 are the ones which are present from the beginning until the end 

# ggplot() +
#   geom_sf( data = geodata %>%  filter(NUTS_ID %in% always_present), aes(geometry =geometry), fill = 'blue', alpha = 0.4, size = 0.02, color = 'black') +
#   coord_sf(xlim = c(-25, 45), ylim = c(30, 73), expand = FALSE) +
#   theme_minimal()
# 


#   1. training_set:  ####


## 1.1 2000/2004 - 1995/1999 ####


train_2000.2004_1995.1999_ctry <- 
  demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups %>% 
  filter(TOTAL != 0 & sex == 'T' & geo %in% always_present) %>% # here we filter for the presence in every interval (the ones which we use to train the model)
  dplyr::select(-UNK) %>% 
  gather(age.group, prop, c("under15","15_24","25_44","45_64","65+")) %>% 
  mutate(abs.value = TOTAL * prop) %>% 
  dplyr::select(-TOTAL) %>% 
  filter(interval %in% c('1990-1994',"1995-1999","2000-2004")) %>% 
  mutate(interval = fct_inorder(interval)) %>% 
  pivot_wider( names_from = interval, values_from = c(prop,abs.value)) %>% 
  #mutate(y = `prop_2005-2009` - `prop_2000-2004`) %>%  
  group_by(geo) %>% 
  mutate(#`tot_pop_NUTS2_1990-1994` = sum(`abs.value_1990-1994`),
    `tot_pop_NUTS2_1990-1994` = sum(`abs.value_1990-1994`),
    `tot_pop_NUTS2_1995-1999` = sum(`abs.value_1995-1999`),
    `tot_pop_NUTS2_2000-2004` = sum(`abs.value_2000-2004`)
  ) %>% 
  ungroup() %>% 
  mutate(g.pop.NUTS2 = `tot_pop_NUTS2_2000-2004`/`tot_pop_NUTS2_1995-1999`) %>%    # growth in total NUTS2 region. This taken coherently with the year of the# here I follow the order of what was defined by Erich in the supporting material
  mutate(`lag1_` = `prop_1995-1999`) %>% 
  mutate(`lag2_` = `prop_1990-1994`) %>% 
  mutate(`lag1_g_` = `prop_1995-1999` / `prop_1990-1994`) %>% 
  mutate(lag1.tot_pop_NUTS2 = `tot_pop_NUTS2_1995-1999`) %>% 
  mutate(lag2.tot_pop_NUTS2 = `tot_pop_NUTS2_1990-1994`) %>% 
  mutate(lag1ratio.tot_pop_NUTS2 = `tot_pop_NUTS2_1995-1999` / `tot_pop_NUTS2_1990-1994`) %>% 
  # mutate(lag2ratio.tot_pop_NUTS2 = `tot_pop_NUTS2_1995-1999` / `tot_pop_NUTS2_1990-1994`) %>% 
  dplyr::select("geo","age.group","g.pop.NUTS2",
                "lag1_","lag2_","lag1_g_",                
                "lag1.tot_pop_NUTS2","lag2.tot_pop_NUTS2",
                "lag1ratio.tot_pop_NUTS2")%>%
  mutate(age = age.group) %>% 
  pivot_wider(names_from = age, values_from = c("lag1_","lag2_","lag1_g_") , names_glue = "{.value}{age}", values_fill = ) %>% 
  group_by(geo) %>% 
  mutate_at(vars(contains(c("lag1_","lag2_","lag1_g_"))), ~mean(.,na.rm = TRUE)) %>% 
  ungroup() %>% 
  inner_join(ctry_spec_ind_var %>% 
               filter(interval == '2000-2004' & sex == 'T'), by = c('geo','age.group')) %>% 
  inner_join(
      dep_vrbl %>%
      ungroup() %>% 
      filter(interval == '2000-2004' & sex == 'T') %>% 
      dplyr::select(geo, age.group, quot) %>% 
      rename(y = quot), by = c('geo','age.group'))



train_2000.2004_1995.1999_ctry %>% 
  colnames()

train_2000.2004_1995.1999_ctry %>% 
  dplyr::mutate(ctry = str_sub(geo,1,2)) %>% 
  distinct(ctry) %>% 
  pull()

save(train_2000.2004_1995.1999_ctry,file = './data/train&test/train_2000.2004_1995.1999_ctry.RData')



## 1.2 2005/2009 - 2000/2004 ####


train_2005.2009_2000.2004_ctry <- 
  demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups %>% 
  filter(TOTAL != 0 & sex == 'T' & geo %in% always_present) %>% # here we filter for the presence in every interval (the ones which we use to train the model)
  dplyr::select(-UNK) %>% 
  gather(age.group, prop, c("under15","15_24","25_44","45_64","65+")) %>% 
  mutate(abs.value = TOTAL * prop) %>% 
  dplyr::select(-TOTAL) %>% 
  filter(interval %in% c("1995-1999","2000-2004","2005-2009")) %>% 
  mutate(interval = fct_inorder(interval)) %>% 
  pivot_wider( names_from = interval, values_from = c(prop,abs.value)) %>% 
  #mutate(y = `prop_2005-2009` - `prop_2000-2004`) %>%  
  group_by(geo) %>% 
  mutate(#`tot_pop_NUTS2_1990-1994` = sum(`abs.value_1990-1994`),
    `tot_pop_NUTS2_1995-1999` = sum(`abs.value_1995-1999`),
    `tot_pop_NUTS2_2000-2004` = sum(`abs.value_2000-2004`),
    `tot_pop_NUTS2_2005-2009` = sum(`abs.value_2005-2009`)) %>% 
  ungroup() %>% 
  mutate(g.pop.NUTS2 = `tot_pop_NUTS2_2005-2009`/`tot_pop_NUTS2_2000-2004`) %>%    # growth in total NUTS2 region. This taken coherently with the year of the# here I follow the order of what was defined by Erich in the supporting material
  mutate(`lag1_` = `prop_2000-2004`) %>% 
  mutate(`lag2_` = `prop_1995-1999`) %>% 
  mutate(`lag1_g_` = `prop_2000-2004` / `prop_1995-1999`) %>% 
  mutate(lag1.tot_pop_NUTS2 = `tot_pop_NUTS2_2000-2004`) %>% 
  mutate(lag2.tot_pop_NUTS2 = `tot_pop_NUTS2_1995-1999`) %>% 
  mutate(lag1ratio.tot_pop_NUTS2 = `tot_pop_NUTS2_2000-2004` / `tot_pop_NUTS2_1995-1999`) %>% 
  # mutate(lag2ratio.tot_pop_NUTS2 = `tot_pop_NUTS2_1995-1999` / `tot_pop_NUTS2_1990-1994`) %>% 
  dplyr::select("geo","age.group","g.pop.NUTS2",
                "lag1_","lag2_","lag1_g_",                
                "lag1.tot_pop_NUTS2","lag2.tot_pop_NUTS2",
                "lag1ratio.tot_pop_NUTS2")%>%
  mutate(age = age.group) %>% 
  pivot_wider(names_from = age, values_from = c("lag1_","lag2_","lag1_g_") , names_glue = "{.value}{age}", values_fill = ) %>% 
  group_by(geo) %>% 
  mutate_at(vars(contains(c("lag1_","lag2_","lag1_g_"))), ~mean(.,na.rm = TRUE)) %>% 
  ungroup() %>% 
  inner_join(ctry_spec_ind_var %>% 
               filter(interval == '2005-2009' & sex == 'T'), by = c('geo','age.group')) %>% 
  inner_join(
    dep_vrbl %>% 
      ungroup() %>% 
      filter(interval == '2005-2009' & sex == 'T') %>% 
      dplyr::select(geo, age.group, quot) %>% 
      rename(y = quot), by = c('geo','age.group'))




train_2005.2009_2000.2004_ctry %>% 
  colnames()
save(train_2005.2009_2000.2004_ctry,file = './data/train&test/train_2005.2009_2000.2004_ctry.RData')





## 1.3 2010/2014 - 2005/2009 ####


train_2010.2014_2005.2009_ctry <- 
  demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups %>% 
  filter(TOTAL != 0 & sex == 'T' & geo %in% always_present) %>% # here we filter for the presence in every interval (the ones which we use to train the model)
  dplyr::select(-UNK) %>% 
  gather(age.group, prop, c("under15","15_24","25_44","45_64","65+")) %>% 
  mutate(abs.value = TOTAL * prop) %>% 
  dplyr::select(-TOTAL) %>% 
  filter(interval %in% c("2000-2004","2005-2009", "2010-2014")) %>% 
  mutate(interval = fct_inorder(interval)) %>% 
  pivot_wider( names_from = interval, values_from = c(prop,abs.value)) %>% 
  #mutate(y = `prop_2005-2009` - `prop_2000-2004`) %>%  
  group_by(geo) %>% 
  mutate(#`tot_pop_NUTS2_1990-1994` = sum(`abs.value_1990-1994`),
    `tot_pop_NUTS2_2000-2004` = sum(`abs.value_2000-2004`),
    `tot_pop_NUTS2_2005-2009` = sum(`abs.value_2005-2009`),
    `tot_pop_NUTS2_2010-2014` = sum(`abs.value_2010-2014`)) %>% 
  ungroup() %>% 
  mutate(g.pop.NUTS2 = `tot_pop_NUTS2_2010-2014`/`tot_pop_NUTS2_2005-2009`) %>%    # growth in total NUTS2 region. This taken coherently with the year of the# here I follow the order of what was defined by Erich in the supporting material
  mutate(`lag1_` = `prop_2005-2009`) %>% 
  mutate(`lag2_` = `prop_2000-2004`) %>% 
  mutate(`lag1_g_` = `prop_2005-2009` / `prop_2000-2004`) %>% 
  mutate(lag1.tot_pop_NUTS2 = `tot_pop_NUTS2_2005-2009`) %>% 
  mutate(lag2.tot_pop_NUTS2 = `tot_pop_NUTS2_2000-2004`) %>% 
  mutate(lag1ratio.tot_pop_NUTS2 = `tot_pop_NUTS2_2005-2009` / `tot_pop_NUTS2_2000-2004`) %>% 
  # mutate(lag2ratio.tot_pop_NUTS2 = `tot_pop_NUTS2_1995-1999` / `tot_pop_NUTS2_1990-1994`) %>% 
  dplyr::select("geo","age.group","g.pop.NUTS2",
                "lag1_","lag2_","lag1_g_",                
                "lag1.tot_pop_NUTS2","lag2.tot_pop_NUTS2",
                "lag1ratio.tot_pop_NUTS2")%>%
  mutate(age = age.group) %>% 
  pivot_wider(names_from = age, values_from = c("lag1_","lag2_","lag1_g_") , names_glue = "{.value}{age}", values_fill = ) %>% 
  group_by(geo) %>% 
  mutate_at(vars(contains(c("lag1_","lag2_","lag1_g_"))), ~mean(.,na.rm = TRUE)) %>% 
  ungroup() %>% 
  inner_join(ctry_spec_ind_var %>% 
               filter(interval == '2010-2014' & sex == 'T'), by = c('geo','age.group')) %>% 
  inner_join(
    dep_vrbl %>% 
      ungroup() %>% 
      filter(interval == '2010-2014' & sex == 'T') %>% 
      dplyr::select(geo, age.group, quot) %>% 
      rename(y = quot), by = c('geo','age.group'))


save(train_2010.2014_2005.2009_ctry,file = './data/train&test/train_2010.2014_2005.2009_ctry.RData')



## 1.4 2015/2019 - 2010/2014 ####


train_2015.2019_2010.2014_ctry <- 
  demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups %>% 
  filter(TOTAL != 0 & sex == 'T' & geo %in% always_present) %>% # here we filter for the presence in every interval (the ones which we use to train the model)
  dplyr::select(-UNK) %>% 
  gather(age.group, prop, c("under15","15_24","25_44","45_64","65+")) %>% 
  mutate(abs.value = TOTAL * prop) %>% 
  dplyr::select(-TOTAL) %>% 
  filter(interval %in% c("2005-2009", "2010-2014","2015-2019")) %>% 
  mutate(interval = fct_inorder(interval)) %>% 
  pivot_wider( names_from = interval, values_from = c(prop,abs.value)) %>% 
  #mutate(y = `prop_2005-2009` - `prop_2000-2004`) %>%  
  group_by(geo) %>% 
  mutate(#`tot_pop_NUTS2_1990-1994` = sum(`abs.value_1990-1994`),
    `tot_pop_NUTS2_2005-2009` = sum(`abs.value_2005-2009`),
    `tot_pop_NUTS2_2010-2014` = sum(`abs.value_2010-2014`),
    `tot_pop_NUTS2_2015-2019` = sum(`abs.value_2015-2019`)) %>% 
  ungroup() %>% 
  mutate(g.pop.NUTS2 = `tot_pop_NUTS2_2015-2019`/`tot_pop_NUTS2_2010-2014`) %>%    # growth in total NUTS2 region. This taken coherently with the year of the# here I follow the order of what was defined by Erich in the supporting material
  mutate(`lag1_` = `prop_2010-2014`) %>% 
  mutate(`lag2_` = `prop_2005-2009`) %>% 
  mutate(`lag1_g_` = `prop_2010-2014` / `prop_2005-2009`) %>% 
  mutate(lag1.tot_pop_NUTS2 = `tot_pop_NUTS2_2010-2014`) %>% 
  mutate(lag2.tot_pop_NUTS2 = `tot_pop_NUTS2_2005-2009`) %>% 
  mutate(lag1ratio.tot_pop_NUTS2 = `tot_pop_NUTS2_2010-2014` / `tot_pop_NUTS2_2005-2009`) %>% 
  # mutate(lag2ratio.tot_pop_NUTS2 = `tot_pop_NUTS2_1995-1999` / `tot_pop_NUTS2_1990-1994`) %>% 
  dplyr::select("geo","age.group","g.pop.NUTS2",
                "lag1_","lag2_","lag1_g_",                
                "lag1.tot_pop_NUTS2","lag2.tot_pop_NUTS2",
                "lag1ratio.tot_pop_NUTS2")%>%
  mutate(age = age.group) %>% 
  pivot_wider(names_from = age, values_from = c("lag1_","lag2_","lag1_g_") , names_glue = "{.value}{age}", values_fill = ) %>% 
  group_by(geo) %>% 
  mutate_at(vars(contains(c("lag1_","lag2_","lag1_g_"))), ~mean(.,na.rm = TRUE)) %>% 
  ungroup() %>% 
  inner_join(ctry_spec_ind_var %>% 
               filter(interval == '2015-2019' & sex == 'T'), by = c('geo','age.group')) %>% 
  inner_join(
    dep_vrbl %>% 
      ungroup() %>% 
      filter(interval == '2015-2019' & sex == 'T') %>% 
      dplyr::select(geo, age.group, quot) %>% 
      rename(y = quot), by = c('geo','age.group'))


save(train_2015.2019_2010.2014_ctry,file = './data/train&test/train_2015.2019_2010.2014_ctry.RData')











# 2. test_set: ####

## 2.1 2020/2024 - 2015/2019 ####

test_2020.2024_2015.2019_ctry <- 
  demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups %>% 
  filter(TOTAL != 0 & sex == 'T' & geo %in% always_present) %>% # here we filter for the presence in every interval (the ones which we use to train the model)
  dplyr::select(-UNK) %>% 
  gather(age.group, prop, c("under15","15_24","25_44","45_64","65+")) %>% 
  mutate(abs.value = TOTAL * prop) %>% 
  dplyr::select(-TOTAL) %>% 
  filter(interval %in% c("2010-2014","2015-2019", "2020-2024")) %>% 
  mutate(interval = fct_inorder(interval)) %>% 
  pivot_wider( names_from = interval, values_from = c(prop,abs.value)) %>% 
  #mutate(y = `prop_2005-2009` - `prop_2000-2004`) %>%  
  group_by(geo) %>% 
  mutate(#`tot_pop_NUTS2_1990-1994` = sum(`abs.value_1990-1994`),
    `tot_pop_NUTS2_2010-2014` = sum(`abs.value_2010-2014`),
    `tot_pop_NUTS2_2015-2019` = sum(`abs.value_2015-2019`),
    `tot_pop_NUTS2_2020-2024` = sum(`abs.value_2020-2024`)) %>% 
  ungroup() %>% 
  mutate(g.pop.NUTS2 = `tot_pop_NUTS2_2020-2024`/`tot_pop_NUTS2_2015-2019`) %>%    # growth in total NUTS2 region. This taken coherently with the year of the# here I follow the order of what was defined by Erich in the supporting material
  mutate(`lag1_` = `prop_2015-2019`) %>% 
  mutate(`lag2_` = `prop_2010-2014`) %>% 
  mutate(`lag1_g_` = `prop_2015-2019` / `prop_2010-2014`) %>% 
  mutate(lag1.tot_pop_NUTS2 = `tot_pop_NUTS2_2015-2019`) %>% 
  mutate(lag2.tot_pop_NUTS2 = `tot_pop_NUTS2_2010-2014`) %>% 
  mutate(lag1ratio.tot_pop_NUTS2 = `tot_pop_NUTS2_2015-2019` / `tot_pop_NUTS2_2010-2014`) %>% 
  # mutate(lag2ratio.tot_pop_NUTS2 = `tot_pop_NUTS2_1995-1999` / `tot_pop_NUTS2_1990-1994`) %>% 
  dplyr::select("geo","age.group","g.pop.NUTS2",
                "lag1_","lag2_","lag1_g_",                
                "lag1.tot_pop_NUTS2","lag2.tot_pop_NUTS2",
                "lag1ratio.tot_pop_NUTS2")%>%
  mutate(age = age.group) %>% 
  pivot_wider(names_from = age, values_from = c("lag1_","lag2_","lag1_g_") , names_glue = "{.value}{age}", values_fill = ) %>% 
  group_by(geo) %>% 
  mutate_at(vars(contains(c("lag1_","lag2_","lag1_g_"))), ~mean(.,na.rm = TRUE)) %>% 
  ungroup() %>% 
  inner_join(ctry_spec_ind_var %>% 
               filter(interval == '2020-2024' & sex == 'T'), by = c('geo','age.group')) %>% 
  inner_join(
    dep_vrbl %>% 
      ungroup() %>% 
      filter(interval == '2020-2024' & sex == 'T') %>% 
      dplyr::select(geo, age.group, quot) %>% 
      rename(y = quot), by = c('geo','age.group'))



# train_2010.2014_2005.2009_ctry %>% 
#   distinct(geo)
# 
# test_2020.2023_2015.2019_ctry%>% 
#   distinct(geo)
# 

save(test_2020.2024_2015.2019_ctry,file = './data/train&test/test_2020.2024_2015.2019_ctry.RData')








































