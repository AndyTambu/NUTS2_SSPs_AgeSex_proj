library(tidyverse)
library(purrr)
library(wcde)


# data ----
##proj results ----

proj_res_all_abs_vls <- tibble(read.csv('./results/projections/proj_res_all_abs_vls.csv'))
colnames(proj_res_all_abs_vls) <- c("geo","interval","under15","15_24","25_44","45_64","65+","ctry","scenario","tot.pop")

proj_res_all_abs_vls %>% 
  group_by(ctry, scenario, interval) %>% 
  summarise(tot_pop = sum(tot.pop)) %>% 
  dplyr::filter(interval == "2020-2024"&scenario == 2)

wcde_Ita <- wcde::get_wcde(indicator = "epop", country_name = 'Italy',pop_age = c("all"), pop_sex = c("total"))



ISO_codes <- read.csv('./data/ctry_infos/all.csv') %>% 
  dplyr::select(alpha.2, country.code) %>% 
  filter(country.code != 438) %>% 
  dplyr::mutate(alpha.2 = dplyr::recode(alpha.2 , "GB"="UK" , "GR" = "EL"))

ISO.num.ctry <- ISO_codes[ISO_codes$alpha.2 %in% c(proj_res_all_abs_vls %>%  distinct(ctry) %>% pull(),'EL','UK'),]$country.code 
# this we have to make it for all the ssps

## wcde data----
wcde_raw_list <- lapply(as.character(seq(1, 5)), function(z) {
  result <- wcde::get_wcde('pop', 
                           scenario = z, 
                           pop_age = c('all'), 
                           pop_sex = 'all', 
                           pop_edu = 'total', 
                           country_code = ISO.num.ctry)
  
  closeAllConnections()  # Close all connections after each iteration
  return(result)  # Return the result for this iteration
})


wcde_proj_tot <- dplyr::bind_rows(wcde_raw_list) %>% 
                  rename(geo = name) %>% 
                   dplyr::select(-country_code)


wcde_raw <- 
  wcde_proj_tot %>%
  tibble() %>% 
  dplyr::mutate(geo = dplyr::recode(geo, 
                                     "Albania" = "AL","Austria"='AT',"Belgium"= 'BE',"Bulgaria"='BG' ,                                          
                                     "Croatia" = "HR", "Czech Republic" = "CZ", "Denmark" =  "DK" ,"Estonia" = "EE",                                              
                                     "Finland" = 'FI',"France" = 'FR',"Germany" = 'DE',
                                     "Greece" = 'EL',"Hungary" = 'HU',"Iceland" = 'IS',                                             
                                     "Ireland"='IE',"Italy"='IT',"Latvia" ='LV', "Lithuania" = 'LT', "Luxembourg"= 'LU',
                                     "Malta" = 'MT', "Montenegro"= 'ME',"Netherlands"='NL',"Norway" = 'NO',
                                     "Poland"= 'PL',"Portugal"='PT',
                                     "Romania"= 'RO',"Serbia"='RS',
                                     "Slovakia"='SK',"Slovenia"='SI',"Spain"='ES',
                                     "Sweden" = 'SE',"Switzerland"='CH',"The former Yugoslav Republic of Macedonia"  = 'MK',         
                                     "Turkey"= 'TR',"United Kingdom of Great Britain and Northern Ireland"='UK','Cyprus' = 'CY')) %>% 
  filter(age != 'All'&str_length(geo)==2) %>% 
  dplyr::mutate(sex = dplyr::recode(sex, 
                                    "Both" = 'T',
                                    "Male" = 'M',
                                    "Female" = 'F'))
  




age_grouping <- tibble(
  age = c("0--4","5--9","10--14","15--19","20--24","25--29","30--34","35--39","40--44","45--49","50--54","55--59","60--64","65--69","70--74","75--79","80--84","85--89","90--94","95--99","100+",'TOTAL'),
  age.group = fct_inorder(c(rep('under15',3),rep('15_24',2),rep('25_44',4),rep('45_64',4),rep('65+',8),'TOTAL'))
)

wcde_raw_ageGrouped_F_over_T <- 
  wcde_raw %>%
  group_by(geo,
           sex,
           year, scenario) %>% 
  summarise(age = 'TOTAL',  # combine times
            pop = sum(pop)) %>%                  # get sum of sizes
  bind_rows(wcde_raw, .)   %>% 
  left_join(age_grouping, by = 'age') %>% 
  group_by(age.group,sex,geo,year,scenario) %>% 
  dplyr::summarise(pop = sum(pop)*1000) %>% 
  ungroup() %>% 
  dplyr::filter(year >= 2020 &  year <= 2100 & sex != 'T') %>% 
  mutate(interval = fct_inorder(paste(year,year+4,sep = '-'))) %>% 
  pivot_wider(names_from = sex, values_from = pop) %>% 
  dplyr::mutate(F_over_T = F/(M+F)) %>% 
  dplyr::select(-c('F','M', year)) %>% 
  pivot_wider(names_from = interval, values_from = F_over_T) %>% 
  mutate(
    across(
      .cols = all_of(as.character(paste(seq(2020, 2100, by = 5),seq(2020, 2100, by = 5)+4,sep = '-'))),
      .fns  = ~ .x / `2020-2024`
    )      ) %>% 
  dplyr::select(-`2020-2024`) %>% 
  dplyr::rename('ctry' = 'geo')
  
wcde_raw_ageGrouped_F_over_T %>% 
  distinct(ctry) %>% pull()



## beginnign points ----
load(file = './data/dep_variables/demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups_with2024.RData')

evolution_F_over_T <- 
demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups %>% 
  dplyr::filter(geo %in% 
                  c(proj_res_all_abs_vls %>% 
                  distinct(geo) %>% pull())) %>% 
  dplyr::mutate(ctry = str_sub(geo,1,2)) %>% 
  dplyr::filter(interval %in%  c("2015-2019","2020-2024") &str_length(geo) == 4) %>% 
  mutate(
    across(
      .cols = c(under15, `15_24`, `25_44`, `45_64`,  `65+`),
      .fns  = ~ .x * `TOTAL`
    )      ) %>% 
  dplyr::select(-c(UNK,TOTAL)) %>% 
  pivot_longer(cols = -c('ctry', 'sex','geo','interval' ), names_to = 'age.group', values_to = 'pop') %>% 
  pivot_wider(names_from = sex, values_from = pop) %>% 
  dplyr::mutate(F_over_T = F/(M+F)) %>% 
  dplyr::select(-c('F','M','T')) %>% 
  pivot_wider(names_from = interval, values_from = F_over_T) %>% 
  dplyr::mutate(`2020-2024` = if_else(is.na(`2020-2024`), `2015-2019` ,`2020-2024`))  %>% 

  left_join(wcde_raw_ageGrouped_F_over_T, by = c('ctry', 'age.group')) %>% 
  mutate(
    across(
      .cols = all_of(as.character(paste(seq(2025, 2100, by = 5),seq(2025, 2100, by = 5)+4,sep = '-'))),
      .fns  = ~ .x * `2020-2024`
    )      ) %>% 
  pivot_longer(cols = -c('geo', 'ctry', 'age.group', 'scenario'), names_to = 'interval', values_to = 'F_over_T') 
  
evolution_F_over_T %>% 
  distinct(ctry) %>% pull()

demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups %>% 
  dplyr::filter(geo == "UKG1")

df <- 
proj_res_all_abs_vls %>% 
  dplyr::mutate(scenario = as.character(scenario)) %>% 
  dplyr::select(-c(tot.pop)) %>% 
  pivot_longer(cols = -c('geo','interval', 'ctry', 'scenario'), names_to = 'age.group', values_to = 'pop') %>% 
  inner_join(
  evolution_F_over_T,
  by = c('geo','interval', 'ctry', 'scenario', 'age.group')) %>% 
  dplyr::mutate(`F` = pop * F_over_T,
                M = pop * (1-F_over_T),
                `T` = pop) %>% 
  dplyr::select(-c(pop,F_over_T )) %>% 
  pivot_longer(cols = -c('geo', 'ctry', 'interval', 'age.group', 'scenario'), names_to = 'sex', values_to = 'pop') 

df %>%  group_by(ctry, scenario, interval,sex) %>% 
  summarise(tot_pop = sum(pop)) %>% 
  dplyr::filter(interval == "2020-2024"&scenario == 2)



df_total <- df %>% 
  group_by(geo, interval, ctry, scenario, sex) %>%
  summarise(
    pop = sum(pop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(age.group = "TOTAL")

df_total %>% distinct(interval)

df_total%>%  group_by(ctry, scenario, interval,sex) %>% 
  summarise(tot_pop = sum(pop)) %>% 
  dplyr::filter(interval == "2020-2024"&scenario == 2)



# Bind total rows to original data
proj_res_all_abs_vls_withSEX <- bind_rows(df, df_total)



proj_res_all_abs_vls_withSEX %>% 
  group_by(ctry, scenario, interval,sex) %>% 
  summarise(tot_pop = sum(pop)) %>% 
  dplyr::filter(interval == "2020-2024"&scenario == 2)


proj_res_all_abs_vls_withSEX <- 
  proj_res_all_abs_vls_withSEX %>% 
  dplyr::filter(ctry != "TR")


# 
# write.csv(proj_res_all_abs_vls_withSEX, file = './results/projections/proj_res_all_abs_vls_withSEX.csv', row.names = F)
# 
# 







