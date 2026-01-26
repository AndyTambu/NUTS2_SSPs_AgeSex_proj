# Infos ####
# Auhtor: Andrea Tamburini 
# content: we harmonise and re-fix the initial point for the different variables considering that we have much more data points (until 2024).


library(wcde)
library(tidyverse)

# list.files('./Data/')
# load('./Data/demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups.RData')
load(file = "./data/GAO_wcde_adjusted_2024/NUTS2_projection.RData")

load(file = './data/dep_variables/demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups_with2024.RData')
# demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups %>% 
#   distinct(interval)

# NUTS2_projection[grepl('IT',NUTS2_projection)] %>% length()


load(  file = './data/UK_reconstructed20202024.RData') #hand-reconstructed UK data since they stopped deliverying to Estat after brexit


ctry_lvl_prop <- 
  demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups %>% 
  filter(geo %in% c("AL", "RS", NUTS2_projection %>% str_sub(start = 1, end = 2) %>% unique())) %>% ####here we added RS and AL !!!
  dplyr::select(-c('UNK', 'TOTAL')) %>% 
  mutate(interval = fct_infreq(interval)) %>% 
  pivot_longer(cols = c('under15',`15_24`,`25_44`,`45_64`,`65+`),names_to='age.group', values_to = 'ctry_prop') %>% 
  group_by(sex,geo,age.group) %>% ### CORRECTION 23.11.2022
  mutate(ctry_lag1 = lag(ctry_prop, 1),
         ctry_lag2 = lag(ctry_prop, 2)) %>% 
  ungroup()


ctry_lvl_prop %>% 
  distinct(geo) %>% pull()

ISO_codes <- read.csv('./data/ctry_infos/all.csv') %>% 
  dplyr::select(alpha.2, country.code) %>% 
  filter(country.code != 438) %>% 
  dplyr::mutate(alpha.2 = dplyr::recode(alpha.2 , "GB"="UK" , "GR" = "EL"))

ISO.num.ctry <- ISO_codes[ISO_codes$alpha.2 %in% c(ctry_lvl_prop %>%  distinct(geo) %>% pull(),'EL','UK'),]$country.code 

# here we download the WIC data via a list 
wcde_raw_list <- lapply(as.character(seq(1, 5)), function(z) {
  result <- wcde::get_wcde('pop', 
                           scenario = z, 
                           pop_age = c('all'), 
                           pop_sex = 'total', 
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
  filter(age != 'All'&str_length(geo)==2)






age_grouping <- tibble(
  age = c("0--4","5--9","10--14","15--19","20--24","25--29","30--34","35--39","40--44","45--49","50--54","55--59","60--64","65--69","70--74","75--79","80--84","85--89","90--94","95--99","100+",'TOTAL'),
  age.group = fct_inorder(c(rep('under15',3),rep('15_24',2),rep('25_44',4),rep('45_64',4),rep('65+',8),'TOTAL'))
)

wcde_raw_ageGrouped <- 
  wcde_raw %>%
  group_by(geo, year, scenario) %>% 
  summarise(age = 'TOTAL',  # combine times
            pop = sum(pop)) %>%                  # get sum of sizes
  bind_rows(wcde_raw, .)   %>% 
  left_join(age_grouping, by = 'age') %>% 
  group_by(age.group,geo,year,scenario) %>% 
  dplyr::summarise(pop = sum(pop)*1000) %>% 
  ungroup() %>% 
  dplyr::filter(year > 2005 &  year <= 2100) %>% 
  mutate(interval = fct_inorder(paste(year,year+4,sep = '-'))) %>% 
  dplyr::select(-c(year)) %>% 
  pivot_wider(names_from = interval, values_from = pop) %>% 
  mutate(across(matches("-"), ~./`2020-2024`)) %>% 
  mutate(`2015-2019` = 1)

setdiff(str_sub(NUTS2_projection, start = 1, end = 2),unique(wcde_raw_ageGrouped$geo))


pop_estat_baseyear <- 
  demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups %>%
  dplyr::filter(sex == 'T', interval == '2020-2024') %>% 
  mutate(across(c( under15,`15_24`,`25_44`,`45_64`,`65+`), ~.*`TOTAL`)) %>% 
  
  rbind(UK_reconstructed20202024 %>% 
          dplyr::mutate(sex = "T", UNK = 0)) %>% 
  
  pivot_longer( cols = c( TOTAL,under15,`15_24`,`25_44`,`45_64`,`65+`), names_to = "age.group") %>% 
  dplyr::select(-c(sex,  UNK, interval)) %>% 
  rename(pop.estat = value)

# here it is very annoying that the data from the UK are actually not available at the moment. 
# now we have to adjust the nuts-2 pop total according to the baselineyear values from estat


pop_estat_baseyear_2intervals <- 
  demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups %>%  

  dplyr::filter(sex == 'T', interval %in% c('2010-2014','2015-2019','2020-2024')) %>% 
  
  rbind(UK_reconstructed20202024 %>% # here UK as a country is missing I think for 2020-2024 !!!
          dplyr::mutate(sex = "T", UNK = 0)) %>% 
  mutate(across(c( under15,`15_24`,`25_44`,`45_64`,`65+`), ~.*`TOTAL`)) %>% 
  pivot_longer( cols = c( TOTAL,under15,`15_24`,`25_44`,`45_64`,`65+`), names_to = "age.group") %>% 
  dplyr::select(-c(sex,  UNK)) %>% 
  rename(pop.estat = value) %>% 
  pivot_wider(names_from = 'interval', values_from = 'pop.estat', names_glue = '{.value}_{interval}')

pop_estat_baseyear_2intervals %>% 
  dplyr::filter(grepl('IT', geo)) 

# 
# wcde_raw_ageGrouped_adjusted_baseYear_Estat <- 
#   wcde_raw_ageGrouped %>% 
#   left_join(pop_estat_baseyear_2intervals, by = c('geo', 'age.group')) %>% 
#   mutate(across(c(`2015-2019`,`2020-2024`,`2025-2029`,`2030-2034`,`2035-2039`,`2040-2044`,`2045-2049`), ~.*`pop.estat_2015-2019`)) %>% 
#   mutate(`2010-2014` = `pop.estat_2010-2014`) %>% 
#   dplyr::select(-c(`pop.estat_2010-2014`,`pop.estat_2015-2019`))
# save(wcde_raw_ageGrouped_adjusted_baseYear_Estat,
#      file = '/Users/tambu/Library/Mobile Documents/com~apple~CloudDocs/A_pop_proj/pop_proj_R/01_Age_Structure/Variables_adjusted_baseLineYear/less_age_groups/wcde_raw_ageGrouped_adjusted_baseYear_Estat.RData')
# 


wcde_raw_ageGrouped_adjusted_baseYear_Estat <- 
  wcde_raw_ageGrouped %>% 
  left_join(pop_estat_baseyear_2intervals, by = c('geo', 'age.group')) %>% 
  mutate(across(c(`2020-2024`,`2025-2029`,`2030-2034`,`2035-2039`,`2040-2044`,`2045-2049`,
                  `2050-2054`,`2055-2059`,`2060-2064`  ,        
                 `2065-2069`,`2070-2074`,`2075-2079`   ,       
                 `2080-2084`,`2085-2089`,`2090-2094`    ,      
                   `2095-2099`, `2100-2104`  
                  
                  
                  ), ~.*`pop.estat_2020-2024`)) %>% 
  mutate(`2015-2019` = `pop.estat_2015-2019`) %>% 
  mutate(`2010-2014` = `pop.estat_2010-2014`) %>% 
  
  dplyr::select(-c(`pop.estat_2015-2019`,`pop.estat_2020-2024`))



# save(wcde_raw_ageGrouped_adjusted_baseYear_Estat, file = './data/GAO_wcde_adjusted_2024/wcde_raw_ageGrouped_adjusted_baseYear_Estat.RData')
load('./DataGao/Europe_tot_NUTS2_result_pop_list_raw_SSP12345.rData')

# code adjusted from Norway ----
## a) population ----
 Gao_adjusted_baseYear_NUTS2_pop_sizes_SSPS <- 
  
  result_pop_list[[1]] %>% 
  left_join(result_pop_list[[2]], by = 'NUTS_ID' ) %>% 
  left_join(result_pop_list[[3]], by = 'NUTS_ID' ) %>% 
  left_join(result_pop_list[[4]], by = 'NUTS_ID' ) %>% 
  left_join(result_pop_list[[5]], by = 'NUTS_ID' ) %>% 
  tibble() %>% 
  rename(geo = NUTS_ID) %>% 
  pivot_longer(names_to = 'period', cols = matches('total'), values_to = 'pop.tot.NUTS_ID.Gao') %>% 
  dplyr::mutate(
    scenario = as.factor(str_sub(period,start = 1,end = 4)),
    period =  as.numeric(str_sub(period,start = str_length(period)-3))
  ) %>% 
  complete(geo,scenario, period = seq(2010,2100, by = 5)) %>% 
  group_by(geo, scenario) %>% 
  mutate(pop.tot.NUTS_ID.Gao = zoo::na.approx(pop.tot.NUTS_ID.Gao, na.rm = FALSE)) %>% 
  ungroup() %>% 
  dplyr::mutate(period = paste(period, period+4, sep = '-')) %>% 
  dplyr::mutate(ctry = str_sub(geo,1,2)) %>% 
  dplyr::group_by(scenario, period, ctry) %>% 
  dplyr::mutate(tot_pop = sum(pop.tot.NUTS_ID.Gao)) %>% 
  ungroup() %>% 
  dplyr::mutate(pop.prop.tot = pop.tot.NUTS_ID.Gao/tot_pop) %>% 
  dplyr::select(-c(pop.tot.NUTS_ID.Gao, tot_pop)) %>% 
  left_join(
  
  
  wcde_raw_ageGrouped_adjusted_baseYear_Estat %>%
  dplyr::select(-"pop.estat_2010-2014") %>% 
  dplyr::filter(age.group == 'TOTAL') %>% 
  dplyr::mutate(scenario = as.factor(paste0('ssp',scenario))) %>% 
  pivot_longer(names_to = 'period', cols = -c('age.group', 'geo', 'scenario'), values_to = 'pop.tot.ctry_wcde'),
  by = c('scenario', 'period', 'ctry'='geo')) %>% 
  dplyr::mutate(pop.tot.NUTS_ID.Gao = pop.tot.ctry_wcde*pop.prop.tot) %>% 
  dplyr::select(c( 'geo', 'scenario', 'period', 'pop.tot.NUTS_ID.Gao')) %>% 
  
  

  
  
  
#   # A tibble: 31,540 × 4
#   geo   scenario period    pop.tot.NUTS_ID.Gao
# <chr> <fct>    <chr>                   <dbl>
#   1 AL01  ssp1     2010-2014             845310.
# 2 AL01  ssp1     2015-2019             844550.
# 3 AL01  ssp1     2020-2024             843791.
# 4 AL01  ssp1     2025-2029             835910.
# 5 AL01  ssp1     2030-2034             828028.
# 6 AL01  ssp1     2035-2039             815265.
# 7 AL01  ssp1     2040-2044             802503.
# 8 AL01  ssp1     2045-2049             785278.
  
  
  
  
  pivot_wider(values_from = `pop.tot.NUTS_ID.Gao`, names_from = c('scenario','period'),names_glue = '{scenario}_{.value}_{period}') %>% 
  # dplyr::select(matches(".NUTS_ID.Gao_2015-2019"))
  mutate(across(contains("ssp1_"), ~./`ssp1_pop.tot.NUTS_ID.Gao_2020-2024`),
         across(contains("ssp2_"), ~./`ssp2_pop.tot.NUTS_ID.Gao_2020-2024`),
         across(contains("ssp3_"), ~./`ssp3_pop.tot.NUTS_ID.Gao_2020-2024`),
         across(contains("ssp4_"), ~./`ssp4_pop.tot.NUTS_ID.Gao_2020-2024`),
         across(contains("ssp5_"), ~./`ssp5_pop.tot.NUTS_ID.Gao_2020-2024`)
  ) %>% 
  # mutate(across(contains("pop.tot.NUTS_ID.Gao_2015-2019"), ~ 1)) %>% 
  
  inner_join(
    demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups %>% 
      dplyr::filter(sex == 'T') %>% 
      dplyr::select(-sex) %>% 
      dplyr::select(interval,geo,TOTAL) %>% 
      distinct() %>% rbind(
      UK_reconstructed20202024 %>% 
        dplyr::select(TOTAL, geo) %>% 
      dplyr::mutate(interval = '2020-2024')) %>% 
      dplyr::filter(interval %in%  c('2010-2014','2015-2019','2020-2024')) %>% 
      pivot_wider(values_from = `TOTAL`, names_from = 'interval',names_glue = '{.value}_EStat_{interval}'),
    by = 'geo') %>% 
  mutate(across(matches(c("pop.tot.NUTS_ID.Gao_2015-2019", "pop.tot.NUTS_ID.Gao_2020-2024", "pop.tot.NUTS_ID.Gao_2025-2029",
                          "pop.tot.NUTS_ID.Gao_2030-2034", "pop.tot.NUTS_ID.Gao_2035-2039","pop.tot.NUTS_ID.Gao_2040-2044",
                          "pop.tot.NUTS_ID.Gao_2045-2049", "pop.tot.NUTS_ID.Gao_2050-2054", "pop.tot.NUTS_ID.Gao_2055-2059",
                          "pop.tot.NUTS_ID.Gao_2060-2064",
                          "pop.tot.NUTS_ID.Gao_2065-2069", "pop.tot.NUTS_ID.Gao_2070-2074","pop.tot.NUTS_ID.Gao_2075-2079",
                          "pop.tot.NUTS_ID.Gao_2080-2084",
                          "pop.tot.NUTS_ID.Gao_2085-2089", "pop.tot.NUTS_ID.Gao_2090-2094","pop.tot.NUTS_ID.Gao_2095-2099",
                          "pop.tot.NUTS_ID.Gao_2100-2104"
                          
  )), ~.*`TOTAL_EStat_2020-2024`)) %>% 
  mutate( across(matches('pop.tot.NUTS_ID.Gao_2010-2014'),  ~.*`TOTAL_EStat_2010-2014`)) %>% 
  dplyr::select(geo, matches('.Gao')) %>%
  rename_with(~ stringr::str_replace(.x, 
                                     pattern = "Gao", 
                                     replacement = "Gao_adj"), 
              matches("Gao")) 
save(
Gao_adjusted_baseYear_NUTS2_pop_sizes_SSPS,
file = './data/GAO_wcde_adjusted_2024/Gao_adjusted_baseYear_NUTS2_pop_sizes_SSPS.RData'
)


# 
# Gao_adjusted_baseYear_NUTS2_pop_sizes_SSPS %>% 
#   dplyr::select(geo,"ssp1_pop.tot.NUTS_ID.Gao_adj_2100-2104", "ssp3_pop.tot.NUTS_ID.Gao_adj_2100-2104") %>% 
#   dplyr::filter(grepl('IT', geo)) %>% 
#   summarise(sum(`ssp1_pop.tot.NUTS_ID.Gao_adj_2100-2104`)) 
# 
# 




## check on the tot pop ----

result_pop_list[[1]] %>% 
  left_join(result_pop_list[[2]], by = 'NUTS_ID' ) %>% 
  left_join(result_pop_list[[3]], by = 'NUTS_ID' ) %>% 
  left_join(result_pop_list[[4]], by = 'NUTS_ID' ) %>% 
  left_join(result_pop_list[[5]], by = 'NUTS_ID' ) %>% 
  tibble() %>% 
  rename(geo = NUTS_ID) %>% 
  pivot_longer(names_to = 'period', cols = matches('total'), values_to = 'pop.tot.NUTS_ID.Gao') %>% 
  dplyr::mutate(
    scenario = as.factor(str_sub(period,start = 1,end = 4)),
    period =  as.numeric(str_sub(period,start = str_length(period)-3))
  ) %>% 
  complete(geo,scenario, period = seq(2010,2100, by = 5)) %>% 
  group_by(geo, scenario) %>% 
  mutate(pop.tot.NUTS_ID.Gao = zoo::na.approx(pop.tot.NUTS_ID.Gao, na.rm = FALSE)) %>% 
  ungroup() %>% 
  dplyr::mutate(period = paste(period, period+4, sep = '-')) %>% 
  pivot_wider(values_from = `pop.tot.NUTS_ID.Gao`, 
              names_from = c('scenario','period'),
              names_glue = '{scenario}_{.value}_{period}') %>% 
  dplyr::select(geo,"ssp1_pop.tot.NUTS_ID.Gao_2100-2104", "ssp3_pop.tot.NUTS_ID.Gao_2100-2104") %>% 
  dplyr::filter(grepl('IT', geo)) %>% 
  summarise(sum(`ssp3_pop.tot.NUTS_ID.Gao_2100-2104`)) 





## b) urbanization ----

load('./DataGao/Europe_tot_NUTS2_result_urb_list_raw_SSP12345.rData')
result_urb_list[[1]] %>% head()



urban_land_fraction_KOMMUNE_SSPs12345_period <- 
  
  result_urb_list[[1]]  %>% tibble() %>% 
  mutate(scenario = 'SSP1') %>% 
  rbind(
    result_urb_list[[2]]  %>% tibble() %>% 
      mutate(scenario = 'SSP2')) %>% 
  rbind(
    result_urb_list[[3]]  %>% tibble() %>% 
      mutate(scenario = 'SSP3')) %>% 
  rbind(
    result_urb_list[[4]]  %>% tibble() %>% 
      mutate(scenario = 'SSP4')) %>% 
  rbind(
    result_urb_list[[5]]  %>% tibble() %>% 
      mutate(scenario = 'SSP5')) %>% 
  tibble() %>% 
  rename(geo = NUTS_ID) %>% 
  
  rename("2010" = "Band_1.1","2020" = "Band_1.2","2030" = "Band_1.3","2040" = "Band_1.4", "2050" = "Band_1.5",
         "2060"  = "Band_1.6", "2070" = "Band_1.7", "2080" = "Band_1.8", "2090" = "Band_1.9", "2100" = "Band_1.10") %>% 
  
  pivot_longer(names_to = 'period', cols = -c('geo', 'scenario'), values_to = 'urb_frac') %>% 
  dplyr::mutate(
    period = as.numeric(period)) %>% 
  complete(geo,scenario, period = seq(2010,2100, by = 5)) %>% 
  group_by(geo, scenario) %>% 
  mutate(urb_frac = zoo::na.approx(urb_frac, na.rm = FALSE)) %>% 
  ungroup() %>% 
  dplyr::mutate(period = paste(period, period+4, sep = '-')) 



save(urban_land_fraction_KOMMUNE_SSPs12345_period,
     file = './data/GAO_wcde_adjusted_2024/urban_land_fraction_KOMMUNE_SSPs12345_period.RData'
)



## check on the urbanisation ----


result_urb_list[[1]]  %>% tibble() %>% 
  mutate(scenario = 'SSP1') %>% 
  rbind(
    result_urb_list[[2]]  %>% tibble() %>% 
      mutate(scenario = 'SSP2')) %>% 
  rbind(
    result_urb_list[[3]]  %>% tibble() %>% 
      mutate(scenario = 'SSP3')) %>% 
  rbind(
    result_urb_list[[4]]  %>% tibble() %>% 
      mutate(scenario = 'SSP4')) %>% 
  rbind(
    result_urb_list[[5]]  %>% tibble() %>% 
      mutate(scenario = 'SSP5')) %>% 
  tibble() %>% 
  rename(geo = NUTS_ID) %>% 
  
  rename("2010" = "Band_1.1","2020" = "Band_1.2","2030" = "Band_1.3","2040" = "Band_1.4", "2050" = "Band_1.5",
         "2060"  = "Band_1.6", "2070" = "Band_1.7", "2080" = "Band_1.8", "2090" = "Band_1.9", "2100" = "Band_1.10") %>% 
  
  pivot_longer(names_to = 'period', cols = -c('geo', 'scenario'), values_to = 'urb_frac') %>% 
  dplyr::mutate(
    period = as.numeric(period)) %>% 
  complete(geo,scenario, period = seq(2010,2100, by = 5)) %>% 
  group_by(geo, scenario) %>% 
  mutate(urb_frac = zoo::na.approx(urb_frac, na.rm = FALSE)) %>% 
  ungroup() %>% 
  dplyr::mutate(period = paste(period, period+4, sep = '-'))  %>% 
  pivot_wider(values_from = `urb_frac`, 
              names_from = c('scenario','period'),
              names_glue = '{scenario}_{.value}_{period}') %>% 
  dplyr::select(geo,"SSP1_urb_frac_2100-2104", "SSP3_urb_frac_2100-2104") %>% 
  dplyr::filter(grepl('IT', geo)) %>% 
  summarise(mean(`SSP3_urb_frac_2100-2104`)) 



# general checks ----

urbanisation_data <-  
  result_urb_list[[1]]  %>% tibble() %>% 
  mutate(scenario = 'SSP1') %>% 
  rbind(
    result_urb_list[[2]]  %>% tibble() %>% 
      mutate(scenario = 'SSP2')) %>% 
  rbind(
    result_urb_list[[3]]  %>% tibble() %>% 
      mutate(scenario = 'SSP3')) %>% 
  rbind(
    result_urb_list[[4]]  %>% tibble() %>% 
      mutate(scenario = 'SSP4')) %>% 
  rbind(
    result_urb_list[[5]]  %>% tibble() %>% 
      mutate(scenario = 'SSP5')) %>% 
  tibble() %>% 
  rename(geo = NUTS_ID) %>% 
  
  rename("2010" = "Band_1.1","2020" = "Band_1.2","2030" = "Band_1.3","2040" = "Band_1.4", "2050" = "Band_1.5",
         "2060"  = "Band_1.6", "2070" = "Band_1.7", "2080" = "Band_1.8", "2090" = "Band_1.9", "2100" = "Band_1.10") %>% 
  
  pivot_longer(names_to = 'period', cols = -c('geo', 'scenario'), values_to = 'urb_frac') %>% 
  dplyr::mutate(
    period = as.numeric(period)) %>% 
  complete(geo,scenario, period = seq(2010,2100, by = 5)) %>% 
  group_by(geo, scenario) %>% 
  mutate(urb_frac = zoo::na.approx(urb_frac, na.rm = FALSE)) %>% 
  ungroup() %>% 
  dplyr::mutate(period = paste(period, period+4, sep = '-')) %>% 
  dplyr::mutate(ctry = str_sub(geo,1,2))


 population_data <- 
  
  result_pop_list[[1]] %>% 
  left_join(result_pop_list[[2]], by = 'NUTS_ID' ) %>% 
  left_join(result_pop_list[[3]], by = 'NUTS_ID' ) %>% 
  left_join(result_pop_list[[4]], by = 'NUTS_ID' ) %>% 
  left_join(result_pop_list[[5]], by = 'NUTS_ID' ) %>% 
  tibble() %>% 
  rename(geo = NUTS_ID) %>% 
  pivot_longer(names_to = 'period', cols = matches('total'), values_to = 'pop.tot.NUTS_ID.Gao') %>% 
  dplyr::mutate(
    scenario = as.factor(str_sub(period,start = 1,end = 4)),
    period =  as.numeric(str_sub(period,start = str_length(period)-3))
  ) %>% 
  complete(geo,scenario, period = seq(2010,2100, by = 5)) %>% 
  group_by(geo, scenario) %>% 
  mutate(pop.tot.NUTS_ID.Gao = zoo::na.approx(pop.tot.NUTS_ID.Gao, na.rm = FALSE)) %>% 
  ungroup() %>% 
  dplyr::mutate(period = paste(period, period+4, sep = '-')) %>% 
  dplyr::mutate(scenario = paste0('SSP', str_sub(scenario,-1))) %>% 
  dplyr::mutate(ctry = str_sub(geo,1,2))
 
# some plot checks ----

#  population_data %>% 
#    dplyr::group_by(scenario, period, ctry) %>% 
#    dplyr::mutate(ctry_pop = sum(pop.tot.NUTS_ID.Gao)) %>% 
#    dplyr::mutate(pop_prop_ctry = pop.tot.NUTS_ID.Gao/ ctry_pop) %>% 
#    dplyr::filter(ctry == "AT") %>% 
#    dplyr::mutate(period = as.factor(period)) %>% 
#    dplyr::mutate(scenario = as.factor(scenario)) %>% 
#    ggplot(aes(x = period, y = pop_prop_ctry, color = geo))+
#    geom_line(aes(group = interaction(geo, scenario)))+
#    facet_wrap(~scenario)
#    
# 
# 
#  
#  population_data %>% 
#    dplyr::group_by(scenario, period, ctry) %>% 
#    dplyr::mutate(ctry_pop = sum(pop.tot.NUTS_ID.Gao)) %>% 
#    dplyr::mutate(pop_prop_ctry = pop.tot.NUTS_ID.Gao/ ctry_pop) %>% 
#    ungroup() %>% 
#    dplyr::filter(period == "2100-2104") %>% 
#    dplyr::filter(ctry == "AT") %>% 
#    dplyr::mutate(period = as.factor(period)) %>% 
#    dplyr::mutate(scenario = as.factor(scenario)) %>% 
#    ggplot(aes(x = scenario, y = pop_prop_ctry, color = geo))+
#    geom_line(aes(group = interaction(geo)))
#  
# 
#  
#  
#  urbanisation_data %>% 
#    dplyr::filter(ctry == "IT") %>% 
#    dplyr::mutate(period = as.factor(period)) %>% 
#    dplyr::mutate(scenario = as.factor(scenario)) %>% 
#    ggplot(aes(x = period, y = urb_frac, color = geo))+
#    geom_line(aes(group = interaction(geo, scenario)))+
#    facet_wrap(~scenario)
#  
#  
#  
#  urbanisation_data %>% 
#    dplyr::filter(period == "2100-2104") %>% 
#    dplyr::filter(ctry == "AT") %>% 
#    dplyr::mutate(period = as.factor(period)) %>% 
#    dplyr::mutate(scenario = as.factor(scenario)) %>% 
#    ggplot(aes(x = scenario, y = urb_frac, color = geo))+
#    geom_line(aes(group = interaction(geo)))
#  
# 
# 
#  
#  population_data %>% 
#    dplyr::group_by(scenario, period, ctry) %>% 
#    dplyr::mutate(ctry_pop = sum(pop.tot.NUTS_ID.Gao)) %>% 
#    dplyr::mutate(pop_prop_ctry = pop.tot.NUTS_ID.Gao/ ctry_pop) %>% 
#    ungroup() %>% 
#    inner_join(urbanisation_data, by = c('geo', 'scenario', 'period', 'ctry')) %>% 
#    dplyr::filter(ctry == "IT") %>% 
#    dplyr::mutate(period = as.factor(period)) %>% 
#    dplyr::filter(period == "2100-2104") %>% 
#    dplyr::mutate(scenario = as.factor(scenario)) %>% 
#    ggplot(aes(x = pop_prop_ctry*100, y = urb_frac*100, color = geo))+
#    geom_point(aes(group = interaction(geo, scenario)))+
#    facet_wrap(~scenario)
#  # there is clearly a correlation but it is maybe not as precise as I thought it woudl be.
#  
#  
#  
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 








