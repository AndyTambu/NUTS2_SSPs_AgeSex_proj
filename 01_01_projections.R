# intro and libraries ----
library('neuralnet')
library('keras')
library('tensorflow')
library(sf)
library(raster)
library(tidyverse)
library(zoo)
library(raster)
library('rgdal')
library(sp)
library(magrittr)
library(spatialRF)
library(kableExtra)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(randomForestExplainer)
library(pdp)
library(rgeos)
library(viridis)
library(data.table)
library(ggplot2)
library(car)
library(textshape)
library(keras)
library(rsample)
library(tidymodels)
library(recipes)
library(Metrics)
library(brulee)


# 1. loading files ----
## 1.1 geo and database files ----


load(file = './data/GAO_wcde_adjusted_2024/urban_land_fraction_KOMMUNE_SSPs12345_period.RData')

load(file = './data/GAO_wcde_adjusted_2024/Gao_adjusted_baseYear_NUTS2_pop_sizes_SSPS.RData')

load(file = './data/GAO_wcde_adjusted_2024/wcde_raw_ageGrouped_adjusted_baseYear_Estat.RData')

load(file = './data/dep_variables/demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups_with2024.RData')

load(file = "./data/GAO_wcde_adjusted_2024/NUTS2_projection.RData")

Gao_adjusted_baseYear_NUTS2_pop_sizes_SSPS %>% 
  colnames()




dataset_eur_raw_melted_noSexnoEdu_intervals <- 
  demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups %>% 
  dplyr::filter(str_length(geo)==4) %>% 
  dplyr::filter(sex == 'T') %>% 
  dplyr::select(-c(sex, UNK)) %>% 
  rbind(UK_reconstructed20202024) # here I added the values from the previous file about the nuts section

# dataset_eur_raw_melted_noSexnoEdu_intervals %>% 
#   dplyr::filter(grepl('UK', geo)) %>% View()

dataset_eur_raw_melted_noSexnoEdu_intervals %>% 
  dplyr::mutate(ctry = str_sub(geo,1,2)) %>% 
  dplyr::filter(interval == "2020-2024"&ctry != "TR") %>% 
  dplyr::distinct(geo) %>% 
  pull()

wcde_raw_ageGrouped_adjusted_baseYear_Estat <- 
  wcde_raw_ageGrouped_adjusted_baseYear_Estat %>% 
  pivot_longer(cols = c(starts_with("20"),"2100-2104") , 
               names_to = "period", 
               values_to = "value") %>%  # Convert wide years into long format
  mutate(period = paste0("SSP", scenario, "_", period)) %>%  # Rename period with SSP
  dplyr::select(-scenario) %>% 
  pivot_wider(names_from = period, values_from = value) 


wcde_raw_ageGrouped_adjusted_baseYear_Estat %>% 
  dplyr::select(age.group,contains('2020-2024'), contains('2025-2029'))
Gao_adjusted_baseYear_NUTS2_pop_sizes_SSPS %>%
  dplyr::select(contains('2010-2014'))



ages <- c('under15', '15_24', '25_44', '45_64', '65plus')
for (j in ages) {
  print(paste0("Sieve_fit_brulee_first_test_corct_funcs_", j,'_test20022025_corrStu.RData'))
  load(paste0("./results/Sieve_fit_brulee_first_test_corct_funcs_", j,'_test20022025_corrStu.RData'))
}
# 
# [1] "Sieve_fit_brulee_first_test_corct_funcs_under15_test20022025_corrStu.RData"
# [1] "Sieve_fit_brulee_first_test_corct_funcs_15_24_test20022025_corrStu.RData"
# [1] "Sieve_fit_brulee_first_test_corct_funcs_25_44_test20022025_corrStu.RData"
# [1] "Sieve_fit_brulee_first_test_corct_funcs_45_64_test20022025_corrStu.RData"
# [1] "Sieve_fit_brulee_first_test_corct_funcs_65plus_test20022025_corrStu.RData"


standardize_cols <- function(df, cols) {
  for (col in cols) {
    # Apply scale and convert the result back to a vector
    df[[col]] <- as.vector(scale(df[[col]]))  # Ensure the result is a vector
  }
  return(df)
}



# 
# load('./ML_data/train_2000.2004_1995.1999_ctry.RData')
# density(train_2000.2004_1995.1999_ctry$lagged_quot_neigh) %>% plot()

# list.files('./00_NewAgeGroups/data/ML_data')
## 1.2 train and test data load ----

myFiles <- paste0('./data/train&test/',list.files('./data/train&test/'))

for (i in myFiles) {
  load(i)
}


names(train_2000.2004_1995.1999_ctry) <- gsub("\\+", "", names(train_2000.2004_1995.1999_ctry))
names(train_2005.2009_2000.2004_ctry) <- gsub("\\+", "", names(train_2005.2009_2000.2004_ctry))
names(train_2010.2014_2005.2009_ctry) <- gsub("\\+", "", names(train_2010.2014_2005.2009_ctry))
names(train_2015.2019_2010.2014_ctry) <- gsub("\\+", "", names(train_2015.2019_2010.2014_ctry))
names(test_2020.2024_2015.2019_ctry) <- gsub("\\+", "", names(test_2020.2024_2015.2019_ctry))


# 
# train_2000.2004_1995.1999_ctry %>% 
#   left_join(second_try_clustering, by = 'geo') %>% 
#   dplyr::group_by(cluster) %>% 
#   dplyr::summarise(n = n_distinct(geo))


# load('./data/second_try_clustering.RData')

load(file = './data/cluster_df_hierarchical_PCA_wardD.RData')
load('./dataGao/cropped/urban_land_fraction_NUTS2_SSP2_period.RData')
# dir.create("results"). # to be run on the first run

second_try_clustering <- cluster_df_hierarchical_PCA_wardD

second_try_clustering %>% 
  distinct(cluster)
urban_land_fraction_NUTS2_SSP2_period

train_2000.2004_1995.1999_ctry <-train_2000.2004_1995.1999_ctry %>% 
  left_join(second_try_clustering, by = 'geo') %>% 
  mutate(cluster = as.character(cluster)) %>% 
  fastDummies::dummy_cols('cluster') %>% 
  left_join(urban_land_fraction_NUTS2_SSP2_period %>% 
              filter(period == '2000-2004'), by = 'geo')


train_2005.2009_2000.2004_ctry <- train_2005.2009_2000.2004_ctry %>% 
  left_join(second_try_clustering, by = 'geo') %>% 
  mutate(cluster = as.character(cluster)) %>% 
  fastDummies::dummy_cols('cluster') %>% 
  left_join(urban_land_fraction_NUTS2_SSP2_period %>% 
              filter(period == '2005-2009'), by = 'geo')

train_2010.2014_2005.2009_ctry <- train_2010.2014_2005.2009_ctry %>% 
  left_join(second_try_clustering, by = 'geo') %>% 
  mutate(cluster = as.character(cluster)) %>% 
  fastDummies::dummy_cols('cluster') %>% 
  left_join(urban_land_fraction_NUTS2_SSP2_period %>% 
              filter(period == '2010-2014'), by = 'geo')

train_2015.2019_2010.2014_ctry <- train_2015.2019_2010.2014_ctry %>% 
  left_join(second_try_clustering, by = 'geo') %>% 
  mutate(cluster = as.character(cluster)) %>% 
  fastDummies::dummy_cols('cluster') %>% 
  left_join(urban_land_fraction_NUTS2_SSP2_period %>% 
              filter(period == '2015-2019'), by = 'geo')


test_2020.2024_2015.2019_ctry <- test_2020.2024_2015.2019_ctry %>% 
  left_join(second_try_clustering, by = 'geo') %>% 
  mutate(cluster = as.character(cluster)) %>% 
  fastDummies::dummy_cols('cluster') %>% 
  left_join(urban_land_fraction_NUTS2_SSP2_period %>% 
              filter(period == '2020-2024'), by = 'geo')



urban_land_fraction_NUTS2_SSP2_period %>% View()




periods <- paste(seq(2005, 2100, by = 5), seq(2009, 2104, by = 5),sep = "-")


# the for loop ----
# 
for (z in seq(1,5)) {
#  for (z in c(1)) {
    
    print(z)
  proj_res <-   dataset_eur_raw_melted_noSexnoEdu_intervals %>%     
    dplyr::select(-c('TOTAL')) %>% 
    dplyr::filter(!is.na(under15)) %>% 
    dplyr::mutate(ctry = str_sub(geo,1,2))
  
  
  
  # wcde_raw_ageGrouped_adjusted_baseYear_NorStatOffice
  wcde_raw_ageGrouped_adjusted <- 
    wcde_raw_ageGrouped_adjusted_baseYear_Estat %>%  
    dplyr::select(age.group, geo,contains(paste0('SSP',z, '_', periods)))
  
  
  ctry_lvl_prop_proj <- 
    wcde_raw_ageGrouped_adjusted %>%   
    rename(ctry = geo) %>% 
    mutate(across(matches("-"), ~as.numeric(.x)))  %>% 
    mutate(across(matches("-"), ~.x/.x[age.group == 'TOTAL']))  %>% 
    ungroup() %>% 
    dplyr::filter(age.group != 'TOTAL') %>% 
    pivot_longer(values_to = "ctry_prop", cols = matches("-"),names_to = 'interval' ) %>% 
    # adjustement fro loop SSPs
    
    dplyr::mutate(interval = str_sub(interval, start = 6)) %>% 
    
    group_by(age.group) %>% ### CORRECTION 23.11.2022
    # you need to add also by geo in case you have more countries 
    mutate(interval = fct_inorder(interval),
           ctry_lag1 = lag(ctry_prop),
           ctry_lag2 = lag(ctry_lag1)) %>% 
    arrange(interval) %>% 
    ungroup()
  
  
  
  
  # NOTE: we leave some NUTS2 in the notation to save time 
  
  
  for (i in 5:length(periods)){ 
    print(i)
    
    nuts2_vs_ctry_proj  <-  # this has to contain the information which before where in dep_vrbl
      proj_res %>% 
      pivot_longer(cols = c('under15',`15_24`,`25_44`,`45_64`,`65+`),names_to='age.group', values_to = 'nuts2_prop') %>% 
      inner_join(ctry_lvl_prop_proj %>% 
                   # dplyr::select(-c(ctry_lag1,ctry_lag2,geo))  ,
                   
                   dplyr::select(-c(ctry_lag1,ctry_lag2))  ,
                 by = c('ctry', 'age.group','interval')) %>% 
      mutate(interval = factor(interval, levels = c(paste(seq(1990, 2100,by = 5), seq(1994,2104, by = 5), sep = "-")))) %>% 
      mutate(quot = nuts2_prop/ctry_prop) %>% #, diff = nuts2_prop-ctry_prop) %>% 
      group_by(geo,age.group) %>% 
      # mutate(diff_1 = lag(diff),
      #        quot_1 = lag(quot)) %>% 
      # ungroup()
      mutate(lag_nuts2 = lag(nuts2_prop,1),
             lag_ctry = lag(ctry_prop,1)) %>% 
      mutate(diff_reg = nuts2_prop-lag(nuts2_prop,1),
             diff_ctry = ctry_prop-lag(ctry_prop,1)) %>% 
      mutate(quot = nuts2_prop/ctry_prop, diff = nuts2_prop-ctry_prop) %>% 
      mutate(quot_diff  = diff_reg/diff_ctry) %>% 
      mutate(lagged_quot  = lag(quot,1)) %>% 
      mutate(lagged_quot_diff  = lag(quot_diff,1)) %>% 
      ungroup() 
    #   na.omit(nuts2_vs_ctry_proj$lagged_quot_diff) 
    
    # lagged_quot_neigh <- 
    #   regions_neighbours %>% 
    #   left_join(nuts2_vs_ctry_proj %>% 
    #               dplyr::select(interval, geo, age.group, lagged_quot,lagged_quot_diff), by = c('neighbours' = 'geo')) %>% 
    #   group_by(region, interval,age.group) %>% 
    #   summarise(lagged_quot_neigh = mean(lagged_quot), 
    #             lagged_quot_diff_neigh = mean(lagged_quot_diff)) %>% 
    #   rename(geo = region) %>% 
    #   
    #   ungroup()
    # 
    # lagged_quot_neigh %>% 
    #   dplyr::filter(interval == '2015-2019')
    
    
    # dep variable ####
    # dep_vrbl <- 
    #   demo_r_pjangroup_ageGroups_timeIntervals_dep1_lessAgeGroups %>% 
    #   inner_join(ctry_lvl_prop %>% 
    #                dplyr::select(-c(ctry_lag1,ctry_lag2)), 
    #              by = c( 'age.group','interval')) %>% 
    #   group_by(age.group,geo) %>% 
    #   mutate(lag_nuts2 = lag(nuts2_prop,1),
    #          lag_ctry = lag(ctry_prop,1)) %>% 
    #   mutate(diff_reg = nuts2_prop-lag(nuts2_prop,1),
    #          diff_ctry = ctry_prop-lag(ctry_prop,1)) %>% 
    #   mutate(quot = nuts2_prop/ctry_prop, diff = nuts2_prop-ctry_prop) %>% 
    #   mutate(quot_diff  = diff_reg/diff_ctry) %>% 
    #   mutate(lagged_quot  = lag(quot,1)) %>% 
    #   mutate(lagged_quot_diff  = lag(quot_diff,1)) %>% 
    #   ungroup() 
    # 
    
    
    
    ctry_spec_ind_var_proj <- 
      
      ctry_lvl_prop_proj %>%
      mutate(lag1_ctry_prop = ctry_lag1,
             lag2_ctry_prop = ctry_lag2,
             lag1_ctry_pop_g = lag1_ctry_prop/lag2_ctry_prop) %>% 
      dplyr::select(-c(ctry_lag1,ctry_lag2 )) %>% 
      ungroup() %>% 
      mutate(age = age.group) %>% 
      pivot_wider(names_from = age,  values_from = c(
        'ctry_prop', 'lag1_ctry_prop', 'lag2_ctry_prop', 'lag1_ctry_pop_g'),
        names_glue = "{.value}_{age}") %>% 
      
      # it is a MULTI county model here
      group_by(ctry, interval) %>% 
      mutate_at(vars(contains(c(
        'ctry_prop', 'lag1_ctry_prop', 'lag2_ctry_prop', 'lag1_ctry_pop_g'))), ~mean(.,na.rm = TRUE)) %>% 
      ungroup()
    

    nuts_vs_ctry_ind_var  <- 
      nuts2_vs_ctry_proj %>% 
      group_by(geo,age.group) %>% 
      mutate(lag1_quot = quot,
             lag2_quot = lag(quot,1)) %>% 
      ungroup() %>% 
      dplyr::select(-c(nuts2_prop)) %>% 
      mutate(age = age.group) %>% 
      pivot_wider(names_from = age,  values_from = c(
        'quot', 'lag1_quot', 'lag2_quot'),
        names_glue = "{.value}_{age}") %>% 
      group_by(geo, interval) %>% 
      mutate_at(vars(contains(c(
        'quot', 'lag1_quot', 'lag2_quot'))), ~mean(.,na.rm = TRUE)) %>% 
      ungroup()
    
    
    
    to_be_proj  <- 
      proj_res %>%  #filter(interval == "2025-2029")
      right_join(
        Gao_adjusted_baseYear_NUTS2_pop_sizes_SSPS %>% 
          # filter(str_detect(geo, 'UK')) 
          dplyr::mutate(ctry = str_sub(geo, start = 1, end = 2)) %>% 
          # filter(!str_detect(geo, 'UK')) %>% 
          
          dplyr::select(geo,ctry,contains(paste0('ssp',z))&contains(periods)) %>% 
          gather(interval, TOTAL,-c(ctry,geo)) %>% 
          mutate(interval = fct_inorder(str_sub(interval,start = str_length(interval)-8)))
        
        , by = c('ctry','geo', 'interval')
      ) %>% 
      gather(age.group, prop, c('under15',`15_24`,`25_44`,`45_64`,`65+`)) %>% 
      mutate(abs.value = TOTAL * prop) %>% 
      filter(interval %in% c(periods[i-2],periods[i-1],periods[i])) %>%
      mutate( interval = ifelse(interval == periods[i-2],'period_2', ifelse(interval == periods[i-1], 'period_1', 'period')))  %>%    # with this we can assign the variable a name in a functional way 
      mutate(interval = factor(interval, levels = c('period_2','period_1','period'))) %>% 
      ungroup() %>% 
      pivot_wider(names_from = interval, values_from = c(prop,abs.value,TOTAL)) %>%
      #mutate(y = `prop_2005-2009` - `prop_2000-2004`) %>%  
      mutate(#`tot_pop_NUTS2_1990-1994` = sum(`abs.value_1990-1994`),
        `tot_pop_NUTS2_periods_2` = TOTAL_period_2,
        `tot_pop_NUTS2_periods_1` = TOTAL_period_1,
        `tot_pop_NUTS2_periods` = TOTAL_period) %>% 
      
      ungroup() %>% 
      mutate(g.pop.NUTS2 = `tot_pop_NUTS2_periods`/`tot_pop_NUTS2_periods_1`) %>%    # growth in total NUTS2 region. This taken coherently with the year of the# here I follow the order of what was defined by Erich in the supporting material
      mutate(`lag1_` = `prop_period_1`) %>% 
      mutate(`lag2_` = `prop_period_2`) %>% 
      mutate(`lag1_g_` = `prop_period_1` / `prop_period_2`) %>% 
      mutate(lag1.tot_pop_NUTS2 = `tot_pop_NUTS2_periods_1`) %>% 
      mutate(lag2.tot_pop_NUTS2 = `tot_pop_NUTS2_periods_2`) %>% 
      mutate(lag1ratio.tot_pop_NUTS2 = `tot_pop_NUTS2_periods_1`/ `tot_pop_NUTS2_periods_2`) %>% 
      # mutate(lag2ratio.tot_pop_NUTS2 = `tot_pop_NUTS2_1995-1999` / `tot_pop_NUTS2_1990-1994`) %>% 
      dplyr::select("geo","age.group","g.pop.NUTS2",
                    "lag1_","lag2_","lag1_g_",                
                    "lag1.tot_pop_NUTS2","lag2.tot_pop_NUTS2",
                    "lag1ratio.tot_pop_NUTS2", ctry)%>%
      mutate(age = age.group) %>% 
      pivot_wider(names_from = age, values_from = c("lag1_","lag2_","lag1_g_") , names_glue = "{.value}{age}", values_fill = ) %>% 
      group_by(geo) %>% 
      mutate_at(vars(contains(c("lag1_","lag2_","lag1_g_"))), ~mean(.,na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(ctry = str_sub(geo, start = 1, end = 2)) %>% 
      inner_join(ctry_spec_ind_var_proj %>% 
                   # rename(ctry = geo) %>% ONE COUNTRY MODEL
                   filter(interval == periods[i]), by = c('age.group','ctry')) %>% 
      inner_join(
        nuts_vs_ctry_ind_var %>% 
          ungroup() %>% 
          filter(interval == periods[i-1]) %>% 
          dplyr::select(geo,ctry, age.group, contains(c('lag1_quot', 'lag2_quot', 'lagged'))) 
        , by = c('geo','age.group','ctry')) %>% 
      
      # chnaged here to accomodate the for loop
      left_join(  
        urban_land_fraction_KOMMUNE_SSPs12345_period %>% 
          dplyr::filter(scenario == paste0('SSP',z)) %>% 
          dplyr::select(-scenario) %>% 
          filter(period == periods[i]),by = 'geo') %>% 
      mutate(urb_frac = as.numeric(urb_frac)) %>% 
      # inner_join(distinct(test_2020.2024_2015.2019_ctry[,c('geo')]),
      #            by = 'geo') %>% 
      left_join(second_try_clustering, by = 'geo') %>% 
      mutate(cluster = as.character(cluster)) %>% 
       fastDummies::dummy_cols('cluster') 
    #  %>% 
    #   dplyr::select(-c(lagged_quot_diff, cluster_NA)) %>% 
    #   dplyr::filter(ctry != "TR")
    #   
    # 
    # colSums(is.na(to_be_proj)) > 0
    # to_be_proj %>% filter(ctry == "TR") %>% 
    #   dplyr::select(cluster_1)
    # 
    ######
    
    
    
    ages <- c('under15', '15_24', '25_44', '45_64', '65plus')
    for (j in ages) {
      print(paste0("Sieve_fit_brulee_first_test_corct_funcs_", j,'_test20022025_corrStu.RData'))
      load(paste0("./results/Sieve_fit_brulee_first_test_corct_funcs_", j,'_test20022025_corrStu.RData'))
    }
    
    
    
    
    ## under15 ----
    Results_under15 <-  
      lapply( seq(1,20),function(x){
        lapply(seq(1,5), function(y) 
        {fit <- Sieve_fit_brulee_first_test_corct_funcs_under15_test20022025_corrStu[[x]][[y]][[3]]
        standardized_data_test <- to_be_proj %>% dplyr::filter(age.group == 'under15') 
        
        new_names <- names(standardized_data_test) %>% str_replace_all("\\+$", "") # remove "+" at the end of column names
        names(standardized_data_test) <- new_names
        to_standardise <- setdiff(fit$dims$features, c("cluster_1","cluster_2","cluster_3"))
        standardized_data_test <-  standardize_cols( dplyr::select(standardized_data_test,fit$dims$features)
                                                     , to_standardise) 
        
        
        
        
        pred = pull((predict(fit, standardized_data_test[,])))
        
        # pred = pull((predict(fit, standardized_data_test[,c('y',predictor.variable.names.new$selected.variables)])))
        
        return(pred)}
        )
      }
      )
    
    Results_unlisted_under15 <- unlist(Results_under15, recursive = FALSE)
    
    num_elements <- length(Results_unlisted_under15[[1]])
    
    # Create a list of vectors for each index
    result_list_under15 <- list()
    for (n in 1:num_elements) {
      result_list_under15[[n]] <- unlist(lapply(Results_unlisted_under15, function(x) x[n]), recursive = FALSE)
    }
    
    medians_under15 <- unlist(lapply(1:length(result_list_under15), function(x) {median(result_list_under15[[x]])}), recursive = FALSE)
    
    
    proj_under15 <- medians_under15
    tbbl_under15  =  tibble(proj.raw = proj_under15,
                            geo = to_be_proj[to_be_proj$age.group == 'under15',]$geo,
                            age.group = "under15")
    
    
    
    
    ## 15_24 ----
    Results_15_24 <-  
      lapply( seq(1,20),function(x){
        lapply(seq(1,5), function(y)
        {fit <- Sieve_fit_brulee_first_test_corct_funcs_15_24_test20022025_corrStu[[x]][[y]][[3]]
        standardized_data_test <- to_be_proj %>% dplyr::filter(age.group == '15_24')
        
        new_names <- names(standardized_data_test) %>% str_replace_all("\\+$", "") # remove "+" at the end of column names
        names(standardized_data_test) <- new_names
        to_standardise <- setdiff(fit$dims$features, c("cluster_1","cluster_2","cluster_3"))
        standardized_data_test <-  standardize_cols( dplyr::select(standardized_data_test,fit$dims$features)
                                                     , to_standardise) 
        
        
        
        
        pred = pull((predict(fit, standardized_data_test[,])))
        
        return(pred)}
        )
      }
      )
    
    Results_unlisted_15_24 <- unlist(Results_15_24, recursive = FALSE)
    
    num_elements <- length(Results_unlisted_15_24[[1]])
    
    # Create a list of vectors for each index
    result_list_15_24 <- list()
    for (n in 1:num_elements) {
      result_list_15_24[[n]] <- unlist(lapply(Results_unlisted_15_24, function(x) x[n]), recursive = FALSE)
    }
    
    medians_15_24 <- unlist(lapply(1:length(result_list_15_24), function(x) {median(result_list_15_24[[x]])}), recursive = FALSE)
    
    
    proj_15_24 <- medians_15_24
    tbbl_15_24 =  tibble(proj.raw = proj_15_24,
                         geo = to_be_proj[to_be_proj$age.group == '15_24',]$geo,
                         age.group = "15_24")
    
    
    
    ## 25_44 ----
    Results_25_44 <-  
      lapply( seq(1,20),function(x){
        lapply(seq(1,5), function(y)
        {fit <- Sieve_fit_brulee_first_test_corct_funcs_25_44_test20022025_corrStu[[x]][[y]][[3]]
        standardized_data_test <- to_be_proj %>% dplyr::filter(age.group == '25_44')
        
        new_names <- names(standardized_data_test) %>% str_replace_all("\\+$", "") # remove "+" at the end of column names
        names(standardized_data_test) <- new_names
        to_standardise <- setdiff(fit$dims$features, c("cluster_1","cluster_2","cluster_3"))
        standardized_data_test <-  standardize_cols( dplyr::select(standardized_data_test,fit$dims$features)
                                                     , to_standardise) 
        
        pred = pull((predict(fit, standardized_data_test[,])))
        
        return(pred)}
        )
      }
      )
    
    Results_unlisted_25_44 <- unlist(Results_25_44, recursive = FALSE)
    
    num_elements <- length(Results_unlisted_25_44[[1]])
    
    # Create a list of vectors for each index
    result_list_25_44 <- list()
    for (n in 1:num_elements) {
      result_list_25_44[[n]] <- unlist(lapply(Results_unlisted_25_44, function(x) x[n]), recursive = FALSE)
    }
    
    medians_25_44 <- unlist(lapply(1:length(result_list_25_44), function(x) {median(result_list_25_44[[x]])}), recursive = FALSE)
    
    
    proj_25_44 <- medians_25_44
    tbbl_25_44 =  tibble(proj.raw = proj_25_44,
                         geo = to_be_proj[to_be_proj$age.group == '25_44',]$geo,
                         age.group = "25_44")
    
    
    
    
    
    ## 45_64 ----
    Results_45_64 <-  
      lapply( seq(1,20),function(x){
        lapply(seq(1,5), function(y)
        {fit <- Sieve_fit_brulee_first_test_corct_funcs_45_64_test20022025_corrStu[[x]][[y]][[3]]
        standardized_data_test <- to_be_proj %>% dplyr::filter(age.group == '45_64')
        new_names <- names(standardized_data_test) %>% str_replace_all("\\+$", "") # remove "+" at the end of column names
        names(standardized_data_test) <- new_names
        to_standardise <- setdiff(fit$dims$features, c("cluster_1","cluster_2","cluster_3"))
        standardized_data_test <-  standardize_cols( dplyr::select(standardized_data_test,fit$dims$features)
                                                     , to_standardise) 
        
        pred = pull((predict(fit, standardized_data_test[,])))
        
        return(pred)}
        )
      }
      )
    
    Results_unlisted_45_64 <- unlist(Results_45_64, recursive = FALSE)
    
    num_elements <- length(Results_unlisted_45_64[[1]])
    
    # Create a list of vectors for each index
    result_list_45_64 <- list()
    for (n in 1:num_elements) {
      result_list_45_64[[n]] <- unlist(lapply(Results_unlisted_45_64, function(x) x[n]), recursive = FALSE)
    }
    
    medians_45_64 <- unlist(lapply(1:length(result_list_45_64), function(x) {median(result_list_45_64[[x]])}), recursive = FALSE)
    
    
    proj_45_64 <- medians_45_64
    tbbl_45_64 =  tibble(proj.raw = proj_45_64,
                         geo = to_be_proj[to_be_proj$age.group == '45_64',]$geo,
                         age.group = "45_64")
    
    
    
    
    
    ## 65+ ----
    Results_65 <-  
      lapply( seq(1,20),function(x){
        lapply(seq(1,5), function(y) 
        {
          fit <- Sieve_fit_brulee_first_test_corct_funcs_65plus_test20022025_corrStu[[x]][[y]][[3]]
          standardized_data_test <- to_be_proj %>% dplyr::filter(age.group == '65+')
          
          new_names <- names(standardized_data_test) %>% str_replace_all("\\+$", "") # remove "+" at the end of column names
          names(standardized_data_test) <- new_names
          to_standardise <- setdiff(fit$dims$features, c("cluster_1","cluster_2","cluster_3"))
          standardized_data_test <-  standardize_cols( dplyr::select(standardized_data_test,fit$dims$features)
                                                       , to_standardise) 
          
          pred = pull((predict(fit, standardized_data_test[,])))
          
          return(pred)}
        )
      }
      )
    
    Results_unlisted_65 <- unlist(Results_65, recursive = FALSE)
    
    num_elements <- length(Results_unlisted_65[[1]])
    
    # Create a list of vectors for each index
    result_list_65 <- list()
    for (n in 1:num_elements) {
      result_list_65[[n]] <- unlist(lapply(Results_unlisted_65, function(x) x[n]), recursive = FALSE)
    }
    
    medians_65 <- unlist(lapply(1:length(result_list_65), function(x) {median(result_list_65[[x]])}), recursive = FALSE)
    
    
    proj_65 <- medians_65
    tbbl_65 =  tibble(proj.raw = proj_65,
                      geo = to_be_proj[to_be_proj$age.group == '65+',]$geo,
                      age.group = "65+")
    
    
    
    proj_raw <- rbind(tbbl_under15,tbbl_15_24,tbbl_25_44,tbbl_45_64,tbbl_65) %>% 
      dplyr::mutate(ctry = str_sub(geo, start = 1, end = 2))
    
    
    
    proj_results_raw <- 
      proj_raw %>%  
      left_join(
        nuts2_vs_ctry_proj %>%
          filter(interval == periods[i-1]),
        by = c('geo', 'age.group', 'ctry')) %>% 
      left_join(ctry_lvl_prop_proj %>% 
                  filter(interval == periods[i]) %>% 
                  # rename(ctry_prop_lead = ctry_prop, ctry = geo) %>% #ONE COUNTRY
                  
                  rename(ctry_prop_lead = ctry_prop) %>% 
                  
                  
                  dplyr::select(-interval), by = c( 'age.group', 'ctry')) %>% 
      mutate(proj.prop.raw = proj.raw*ctry_prop_lead,  # here we multiply for the prop at the country level which is used to define the dependent variable
             interval = periods[i]) %>% 
      dplyr::select(interval,geo,age.group,proj.prop.raw) %>% 
      group_by(interval, geo) %>%  
      dplyr::mutate(sum = sum(proj.prop.raw)) %>% 
      mutate(proj.prop.raw  = proj.prop.raw / sum) %>% 
      ungroup() %>% 
      pivot_wider(names_from = age.group, values_from = proj.prop.raw) %>% 
      ungroup() %>% 
      dplyr::select(-sum)  %>% 
      dplyr::mutate(ctry = str_sub(geo, start = 1, end = 2))
    
    
    
    
    
    # %>% 
    #   dplyr::mutate(ctry = str_sub(geo,1,2)) 
    # 
    # 
    # proj_results_raw_1 # now we have to take these results and country by country we have to operate the 2-dim impf
    # 
    # countries <- unique(proj_results_raw_1$ctry)
    # proj_results <- tibble()
    # 
    # for (c in countries) { 
    #   pop_tot_raw <- 
    #     proj_results_raw_1[proj_results_raw_1$ctry ==c,] %>% 
    #     inner_join(Gao_adjusted_baseYear_NUTS2_pop_sizes %>% 
    #                  dplyr::select(geo, matches(periods[i])) %>% 
    #                  rename("Nuts2_tot" = matches(periods[i])), by = 'geo') %>% 
    #     mutate(across(c('under15',`15_24`,`25_44`,`45_64`,`65+`),~.*Nuts2_tot)) 
    #   
    #   
    #   pop_tot_raw.seed <- dplyr::select(pop_tot_raw,-c(interval,ctry,geo,Nuts2_tot))
    #   
    #   
    #   target.col <- 
    #     wcde_raw_ageGrouped_adjusted_baseYear_Estat %>% 
    #     dplyr::filter(geo == c) %>% 
    #     gather(period,tot.pop.ctry.age, -c('age.group','geo')) %>% 
    #     dplyr::filter(period == periods[i]) %>% 
    #     dplyr::filter(age.group != 'TOTAL') %>% 
    #     pull(tot.pop.ctry.age)
    #   
    #   target.row <- Gao_adjusted_baseYear_NUTS2_pop_sizes %>% 
    #     dplyr::mutate(ctry = str_sub(geo,1,2)) %>% 
    #     dplyr::select(ctry,geo, matches(periods[i])) %>% 
    #     rename("Nuts2_tot" = matches(periods[i])) %>% 
    #     dplyr::filter(ctry == c) %>% 
    #     pull(Nuts2_tot)
    #   res.c <- Ipfp(pop_tot_raw.seed,list(1),list(target.row), tol.margins = 50000,iter = 10000)
    #   proj_results.c <- tibble(
    #     interval = pop_tot_raw$interval,
    #     geo = pop_tot_raw$geo,
    #     res.c$x.hat %>% as.data.frame()
    #   ) 
    
    #  proj_results <- rbind(proj_results,proj_results_raw_1)
    
    #}
    
    
    
    
    proj_res <- rbind(proj_res,
                      proj_results_raw)
    
  }
  
  
  assign(paste0('proj_res_', "SSP", z),proj_res)
  
  # save(proj_res_SSP1, file = './results/projections/proj_res__SSP2_first_round.RData')
  
}


#####
# besides uk I would say that the problem is 
# 
# [1] "AL01" "AL02" "AL03" "EFTA" "HR02" "HR05" "HR06" "LI00" "NL35" "NL36" "NO08" "NO09" "NO0A"  "PT19" "PT1A" "PT1B" "PT1C" "PT1D" "RS11" "RS12" "RS21" "RS22"
# 
# these are not in our geodata
# [1]                       "EFTA" "HR02" "HR05" "HR06"        "NL35" "NL36" "NO08" "NO09" "NO0A" "PT19" "PT1A" "PT1B" "PT1C" "PT1D"

proj_res_SSP1 %>% 
  group_by(interval) %>% 
  summarise(n = n_distinct(geo)) %>% 


setdiff(
  geo2020 <- 
    proj_res %>% 
    dplyr::filter(interval == "2020-2024") %>% 
    distinct(geo) %>% pull()
  
  ,
  to_be_proj %>% 
    distinct(geo) %>% 
    pull()
)


# 
# 
# to_be_proj %>% 
#   dplyr::filter(ctry == 'UK') %>% 
#   distinct(geo)

proj_res_SSP1$scenario = 1
proj_res_SSP2$scenario = 2
proj_res_SSP3$scenario = 3
proj_res_SSP4$scenario = 4
proj_res_SSP5$scenario = 5



proj_res_all <- rbind(proj_res_SSP1,
                      proj_res_SSP2,
                      proj_res_SSP3,
                      proj_res_SSP4,
                      proj_res_SSP5)


Gao_tot_pop_reformatted <- 
Gao_adjusted_baseYear_NUTS2_pop_sizes_SSPS %>% # this needs to be checkd.
  pivot_longer(cols = -geo, names_to = "name", values_to = "tot.pop") %>% 
  dplyr::mutate(interval = stringr::str_sub(`name`, start = -9,end = -1) ,
                scenario = as.double(stringr::str_sub(`name`, start =4,end = 4))) %>% 
  dplyr::select(-name)
              
proj_res_all_abs_vls <- proj_res_all %>% 
  left_join(Gao_tot_pop_reformatted, by = c('interval', 'geo', 'scenario')) %>% 
  dplyr::filter(!is.na(tot.pop),
                interval != "2010-2014",
                ctry != "LI") %>% 
  dplyr::mutate(across(c(under15,`15_24`,`25_44`,`45_64`,`65+`), ~.x*tot.pop))




## saving results ----

dir.create('./results/projections/')

save(proj_res_SSP1, file = './results/projections/proj_res_SSP1_first_round.rds')
save(proj_res_SSP2, file = './results/projections/proj_res_SSP2_first_round.rds')
save(proj_res_SSP3, file = './results/projections/proj_res_SSP3_first_round.rds')
save(proj_res_SSP4, file = './results/projections/proj_res_SSP4_first_round.rds')
save(proj_res_SSP5, file = './results/projections/proj_res_SSP5_first_round.rds')


save(proj_res_all, file = './results/projections/proj_res_all.rds')
write.csv(proj_res_all, file = './results/projections/proj_res_all.csv', row.names = F)


save(proj_res_all_abs_vls, file = './results/projections/proj_res_all_abs_vls.rds')
write.csv(proj_res_all_abs_vls, file = './results/projections/proj_res_all_abs_vls.csv', row.names = F)
# 
# proj_res_all <- read.csv(file = './results/projections/proj_res_all.csv')
# proj_res_all_abs_vls <- read.csv(file = './results/projections/proj_res_all_abs_vls.csv')
# load(file = './results/projections/proj_res_SSP1_first_round.rds')
# load(file = './results/projections/proj_res_SSP2_first_round.rds')
# load(file = './results/projections/proj_res_SSP3_first_round.rds')
# load(file = './results/projections/proj_res_SSP4_first_round.rds')
# load(file = './results/projections/proj_res_SSP5_first_round.rds')
#  
# 
# 
# 


