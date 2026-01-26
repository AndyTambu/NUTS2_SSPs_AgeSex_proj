# Infos ####
# Auhtor: Andrea Tamburini 
# content: here we add the clustering results and the urbanisation percentage. 
library(tidyverse)
library(purrr)

myFiles <- paste0('./data/train&test/',list.files('./data/train&test/'))

for (i in myFiles) {
  load(i)
}


names(train_2000.2004_1995.1999_ctry) <- gsub("\\+", "", names(train_2000.2004_1995.1999_ctry))
names(train_2005.2009_2000.2004_ctry) <- gsub("\\+", "", names(train_2005.2009_2000.2004_ctry))
names(train_2010.2014_2005.2009_ctry) <- gsub("\\+", "", names(train_2010.2014_2005.2009_ctry))
names(train_2015.2019_2010.2014_ctry) <- gsub("\\+", "", names(train_2015.2019_2010.2014_ctry))
names(test_2020.2024_2015.2019_ctry) <- gsub("\\+", "", names(test_2020.2024_2015.2019_ctry))



load(file = './data/cluster_df_hierarchical_PCA_wardD.RData')
load('./dataGao/cropped/urban_land_fraction_NUTS2_SSP2_period.RData') # these are the cropped correspondent of the Hardvard DataVerse data from Gao et al. 


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

# pay attention here the period does not align perfectly with the one of the variables since the population info are not available for the 2024















