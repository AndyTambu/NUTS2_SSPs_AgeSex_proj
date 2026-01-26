# libraries and functions ----
library(conclust)
library(eurostat)
library(mclust)


  # Clustering with 5 age groups ----
  ## ratios----

  load(file = './data/data_for_clustering.RData') #these are just the proportion data but filtered by time so that we just have the ones for the clustering 
  

  ctry_info <- 
    data_for_clustering %>% 
    dplyr::filter(nchar(geo) == 2) %>% 
    dplyr::filter(sex == 'T') %>% 
    dplyr::select(-c(UNK, TOTAL,sex)) %>% 
    pivot_longer(names_to = 'age.group', values_to = 'prop_ctry', cols = c("under15","15_24","25_44","45_64","65+" ))
  
  
  nuts2_info <- 
    data_for_clustering %>% 
    filter(!stringr::str_detect(geo,'XX')&!str_length(geo)==2&!geo%in%c("ALXX","EFTA","EU28","FRXX","HUXX",'MKXX',
                                                                        "HR02","HR05","HR06","NO08","NO09","NO08","NO09","NO0A","NO0B")) %>% 
    dplyr::filter(nchar(geo) == 4) %>% 
    dplyr::filter(sex == 'T') %>%
    dplyr::select(-c(UNK, TOTAL, sex)) %>% 
    pivot_longer(names_to = 'age.group', values_to = 'prop_nuts', cols = c("under15","15_24","25_44","45_64","65+" )) %>% 
    dplyr::mutate(ctry = str_sub(geo,1,2))
  
  
  mean_ratios <- 
    nuts2_info %>% 
    dplyr::filter(ctry != 'TR') %>%    # here we are excluding TR !!
    left_join(ctry_info, by = c('interval', 'ctry' = 'geo', 'age.group')) %>% 
    dplyr::mutate(ratio = prop_nuts/prop_ctry) %>% 
    dplyr::filter(interval == "2015-2019") %>% 
    dplyr::group_by(geo, age.group) %>% 
    dplyr::summarise(ratio = mean(ratio, na.rm = T))
  
  
  
  geodata_16 <- get_eurostat_geospatial(
    output_class = "sf",
    resolution = "10",
    nuts_level = 'all',
    crs = "3035",
    year = 2016
  )
  


  ## PCA ----
  
  normalised_data <- 
    mean_ratios %>% 
    dplyr::filter(geo %in% eurostat_geodata_60_2016$geo) %>% 

    filter(!stringr::str_detect(geo,'XX')&!str_length(geo)==2&!geo%in%c("ALXX","EFTA","EU28","FRXX","HUXX",'MKXX',"NO08","NO09","NO0A","NO0B")) %>% 
    
    dplyr::mutate(y_avg = (ratio)) %>% 
    dplyr::select(-ratio) %>% 
    pivot_wider(names_from = age.group, values_from = y_avg) %>% 
    column_to_rownames("geo") %>% 
    dplyr::select(under15, '15_24','25_44','45_64','65+') %>% 
    ungroup() %>% 
    
    mutate_at(.vars = vars(under15, '15_24','25_44','45_64','65+'),
              ~( . - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)) %>%
    # 
    # mutate_at(.vars = vars(under15, '15_24','25_44','45_64','65+'),
    #           ~( . - base::min(., na.rm = TRUE) )/ (base::max(., na.rm = TRUE) - base::min(., na.rm = TRUE))) %>% 
    # ~ .) %>% 
    as.matrix()
  
  
  
  pca_result <- prcomp(normalised_data, scale. = F)
  pca_data <- as.data.frame(pca_result$x[, 1:3])  # Use first 3 principal components
  

  data_to_cluster <- 
    pca_data %>% 
    as.matrix() %>% 
    dist()  
  
  
  
  
  
  hier_methods <- c("ward.D", "ward.D2", "single", "complete",
                    "average", "mcquitty", "median","centroid")
  
  
  
  hierclustS <- lapply(hier_methods, function(x){hclust(data_to_cluster,method = x)})
  

  
  cluster_df_hierS <- list()
  info <- list()
  for (i in 1:length(hier_methods)) {
    cut_tree <- 
      cutree(hierclustS[[i]], k = 3)
    
    cluster_df_hier <- 
      cut_tree %>% 
      as.data.frame() %>%
      rownames_to_column() %>%
      tibble::as_tibble() %>% 
      dplyr::rename('cluster' = '.',
                    'geo' = 'rowname') %>% 
      dplyr::mutate(cluster = as.factor(cluster))
    
    distinct_clust <- 
      cluster_df_hier%>% 
      group_by(cluster) %>% 
      summarise(n = n_distinct(geo))
    
    
    cluster_df_hierS[[i]] <- cluster_df_hier
    info[[i]] <- distinct_clust
  }
  

  
  

  
  
  # plotting ###############################
  cluster_df_hierarchical_PCA_wardD <- cluster_df_hierS[[1]]


  plot_clust <- ggplot() +  
    # Plot the regions with thin borders  
    geom_sf(data = geodata_16 %>% 
              st_transform(crs = 3035) %>%  # this is a projection which is good for europe 
              dplyr::filter(LEVL_CODE == 2) %>%  
              merge(cluster_df_hierarchical_PCA_wardD, by.x = 'NUTS_ID', by.y = 'geo'),  
            aes(fill = as.factor(cluster), geometry = geometry),  
            linewidth = 0.1, color = 'black') +  
    # Overlay country borders with thicker lines  
    geom_sf(data = geodata_16 %>%  
              dplyr::filter(LEVL_CODE == 0 & NUTS_ID != 'TR'),  
            aes(geometry = geometry),  
            fill = NA, color = 'black', linewidth = 0.6) +  
    # Define custom cluster colors  
    scale_fill_manual(
      values = c("1" = "#0173b2", "2" = "#de8f05", "3" = "#029e73"),
      guide = guide_legend(override.aes = list(size = 10))  # make fill boxes larger
    ) +
    coord_sf(
      xlim = c(1107913, 5900000),
      ylim = c(1338895, 5709650),
      expand = FALSE
    ) +
    labs(fill = "Cluster") +  
    theme_void() +  
    theme(
      legend.position = c(0.15, 0.65),
      legend.justification = c(0, 0.5),
      legend.key.size = unit(4.5, "lines"),          # very large keys
      legend.text = element_text(size = 26),         # large text
      legend.title = element_text(size = 30, face = "bold"),
      legend.margin = margin(20, 20, 20, 20)
    )
  plot_clust
  
