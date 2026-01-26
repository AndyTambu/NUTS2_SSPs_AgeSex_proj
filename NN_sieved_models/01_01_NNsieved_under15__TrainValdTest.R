# intro and libraries ----
library('neuralnet')
library('keras')
library('tensorflow')
library(sf)
library(raster)
library(tidyverse)
library(zoo)
library(raster)
# library('rgdal')
library(sp)
library(magrittr)
library(spatialRF)
library(kableExtra)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(randomForestExplainer)
library(pdp)
# library(rgeos)
library(viridis)
library(data.table)
library(ggplot2)
library(car)
library(textshape)
library(keras)
library(tidymodels)
library(recipes)
library(Metrics)
library(brulee)
library(rsample)
library(workflows)
library(parsnip)
library(tune)
library(dcortools)
library(reshape2)


# 1. loading files ----
## 1.1 geo and database files ----
## 1.2 train and test data load ----

source(file = "./00_03_addingClustAndUrb.R")
### age group selection ----
agroup = 'under15'

full_trainingData <- 
  
  train_2000.2004_1995.1999_ctry  %>% 
  
  rbind(train_2005.2009_2000.2004_ctry) %>% 
  
  rbind(train_2010.2014_2005.2009_ctry) %>% 
  
  rbind(train_2015.2019_2010.2014_ctry) %>% 
  
  filter(age.group==agroup) %>% 
  
  dplyr::select(-c(geo,age.group, interval,diff,lag1.tot_pop_NUTS2,lag2.tot_pop_NUTS2)) %>% 
  
  filter(!is.na(y)&y != Inf) 


train_2000.2004_1995.1999_ctry %>%
  filter(age.group==agroup) %>% 
  group_by(cluster) %>% 
  summarise(n = n()) %>% 
  dplyr::mutate(prop = n/sum(n))

# we have to keep into account that not all the clusters are represented equally: 
# this is why we then had to make the stratification for the test validation training.


# this unintuitive definitions come from the old approach but now we aim at having one unique big data bloc from which we
# take the different components to produce the training validation test

full_trainingData_raw <- full_trainingData
new_names <- names(full_trainingData) %>% 
  str_replace_all("\\+$", "") # remove "+" at the end of column names

names(full_trainingData) <- new_names
full_trainingData %>%
  colnames()

drop_columns_with_zero_variance <- function(data) {
  # Calculate the variances of each column
  variances <- apply(data, 2, var)
  
  # Find the names of columns with zero variance
  zero_var_cols <- names(variances[variances == 0])
  
  # Drop the columns with zero variance
  data <- data[, !names(data) %in% zero_var_cols]
  
  # Print the names of dropped columns
  if (length(zero_var_cols) > 0) {
    cat("Dropped columns with zero variance:", paste(zero_var_cols, collapse = ", "), "\n")
  } else {
    cat("No columns with zero variance found.\n")
  }
  
  return(data)
}

full_trainingData <- full_trainingData %>% 
  dplyr::select(-c(
    'quot_65', 'quot_45_64', 'quot_25_44', 'quot_15_24', 'quot_under15',
    'diff_1', 'diff_diffs',
    'log.quot')) # here the log.quot has to be removed because this was meant to be a dependent variable. 



# TEST data come into play ----
full_testData <- test_2020.2024_2015.2019_ctry %>% 
  filter(age.group==agroup) %>% 
  dplyr::select(-c(geo,age.group, interval,diff,lag1.tot_pop_NUTS2,lag2.tot_pop_NUTS2,
                   'quot_65', 'quot_45_64', 'quot_25_44', 'quot_15_24', 'quot_under15', 
                   'diff_1', 'diff_diffs', 
                   'log.quot')) %>% 
  filter(!is.na(y)&y != Inf)


new_names <- names(full_testData) %>% 
  str_replace_all("\\+$", "") # remove "+" at the end of column names

names(full_testData) <- new_names

# we have to drop of course all the information which are "in the future". 
# We had to delete all the stuff which was referring to the same time step 
# as the one which we have to guess


# in the train val test version of the script, here we finally create the full data-set 
# in which we will define the training test and validation components

full_Data <- rbind(full_testData,full_trainingData)




# 2. Data processing ----

# here we get rid of the columns which have repetitive information for all the data point like the ones which 
# are referring ot the country level data.


## 2.1 basic cleaning ----

source('./SIEVE_functions.R')

# visualisation of the rule-of.thumb dimension of the amount of possible independent variables 
# plot_inequality(1000)


### "cleaning" functions ----

# Create a function to calculate pairwise correlations
calculate_pairwise_correlation <- function(data) {
  
  cor_matrix <- cor(data, use = "pairwise.complete.obs")
  
  
  rownames(cor_matrix) <- colnames(data)
  cor_data <- as.data.frame(as.table(cor_matrix))
  colnames(cor_data) <- c("Variable1", "Variable2", "Correlation")
  return(cor_data)
}

# Create a function to calculate pairwise VIF
calculate_pairwise_vif <- function(data) {
  vif_list <- vector("list", length = ncol(data))
  
  data = as.data.frame(your_tibble)
  
  for (i in 1:ncol(data)) {
    variables <- setdiff(colnames(data), colnames(data)[i])
    formula <- as.formula(paste(colnames(data)[i], "~", paste(variables, collapse = "+")))
    vif_list[[i]] <- data.frame(Variable = colnames(data)[i], VIF = car::vif(lm(formula, data = data))
    )
  }
  vif_data <- do.call(rbind, vif_list)
  return(vif_data)
}



### high correlation filtering ----
# Now I first make a basic filtering of the variables whcih are very very highly 
# correlated and then we proceed with a definition of what we need in terms of the rest.

dependent.variable.name <- "y"
predictor.variable.names <- setdiff(colnames(full_Data), dependent.variable.name)


predictor.variable.names.new <- spatialRF::auto_cor(
  x = full_Data[, predictor.variable.names],
  cor.threshold = 0.9,
  preference.order = NULL
)



your_tibble <- data.frame( # Replace with your data
  predictor.variable.names.new$selected.variables.df
  # Add more variables as needed
)

# Calculate pairwise correlation
# cor_matrix <- calculate_pairwise_correlation(your_tibble)
### distance correlation ----
# in this version of the script we consider the distance correlation which is more 
# general, in terms of relationships which can get than the normal correlation 


dcor_matrix <- dcortools::dcmatrix(your_tibble %>% 
                                     dplyr::select(-starts_with("cluster")))
cor_matrix <- dcor_matrix$dcor
# Reshape the correlation matrix into a long format suitable for ggplot2

# Reshape the correlation matrix into a long format suitable for ggplot2
cor_matrix_melted <- melt(cor_matrix) %>% 
  mutate(nw_value = ifelse(value > 0.999999999,NA , value)) # NOTE: strange here, with == 1 the 1 was not actually disappearing. Makes sense considering the numerical approximations.

# Calculate the minimum and maximum values
min_value <- min(cor_matrix_melted$nw_value, na.rm = T)
max_value <- max(cor_matrix_melted$nw_value, na.rm = T)

min_value_rounded <- round(min_value, 2)
max_value_rounded <- round(max_value, 2)


# Correlation heatmap
heatmap_plot <- ggplot(cor_matrix_melted, 
                       aes(Var1, Var2, fill = nw_value)
                       #aes(Variable1, Variable2, fill = nw_value)
) +
  geom_tile() +
  scale_fill_viridis(option = "C", name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  labs(title = "Correlation Matrix Heatmap") +
  guides(fill = guide_colorbar(title = paste("Correlation\n", "Min:", min_value_rounded, "Max:", max_value_rounded)))

# Display the plot
# print(heatmap_plot)

#Initial correlation study. Here I now isolate the variables which have a correlation above 0.6. These are stored and they will be filtered out in the proceeding of the study. 

# Calculate pairwise VIF
# vif_matrix <- calculate_pairwise_vif(your_tibble)
# 
# Correlation_filtered <- 
#   unique_pairs(cor_matrix, "Variable1", "Variable2") %>% 
#   filter(Variable1 != Variable2) %>% 
#   filter(abs(Correlation)> 0.75) %>% 
#   arrange(Correlation) %>% 
#   mutate_at(vars('Variable1','Variable2'), ~ as.character(.x))



# length(all.vars(frml))
# this is the version for the distance correlation
# here we fix a boundary for the correlation which we want to accept as maximum and we look at all the 
# couples of variables which have a correlation which is higher than the boundary which we defined.

Correlation_filtered <- 
  #unique_pairs(cor_matrix, "Variable1", "Variable2") %>% 
  unique_pairs(cor_matrix_melted, "Var1", "Var2") %>% 
  filter(Var1 != Var2) %>% 
  filter(abs(value)> 0.65) %>% 
  arrange(value) %>% 
  mutate_at(vars('Var1','Var2'), ~ as.character(.x)) %>% 
  dplyr::rename('Variable1' = 'Var1', 'Variable2' = 'Var2',
                'Correlation' = 'value')


#Here I build and additional column which defines which would be the
#priority in terms of variable selection when we have both variables 
#having basically the same impact on the R2.

# Assuming you have a tibble called 'your_tibble' with columns 'Variable1', 'Variable2', and 'Correlation'
# Create an empty column 'Priority'

your_tibble <- Correlation_filtered[,1:3]
your_tibble$Priority <- NA

# Iterate through rows
for (i in 1:nrow(your_tibble)) {
  row <- your_tibble[i, ]
  
  # Print the values of 'Variable1' and 'Variable2' in the current row
  cat("Row", i, ":\n", "Variable1:", row$'Variable1', "\n", "Variable2:", row$Variable2, "\n")
  
  # Prompt for priority (1 or 2)
  priority <- as.integer(readline(prompt = "Enter priority (1 for Variable1, 2 for Variable2): "))
  
  # Check if the input is valid (1 or 2)
  if (priority %in% c(1, 2)) {
    # Assign the priority to the 'Priority' column
    your_tibble$Priority[i] <- priority
  } else {
    cat("Invalid input. Please enter 1 or 2.\n")
    i <- i - 1  # Decrement i to repeat the current row
  }
}

print(your_tibble)


# Variable1       Variable2 Correlation Priority
# 1 lag1ratio.tot_pop_NUTS2     g.pop.NUTS2   0.7895760        2
# 2                     lon             lat   0.8059816        2
# 3         lag1_quot_45_64      lag1_45_64   0.8282618        2
# 4         lag2_quot_45_64 lag1_quot_45_64   0.8306539        2
# 5            lag2_under15    lag1_under15   0.8927068        2




Correlation_study <- your_tibble


# here we define the data which are going to be used to train the very basic model, the one which we will need to 
# then improve in terms of reduction fo the variables.


data_raw <- 
  full_Data[,c('y',predictor.variable.names.new$selected.variables)]


# predictor.variable.names.new <- spatialRF::auto_cor(
#   x = full_trainingData[, predictor.variable.names],
#   cor.threshold = 0.9,
#   preference.order = NULL
# ) %>% 
#   spatialRF::auto_vif(
#     vif.threshold = 5,
#     preference.order = NULL
#   )
##  plot the selected covariates ----

plot1 <- 
  spatialRF::plot_training_df(
    data = data_raw,
    dependent.variable.name = 'y',
    predictor.variable.names = predictor.variable.names.new$selected.variables[1:12],
    ncol = 3,
    point.color = viridis::viridis(100, option = "F"),
    line.color = "gray30"
  )

plot2 <- 
  spatialRF::plot_training_df(
    data = data_raw,
    dependent.variable.name = 'y',
    predictor.variable.names = predictor.variable.names.new$selected.variables[13:24],
    ncol = 3,
    point.color = viridis::viridis(100, option = "F"),
    line.color = "gray30"
  )



# plot1
# plot2



## SPLITTING: TRAINING VALIDATION TEST ----

# so here the point is that we have now a way to distinguish here what we want and also we have a way to decide how do we standardise.

# for us is fundamental to keep in mind that data come form different time points,
# which also implies different nuts definitions and also different population and migration
# situations. Also, we know that we have identified 3 different clusters which represents 
# our archetypes for the different population structures. In the light of this, we define the
# different proportions for the different parts. These are then gonan be used to define the splitting 
# stratifications for the train validation and test set.


cluster_prop <- 
train_2000.2004_1995.1999_ctry %>%
  filter(age.group==agroup) %>% 
  group_by(cluster) %>% 
  summarise(n = n()) %>% 
  dplyr::mutate(prop = n/sum(n))

period_prop <- 
  full_Data %>%
  group_by(period) %>% 
  summarise(n = n()) %>% 
  dplyr::mutate(prop = n/sum(n))

# "2000-2004" "2005-2009" "2010-2014" "2015-2019" "2020-2024"




### phase 1: initial split ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
set.seed(13)                                    # <- Ensure reproducibility
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


# Create a combined column for stratification
full_Data_sampled <- full_Data %>% # we resample just to add an additional layer or randomness
  sample_frac(1) %>% 
  mutate(strata_col = interaction(period, cluster, drop = TRUE))
# since we need to stratify over two different columns, 
# here we need to create a new column which is
# built using the interaction of the other twp


# First split (70% train, 30% remaining)
split_data <- initial_split(full_Data_sampled, prop = 0.7, strata = strata_col)
# Extract training set
train_data <- training(split_data)

# Now split the remaining 30% (15% validation, 15% test)
remaining_data <- testing(split_data)
val_test_split <- initial_split(remaining_data, prop = 0.5, strata = strata_col)

# Extract validation and test sets
val_data <- training(val_test_split)
test_data <- testing(val_test_split)

# Remove temporary column
train_data <- dplyr::select(train_data, -strata_col)
val_data <- dplyr::select(val_data, -strata_col)
test_data <- dplyr::select(test_data, -strata_col)


#### splitting raw data ----
# we create a twind data split with just the minimum information with geo, cluster and period to make 
# then our considerations in terms of model performance like in point 4.3

full_trainingData_raw <- 
  train_2000.2004_1995.1999_ctry  %>% 
  rbind(train_2005.2009_2000.2004_ctry) %>% 
  rbind(train_2010.2014_2005.2009_ctry) %>% 
  rbind(train_2015.2019_2010.2014_ctry) %>% 
  filter(age.group==agroup) %>% 
  dplyr::select(-c(age.group, interval,diff,lag1.tot_pop_NUTS2,lag2.tot_pop_NUTS2)) %>% 
  filter(!is.na(y)&y != Inf) 

full_testData_raw <- test_2020.2024_2015.2019_ctry %>% 
  filter(age.group==agroup) %>% 
  dplyr::select(-c(age.group, interval,diff,lag1.tot_pop_NUTS2,lag2.tot_pop_NUTS2))

full_Data_raw <- rbind(full_testData_raw,full_trainingData_raw) %>% 
  dplyr::select(geo,y, period, cluster) # we resample just to add an additional layer or randomness
  

# Create a combined column for stratification
full_Data_sampled_raw <- full_Data_raw %>% 
  sample_frac(1) %>% 
  mutate(strata_col = interaction(period, cluster, drop = TRUE))
# since we need to stratify over two different columns, 
# here we need to create a new column which is
# built using the interaction of the other twp


# First split (70% train, 30% remaining)
split_data_raw <- initial_split(full_Data_sampled_raw, prop = 0.7, strata = strata_col)
# Extract training set
train_data_raw <- training(split_data_raw)

# Now split the remaining 30% (15% validation, 15% test)
remaining_data_raw <- testing(split_data_raw)
val_test_split_raw <- initial_split(remaining_data_raw, prop = 0.5, strata = strata_col)

# Extract validation and test sets
val_data_raw <- training(val_test_split_raw)
test_data_raw <- testing(val_test_split_raw)




### phase 2: standardize ----

formula_str <- paste("y ~", paste(c(predictor.variable.names.new$selected.variables), collapse = " + "))
frml <- as.formula(formula_str)
to_standardise <- setdiff(predictor.variable.names.new$selected.variables, c("cluster_1","cluster_2","cluster_3"))

# for this new version we are heavily relying on the pre-processing steps coming from the 
# recepies package. Here we define a recipe which is basically just about standardisign The
# data from the training set. Once that is done, then we are going to use the same information
# in terms of mean and standard deviation to standardise the validation and the test set. This is
# to avoid spilling information to the sets which are supposed to be unknown for the model. 
# DATA LEAKAGE

# The recipes package in R is part of tidymodels and is designed to preprocess data in a structured, reproducible way. It allows you to create a "recipe" for data transformation and then apply it consistently across training, validation, and test sets.
# 
# 🔹 How recipes Works
# Define a recipe (recipe())
# Add preprocessing steps (e.g., normalization, imputation, encoding)
# Train the recipe on the training data (prep())
# Apply transformations to all datasets (bake())
# This workflow ensures that the same transformations (computed on the training set) are applied to new data (validation/test sets), avoiding data leakage.
# 
# 
# 
# # Define a recipe
rec <- recipe(train_data) %>%
       update_role(all_of(to_standardise), new_role = "predictor") %>%  # Define variables to scale
       step_normalize(all_of(to_standardise))  # Standardize the selected columns



# Train the recipe using the training data
prep_rec <- prep(rec, training = train_data)

# Apply to all datasets using `bake()`
train_scaled <- bake(prep_rec, new_data = train_data)
val_scaled <- bake(prep_rec, new_data = val_data)
test_scaled <- bake(prep_rec, new_data = test_data)





# 3. NN model ----

# Here we introduce the first time the nn model. Here the point is about selecting the variables. 
# This means that we first introduce the model to see which variable can be more useful
# when we have couples of variables which show a high degree of distance/linear correlation
# (this is defined according to what is the correlation definition which we are using).


## 3.1 I hyper param opt: CUSTOM train - validation APPROACH ----

brulee_tune_pra <- parsnip::mlp(
  hidden_units = tune(),
  learn_rate = tune(), 
  activation = tune(),
  epochs = 10
) %>%
  
  parsnip::set_engine("brulee", output_activation = "tanh") %>% 
  # parsnip::set_engine("brulee") %>%
  parsnip::set_mode("regression")

num_elements <- length(all.vars(frml))

# brulee_grid <- my.grid <- expand.grid( learn_rate = seq(0.01,0.2, by = 0.01), 
#                                        hidden_units = seq(6,8),
#                                        activation = c('elu', 'relu')
# )

brulee_grid <- my.grid <- expand.grid( learn_rate = 10^(seq(-3, -1, length.out = 5)), 
                                       hidden_units = seq(5,32, by = 3),
                                       activation = c('elu', 'relu', 'tanh')
)



# brulee_grid <- my.grid <- expand.grid( learn_rate = c(0.01,0.1), hidden_units = c(4,5,6))
# brulee_grid <- my.grid <- expand.grid( learn_rate = c(0.01), hidden_units = c(4))



brulee_wflow <- workflow() %>%
  add_model(brulee_tune_pra) %>% 
  add_recipe(recipe(formula = frml,
                    data = train_scaled,
                    stop_iter = 5
  )
  )

# here we use the repeated sub sampling to make what it is usually employed for: 
# hyperparameters optimisation. It is meant to select which ones are the 
# hyperparameters which perform better


# Here, in order to use the tune_grid function we used the manual_rset() function forcing the indeces to at the end mimic 
# our train validation and test split

# split = rsample::manual_rset(split)
indices <- list(
  list(analysis = seq(1,850), assessment = seq(851,1032))
  )
splits <- lapply(indices, make_splits, data =  rbind(train_scaled, val_scaled))
manual_set = manual_rset(splits, c("Split 1"))


semi_sieve_cv <- 
  lapply(seq(1,1), 
         
         function(x){
           set.seed(x*78)
           tuned_model <- tune_grid(brulee_wflow,
                                    resamples = manual_set,
                                    grid = brulee_grid,
                                    control = control_resamples(save_pred = TRUE))
           
           
           
           # results combination and we extract the ones which are working better. 
           combined_data <- do.call(rbind, Map(cbind, tuned_model$.metrics, round = paste0("round_", seq_along(tuned_model$.metrics))))
           return(combined_data)
           
         }
  )

combined_semi_sieve_cv <- do.call(rbind, Map(cbind, semi_sieve_cv, round = paste0("round_", seq_along(semi_sieve_cv))))
# save(combined_semi_sieve_cv, file = './01_Age_Structure/AAA_longer_timeSeries/results/xxxxxxxx.RData')



combined_semi_sieve_cv %>% 
  dplyr::select(-round) %>% 
  group_by(hidden_units,activation, learn_rate, .metric) %>% 
  # summarise( n =n_distinct(.estimate))
  summarise(avg_perf = mean(.estimate)) %>% 
  pivot_wider(names_from = .metric, values_from = avg_perf) %>% 
  arrange(rmse)


# 22012025
# `summarise()` has grouped output by 'hidden_units', 'activation', 'learn_rate'. You can override using
# the `.groups` argument.
# # A tibble: 150 × 5
# # Groups:   hidden_units, activation, learn_rate [150]
# hidden_units activation learn_rate   rmse   rsq
# <dbl> <chr>           <dbl>  <dbl> <dbl>
#   1           26 elu            0.1    0.0137 0.983


## 3.2 Covariates selection ----
# 
# Here we implement the "jack-knife"-style covariance selection for the work. 
# So the idea is to take the variables which were highly correlated and 
# understand which of those is actually important for the model. 
# 
# What do we do now ? We take what we did for the correlation definition and 
# we work from there to decide which ones to keep and which ones to drop.


r2s_test <- rep(NA,10)
for (i in 1:10){
  seed =set.seed(i*984047)
  model_superBasic_loop =
    fit_brulee_mlp(
      formula = frml,
      training_data=train_scaled,
      num_neurons=26,
      epochs_num = 10,
      activation_fun = "elu",
      seed_to_set = seed,
      learning_rate = 0.1
    )
  
  mse_test_loop =  mean(pull((predict(model_superBasic_loop,  val_scaled) -  val_scaled$y)^2))
  rmse_test_loop = sqrt(mse_test_loop)
  test_r2_loop =  1-(mse_test_loop/var(val_scaled$y))
  r2s_test[i] <- test_r2_loop
  
}

r2_comparison = mean(r2s_test)

r2_comparison

# r2_comparison will act as comparing point for the definition of the performances with a 
# variable or the other one 

## dropping ----

# the formual was corrected on the 26112024 
drop_formula_terms <- function(the_formula, var_names) {
  # Get the term.labels from the formula
  # Loop through each var_name to drop terms
  for (var_name in var_names){
    term_labels <- attr(terms(the_formula), "term.labels")
    var_positions <- grep(paste("^", var_name, "$", sep = ""), term_labels)
    if (length(var_positions) > 0) {
      for (var_position in var_positions) {
        the_formula <- update(
          the_formula,
          drop.terms(terms(the_formula), var_position, keep.response = TRUE)
        )
      }
    }
    
    
  }
  return(the_formula)
  
}






Correlation_study_order <- 
  Correlation_study %>% 
  arrange(-abs(Correlation))

to_drop <- c()

for (j in 1:dim(Correlation_study_order)[1]) {
  print(j)
  r2s_drop <- array(NA, dim = c(5,2))
  
  for (i in 1:5) {
    
    seed = set.seed(334)
    
    model_superBasic_drop_1 = fit_brulee_mlp(
      formula = drop_formula_terms(frml,Correlation_study_order[j,1]),
      # training_data=standardized_data[,c('y',predictor.variable.names.new$selected.variables)],
      training_data=train_scaled,
      num_neurons=26,
      epochs_num = 10,
      activation_fun = "elu",
      seed_to_set = seed,
      learning_rate = 0.1
    )
    
    # 
    # mse_test_drop_1 = mean(pull((predict(model_superBasic_drop_1, standardized_data_test[,c('y',predictor.variable.names.new$selected.variables)]) 
    #                              - standardized_data_test$y)^2))
    
    mse_test_drop_1 = mean(pull((predict(model_superBasic_drop_1,   val_scaled) 
                                 -  val_scaled$y)^2))
    
    
    rmse_test_drop_1 = sqrt(mse_test_drop_1)
    test_r2_drop_1 = 1-(mse_test_drop_1/var(val_scaled$y))
    r2s_drop[i,1] <- test_r2_drop_1
    
    
    
    model_superBasic_drop_2 = fit_brulee_mlp(
      formula = drop_formula_terms(frml,Correlation_study_order[j,2]),
      #training_data=standardized_data[,c('y',predictor.variable.names.new$selected.variables)],
      training_data=train_scaled,
      num_neurons=26,
      epochs_num = 10,
      activation_fun = "elu",
      seed_to_set = seed,
      learning_rate = 0.1
    )
    
    mse_test_drop_2 = mean(pull((predict(model_superBasic_drop_2,   val_scaled) 
                                 -  val_scaled$y)^2))
    rmse_test_drop_2 = sqrt(mse_test_drop_2)
    test_r2_drop_2 = 1-(mse_test_drop_2/var(val_scaled$y))
    r2s_drop[i,2] <- test_r2_drop_2
    
  }
  
  
  
  
  r2s_new <- colMeans(r2s_drop)
  print(r2s_new)
  
  
  
  to_drop_new  =  {if(r2s_new[1]>r2_comparison&r2s_new[2]>r2_comparison&Correlation_study_order[j,4] == 1)
    Correlation_study_order[j,2] else 
      if(r2s_new[1]>r2_comparison&r2s_new[2]>r2_comparison&Correlation_study_order[j,4] == 2)
        Correlation_study_order[j,1] else 
          if(r2s_new[1]>r2_comparison&r2s_new[2]<=r2_comparison)
            Correlation_study_order[j,1] else 
              if(r2s_new[1]<=r2_comparison&r2s_new[2]>r2_comparison)
                Correlation_study_order[j,2] else 
                  NA}
  
  
  
  to_drop <- cbind(to_drop,to_drop_new)         
  print(to_drop)
  
}

to_drop_clean <- to_drop[!is.na(to_drop)]



#Correlation_study_order_clean <-
# here we the which of the correlation study variables were not mapped to any of the 
# two priorities. This means that we see what we have to pick by hand. This is because
# there was no performance difference when dropping the one or the other or that they both
# decresed the performance and so we have to decide by hand

Correlation_study_order[is.na(to_drop),]

to_drop_clean <- c(to_drop_clean
#                    ,c(
# 'lag1_ctry_pop_g_under15',
#'lag1_g_25_44' 
                   ) %>% 
  unique()


# this we made as a for loop because we test the performance 10 times. 
for (z in 1:10){
  trick <- 
    lapply(seq(1,10), function(i){
      model_clean_basic = fit_brulee_mlp(
        formula = drop_formula_terms(frml,to_drop_clean),#auto
        
        training_data=train_scaled,
        num_neurons=26,
        epochs_num = 20,
        activation_fun = "elu",
        seed_to_set = 8458*z,
        learning_rate = 0.1
      )
      
      
      mse_test =  mean(pull((predict(model_clean_basic, val_scaled) 
                             -  val_scaled$y)^2))
      rmse_test = sqrt(mse_test)
      test_r2 = 1-(mse_test/var(val_scaled$y))
      
      return(test_r2)
    })
  print(mean(unlist(trick)))
}



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
new_frml = drop_formula_terms(frml,to_drop_clean)
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

length(all.vars(new_frml))



## 3.3 II hyper param opt: CUSTOM train - validation APPROACH ----
# since we have now a completely different formula ! 

brulee_tune_pra <- parsnip::mlp(
  hidden_units = tune(),
  learn_rate = tune(), 
  activation = tune(),
  epochs = 10
  ) %>%
  parsnip::set_engine("brulee", output_activation = "relu") %>% 
  # parsnip::set_engine("brulee") %>%
  parsnip::set_mode("regression")

num_elements <- length(all.vars(frml))

# brulee_grid <- my.grid <- expand.grid( learn_rate = seq(0.01,0.2, by = 0.01), 
#                                        hidden_units = seq(6,8),
#                                        activation = c('elu', 'relu')
# )

brulee_grid_II <- my.grid <- expand.grid( 
                                            learn_rate = 10^seq(-5, 0, length.out = 15), 
                                          hidden_units = seq(2,10, by = 1),
                                            activation = c('elu', 'relu', 'tanh')
                                        )


# brulee_grid <- my.grid <- expand.grid( learn_rate = c(0.01,0.1), hidden_units = c(4,5,6))
# brulee_grid <- my.grid <- expand.grid( learn_rate = c(0.01), hidden_units = c(4))



brulee_wflow <- workflow() %>%
  add_model(brulee_tune_pra) %>% 
  add_recipe(recipe(formula = new_frml,
                    data = train_scaled,
                    stop_iter = 5
  )
  )

# here we use the repeated sub sampling to make what it is usually employed for: 
# hyperparameters optimisation. It is meant to select which ones are the 
# hyperparameters which perform better


# Here, in order to use the tune_grid function we used the manual_rset() function forcing the indeces to at the end mimic 
# our train validation and test split

# split = rsample::manual_rset(split)
indices <- list(
  list(analysis = seq(1,850), assessment = seq(851,1032))
)
splits <- lapply(indices, make_splits, data =  rbind(train_scaled, val_scaled))
manual_set = manual_rset(splits, c("Split 1"))

manual_set$splits[[3]]  # Inspect the split object


semi_sieve_cv <- 
  lapply(seq(1,1), 
         
         function(x){
           set.seed(x*756)
           tuned_model <- tune_grid(brulee_wflow,
                                    resamples = manual_set,
                                    grid = brulee_grid_II,
                                    control = control_resamples(save_pred = TRUE))
           
           
           
           # results combination and we extract the ones which are working better. 
           combined_data <- do.call(rbind, Map(cbind, tuned_model$.metrics, 
                                               round = paste0("round_", seq_along(tuned_model$.metrics))))
           return(combined_data)
           
         }
  )

combined_semi_sieve_cv_II <- do.call(rbind, Map(cbind, semi_sieve_cv, round = paste0("round_", seq_along(semi_sieve_cv))))
# save(combined_semi_sieve_cv, file = './01_Age_Structure/AAA_longer_timeSeries/results/xxxxxxxx.RData')



combined_semi_sieve_cv_II %>% 
  dplyr::select(-round) %>% 
  group_by(hidden_units,activation, learn_rate, .metric) %>% 
  # summarise( n =n_distinct(.estimate))
  summarise(avg_perf = mean(.estimate)) %>% 
  pivot_wider(names_from = .metric, values_from = avg_perf) %>% 
  arrange(rmse)




## 3.3 Run SIEVEd cleaned model ----
# new_frml = drop_formula_terms(frml,to_drop_clean)
# to_drop_clean %>% un
# 
# length(all.vars(new_frml))
# 

# test_speed

# start = Sys.time()
# for (k in c(1:1)) {
#   model_clean_basic = fit_brulee_mlp(
#     formula = new_frml,#auto
#     training_data=train_scaled,
#     num_neurons=7,
#     epochs_num = 125,
#     activation_fun = "elu",
#     seed_to_set = 845*k,
#     learning_rate = 0.2
#   )
# }
# Sys.time() - start


# this was used for the correction  
# epochs_init = 5
# rounds = 4
# for (i in 1:rounds) {
#   print(epochs_init^(rounds-(rounds-(i))))
# }

set.seed(123)
rand_samp <- sample(seq(1,50), 20)

start = Sys.time()


Sieve_fit_brulee_first_test_corct_funcs_under15_test20022025_corrStu <- lapply(rand_samp,function(z){
  Sieve_fit_brulee_mlp_20(train = train_scaled ,
                          test = val_scaled,
                          formula_mod = new_frml ,
                          num_neurons = 10, 
                          learning_rate = 0.193, 
                          activation_fun = 'elu', 
                          epochs_init = 5, rounds = 3, 
                          seed_basis = z)
}
)

Sys.time() - start

#save(Sieve_fit_brulee_first_test_corct_funcs_under15_test20022025_corrStu, file = './results/Sieve_fit_brulee_first_test_corct_funcs_under15_test20022025_corrStu.RData')
 load(file = './results/Sieve_fit_brulee_first_test_corct_funcs_under15_test20022025_corrStu.RData')
 lapply(Sieve_fit_brulee_first_test_corct_funcs_under15_test20022025_corrStu, function(x) sapply(x, length))

## 3.3 results analysis ----

# this is the first check which we make which is about checking what is the average of the R2 of
# the dufferent models which we fit

R2s <-  
  lapply( seq(1,20),function(x){ #pay attention here to how many models who fit. 
    lapply(seq(1,5), function(y)
    {
      fit <- Sieve_fit_brulee_first_test_corct_funcs_under15_test20022025_corrStu[[x]][[y]][[3]]
      
      mse_test =  mean(pull((
        predict(fit, test_scaled) 
        - test_scaled$y)^2))
      
      
      rmse_test = sqrt(mse_test)
      test_r2 = 1-(mse_test/var(test_scaled$y))
      return(test_r2)
    }
    )})
mean(unlist(R2s))



### results definition ----

# Considering how the SIEVE is concived, it is fundamental to have this stuff defined 
# for each model. This means that we have to create a data collection for each of the models 
# and then we need to make operations on the collection of all the predictions 

res_all <- 
  lapply(seq(1,20),function(x){ #also here, cheche how many models for each round you produced. 
    lapply(seq(1,5), function(y)
    {
      fit <- Sieve_fit_brulee_first_test_corct_funcs_under15_test20022025_corrStu[[x]][[y]][[3]]
      
      res = predict(fit, test_scaled)
      
      return(res)
    })})



# Assuming `res_all` is your list of lists
# Extract all vectors from the nested lists and combine them into a data frame

# Flatten the list of lists into a single list of vectors
flattened_list <- unlist(res_all, recursive = F)
# Combine the vectors into a data frame
result_df <- as.data.frame(do.call(cbind, flattened_list))
# Optional: Set meaningful column names
colnames(result_df) <- paste0("Pred", seq_along(flattened_list))



result_tbbl <- as_tibble(result_df)%>% 
  # cbind(geo = c(test_data %>% 
  #                 # dplyr::filter(age.group == 'under15') %>%
  #                 dplyr::select(geo) %>% pull()))%>% 
  rowwise() %>% 
  # mutate(row_median = median(c_across(-all_of("geo")), na.rm = TRUE)) %>%
  mutate(row_median = median(c_across(), na.rm = TRUE)) %>%
  
  ungroup()



assign(paste0('result_tbbl_', agroup), 
       as_tibble(result_df) %>% 
         rowwise() %>% 
         dplyr::mutate(row_median = median(c_across(), na.rm = TRUE)) %>% 
         dplyr::mutate(row_0025 = quantile(c_across(), probs = c(0.025), na.rm = TRUE)) %>% 
         dplyr::mutate(row_0975 = quantile(c_across(), probs = c(0.975), na.rm = TRUE)) %>% 
         mutate(age.group = agroup) %>% 
         ungroup() %>% 
         dplyr::select(row_median, row_0025, row_0975, age.group) %>% 
         cbind(test_data$y) %>% 
         mutate(id =  rank(`row_median`))) 

# 
# save(list = paste0('result_tbbl_', agroup), 
#      file = paste0('./01_Age_Structure/AAA_longer_timeSeries/results/', 
#                    paste0('result_tbbl_', agroup), '.RData'))
# 


ggplot(result_tbbl_under15, aes(x = id)) +
  # Add vertical error bars for the quantiles (2.5% and 97.5%)
   geom_errorbar(aes(ymin = row_0025, ymax = row_0975), width = 0.1, color = "#004D40") +
  # # Add a point for the median
  geom_point(aes(y = row_median), color = "#004D40", size = 1) +
  # Overlay the actual data points (test_data$y)
  geom_point(aes(y = `test_data$y`), color = "red", size = 2, shape = 4) +
  # Customize axis labels
  labs(x = "Observations", y = "Values", 
       title = "Caterpillar Plot with Median and Quantiles") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), # Hide x-axis labels
        axis.ticks.x = element_blank())  # Hide x-axis ticks



# this is the version where we condier the mean of the row as the estimate which is actually the thing which
# we are goign to use then in the proper modelling effort.

mse_test =  mean((result_tbbl$row_median  - test_data$y)^2)
rmse_test = sqrt(mse_test)
rmse_test
test_r2 = 1-(mse_test/var(test_data$y))
test_r2


# result_tbbl <- 
#   result_tbbl %>% 
#   cbind(y = standardized_data_test$y) %>% 
#   cbind(cluster = test_2020.2024_2015.2019_ctry %>% 
#           dplyr::filter(age.group == 'under15') %>% pull(cluster))




# Function to generate the ggplot
# plot_density_for_row <- function(data, row_index) {
#   # Check if the row index is valid
#   if (row_index < 1 || row_index > nrow(data)) {
#     stop("Invalid row index. Please choose a value between 1 and ", nrow(data))
#   }
#   
#   # Extract the specific row
#   selected_row <- data[row_index, ]
#   
#   # Gather all "Pred*" columns into a long format
#   pred_data <- selected_row %>%
#     select(starts_with("Pred")) %>%
#     pivot_longer(everything(), names_to = "Variable", values_to = "Value")
#   
#   # Extract the "y" and "row_median" values
#   y_value <- selected_row$y
#   row_median_value <- selected_row$row_median
#   
#   # Generate the density plot
#   ggplot(pred_data, aes(x = Value)) +
#     geom_density(fill = "gray", alpha = 0.5) +
#     geom_vline(aes(xintercept = y_value), color = "red", linetype = "dashed", size = 1) +
#     geom_vline(aes(xintercept = row_median_value), color = "blue", linetype = "dashed", size = 1) +
#     labs(
#       title = paste("Density Plot for Row", row_index),
#       x = "Value",
#       y = "Density"
#     ) +
#     theme_minimal()
# }
# plot_density_for_row(result_tbbl,1)

# I do not remember the meaning of this bit
# result_tbbl %>% 
#   dplyr::mutate(se_test =  (row_median  - test_data$lag1_quot_under15)^2) %>% 
#   # dplyr::group_by(cluster) %>% 
#   dplyr::summarise(rmse_test = sqrt(mean(se_test)),
#                    test_r2 = 1-(mean(se_test)/var(test_data$lag1_quot_under15)))


### rescaled results ----
# this means that we go from the nuts/ctry to the nuts proportion. It means that we look at how the performance looks
# like when we actually look at the proper quzantity we are interested in 

result_tbbl_rescaled <- 
  result_tbbl %>% 
  dplyr::select( "row_median") %>% 
  cbind(true_val = test_data$lag1_quot_under15) %>% #the variable need to be changed according to the age group.
  cbind( ctry_prop = 
           test_data %>% 
           # dplyr::filter(age.group == 'under15') %>% 
           dplyr::select(ctry_prop_under15) %>% pull()
  ) %>% 
  dplyr::mutate(row_median = row_median *  ctry_prop,
                  true_val = true_val *  ctry_prop #true_val was defined before.
  ) %>% 
  cbind(cluster = test_data %>% 
                  pull(cluster ) )




result_tbbl_rescaled %>% 
    dplyr::mutate(se_test =  (row_median  - true_val)^2) %>% 
 #  dplyr::group_by(cluster) %>% 
    dplyr::summarise(rmse_test = sqrt(mean(se_test)),
                       test_r2 = 1-(mean(se_test)/var(true_val)),
                           var = var(true_val))






mse_test =  mean((result_tbbl_rescaled$row_median  - result_tbbl_rescaled$true_val)^2)
rmse_test = sqrt(mse_test)
test_r2 = 1-(mse_test/var(result_tbbl_rescaled$true_val))

# mse_test_1 =  mean((repeater_rescaled$prop_2020__2024  - result_tbbl_rescaled$row_median)^2)
# test_r2_1 = 1-(mse_test_1/var(repeater_rescaled$prop_2020__2024))



# 4. Model's performance testing ----
# keeping into account that now to see the repeater we can not just take the value which we had in the previous bacth of data 

## 4.1 testing against REPEATER ----
# here we have to keep into account that we do not have the time dimension
# as explicit as we had it before. We rather have now mixed-up data for which we 
# have to identify the different lagged value for the different age groups.

mse_test =  mean(
  (
  test_data %>% 
    # dplyr::filter(geo %in% test_data$geo) %>% 
    dplyr::select(y) %>% pull()
    - 
    test_data$lag1_quot_under15
  )^2
  ) #maybe here we need to specify the right time frame.


rmse_test = sqrt(mse_test)
rmse_test
test_r2 = 1-(mse_test/var(test_data$y))
test_r2
# test_data$lag1_quot_under15[1]
# test_data$lag1_under15[1]/
# test_data$lag1_ctry_prop_under15[1]


## 4.2 testing Rescaled REPEATER ----
### XXXXX needs to be corrected with the new train valdiation test idea ----


 
# repeater_rescaled <-
#   cbind(
#     test_data %>%
#       dplyr::filter(age.group == '65+') %>%
#       dplyr::select(ctry_prop_65,y) %>%
#       dplyr::mutate(prop_2020__2024 = ctry_prop_65*y) %>%
#       dplyr::select(prop_2020__204)
#   ) %>%
#   cbind(
#     train_2015.2019_2010.2014_ctry %>%
#       dplyr::filter(geo %in% test_2020.2023_2015.2019_ctry$geo)  %>%
#       dplyr::filter(age.group == '65+') %>%
#       dplyr::select(y,geo) %>%
#       left_join(    test_2020.2023_2015.2019_ctry %>%
#                       dplyr::filter(age.group == '65+') %>%
#                       dplyr::select(ctry_prop_65,geo), by = 'geo')) %>%
#   dplyr::mutate(repeater_res_rescaled = ctry_prop_65*y)  %>%
#   dplyr::select(prop_2020__2023,repeater_res_rescaled,geo) %>%
#   as_tibble()
# 
# repeater_rescaled %>% head()
# 
# 
# 
# mse_test =  mean((repeater_rescaled$prop_2020__2023  - repeater_rescaled$repeater_res_rescaled)^2)
# rmse_test = sqrt(mse_test)
# test_r2 = 1-(mse_test/var(repeater_rescaled$prop_2020__2023))
# test_r2



## 4.3 testing against MEAN ----

# to do this we need to have the geo dimension because otherwise we are unable
# to group with the geo


mse_test =  mean((
train_data_raw %>% 
  dplyr::filter(geo %in% test_data_raw$geo) %>% 
  dplyr::select(geo,y) %>% 
  dplyr::group_by(geo) %>% 
  summarise(y_mean = mean(y)) %>% 
  inner_join(test_data_raw, by = 'geo') %>% 
  mutate(diff2 = (y-y_mean)^2) %>% 
  pull(diff2)))

rmse_test = sqrt(mse_test)
rmse_test
test_r2 = 1-(mse_test/var(test_data_raw$y))
test_r2

# 
# test_data$y[1:10]
# test_data_raw$y[1:10]
# 

## 4.4 mean rescaled ----
### XXXXX needs to be corrected with the new train valdiation test idea ----
# 
# 
# MEAN_rescaled <- 
#   left_join(  
#     
#     test_2020.2023_2015.2019_ctry %>% 
#       dplyr::filter(age.group == 'under15') %>% 
#       dplyr::select(ctry_prop_65,y,geo) %>% 
#       dplyr::mutate(prop_2020__2023 = ctry_prop_65*y) %>% 
#       dplyr::select(geo, prop_2020__2023),
#     
#     
#     train_2015.2019_2010.2014_ctry %>% 
#       dplyr::filter(geo %in% test_2020.2023_2015.2019_ctry$geo)  %>% 
#       dplyr::filter(age.group == 'under15') %>% 
#       dplyr::select(ctry_prop_65,y,geo) %>% 
#       dplyr::mutate(prop_2015__2019 = ctry_prop_65*y) %>% 
#       dplyr::select(geo, prop_2015__2019) %>% 
#       left_join( 
#         train_2010.2014_2005.2009_ctry %>% 
#           dplyr::filter(geo %in% test_2020.2023_2015.2019_ctry$geo)  %>% 
#           dplyr::filter(age.group == 'under15') %>% 
#           dplyr::select(ctry_prop_65,y,geo) %>% 
#           dplyr::mutate(prop_2010__2014 = ctry_prop_65*y) %>% 
#           dplyr::select(geo, prop_2010__2014), by = 'geo') %>% 
#       
#       left_join(
#         train_2005.2009_2000.2004_ctry %>% 
#           dplyr::filter(geo %in% test_2020.2023_2015.2019_ctry$geo)  %>% 
#           dplyr::filter(age.group == 'under15') %>% 
#           dplyr::select(ctry_prop_65,y,geo) %>% 
#           dplyr::mutate(prop_2005__2009 = ctry_prop_65*y) %>% 
#           dplyr::select(geo, prop_2005__2009), by = 'geo') %>% 
#       left_join(
#         train_2000.2004_1995.1999_ctry %>% 
#           dplyr::filter(geo %in% test_2020.2023_2015.2019_ctry$geo)  %>% 
#           dplyr::filter(age.group == 'under15') %>% 
#           dplyr::select(ctry_prop_65,y,geo) %>% 
#           dplyr::mutate(prop_2000__2004 = ctry_prop_65*y) %>% 
#           dplyr::select(geo, prop_2000__2004),by = 'geo') %>% 
#       dplyr::select("geo","prop_2015__2019","prop_2010__2014","prop_2005__2009","prop_2000__2004") %>% 
#       dplyr::rowwise() %>% 
#       dplyr::mutate(avg_2000_2019 = mean(c_across(-all_of("geo")), na.rm = TRUE)) %>% 
#       dplyr::select(geo,avg_2000_2019 ),  by = 'geo')
# 
# 
# 
# 
# mse_test =  mean((MEAN_rescaled$prop_2020__2023  - MEAN_rescaled$avg_2000_2019)^2)
# rmse_test = sqrt(mse_test)
# test_r2 = 1-(mse_test/var(MEAN_rescaled$prop_2020__2023))















