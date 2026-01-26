plot_inequality <- function(x){
  M <- 6:150
  n <- 2:250
  combinations <- expand.grid(M, n)
  colnames(combinations) = c('M','n')
  valid_combinations <- subset(combinations,(1/10*(((x-10)/n)-20)>M))
  ggplot(data = valid_combinations, aes(x = M, y = n)) +
    geom_point(color = "blue") +
    scale_x_continuous(name = "M (covariates)", breaks = seq(min(valid_combinations$M), max(valid_combinations$M))) +
    scale_y_continuous(name = "n (neurons)", breaks = seq(min(valid_combinations$n), max(valid_combinations$n))) +
    geom_vline(xintercept = unique(valid_combinations$M), color = "gray", linetype = "dashed") +
    labs(title = "Combinations respecting the rule of thumb")+
    theme_bw()+
    theme(axis.text.x = element_text(size = 4))
}



fit_brulee_mlp <- function(training_data, formula, num_neurons, learning_rate, epochs_num, activation_fun, seed_to_set) {
  # here we defint eh recipe and we assume the data to be already trasformed as necessary ! 
  rece <-
    recipe(formula,
           data = training_data)
  
  set.seed(seed_to_set)
  fit <- brulee_mlp(rece,
                    activation = activation_fun,
                    data = training_data,
                    hidden_units = num_neurons,
                    epochs = epochs_num,
                    verbose = FALSE,
                    learn_rate = learning_rate,
                    stop_iter = 10,
                    validation = 0
                    )
  
  return(fit)
}



extract_lowest <- function(my_list, n) {
  if (length(my_list) == 0) {
    stop("The input list is empty.")
  }
  # Extract the second elements from each pair in the list
  second_elements <- sapply(my_list, function(x) x[[2]])
  
  # Find the indices of the 125 lowest elements
  lowest_indices <- order(second_elements)[1:n] 
  
  # Extract the 125 elements from the original list
  selected_elements <- my_list[lowest_indices]
  
  return(selected_elements)
}



Sieve_fit_brulee_mlp_20 <- function(train ,test , formula_mod ,
                                 num_neurons, learning_rate , activation_fun , 
                                 epochs_init, rounds , seed_basis ){

  MAEs <- list()
  candidate <- list()  
  print(paste0("round: ", 1, " I run ", epochs_init, " epochs"))
  # the first round is not "competitive". The others are.
  MAEs[[1]] <-
    lapply(seq(1, epochs_init^(epochs_init-1)),
           function(x) {
             print(seed_basis*x)
             fit_b <- fit_brulee_mlp(training_data = train,
                                     formula_mod,
                                     num_neurons,
                                     learning_rate,
                                     epochs_init,
                                     activation_fun,
                      
                                     seed_to_set = seed_basis * x)
             
             MAE = Metrics::mae(pull(predict(fit_b, test)),test$y)+Metrics::mae(pull(predict(fit_b, train)),train$y)
             
             res_list <- list(seed_basis * x,MAE)
             return(res_list)
           }
           
    )
  
  candidate[[1]] <-  extract_lowest(MAEs[[1]], epochs_init^(epochs_init-2))
  print(paste0("I finished the first round, I did ", epochs_init^(epochs_init-1), " models and i selected seeds ",
        sapply(candidate[[1]], function(x) x[[1]])))
  for (i in 2:rounds) { print(paste0("round: ", i, " I run ", epochs_init^(rounds-(rounds-(i))), " epochs"))
                        
  if(i == rounds){
  MAEs[[i]] <-
    lapply(sapply(candidate[[i-1]] , function(x) x[[1]]),
           function(x) {
             print(x)
             fit_b <- fit_brulee_mlp(training_data = train,
                                     formula_mod,
                                     num_neurons,
                                     learning_rate,
                                     # epochs_init^(rounds-(rounds-(i+1))),
                                     epochs_init^(rounds-(rounds-(i))),
                                     
                                     activation_fun,
                                     
                                     seed_to_set = x) # here the meaning of x, changes.
             
             
             MAE = Metrics::mae(pull(predict(fit_b, test)),test$y)+Metrics::mae(pull(predict(fit_b, train)),train$y)
             
             res_list <- list(x,MAE, fit_b)
             return(res_list)
           }
           
    )
  
  
} else {MAEs[[i]] <-
      lapply(sapply(candidate[[i-1]] , function(x) x[[1]]),
             function(x) {
               print(x)
               fit_b <- fit_brulee_mlp(training_data = train,
                                       formula_mod,
                                       num_neurons,
                                       learning_rate,
                                       # epochs_init^(rounds-(rounds-(i+1))),
                                       epochs_init^(rounds-(rounds-(i))),
                                       
                                       activation_fun,

                                       seed_to_set = x)
               

               MAE = Metrics::mae(pull(predict(fit_b, test)),test$y)+Metrics::mae(pull(predict(fit_b, train)),train$y)

               res_list <- list(x,MAE)
               return(res_list)
             }

      )
}

    candidate[[i]] <-  extract_lowest(MAEs[[i]], epochs_init^(epochs_init-(i+1))  )
    print(paste0("I finished round ", i ," I did ", length(candidate[[i-1]]), " models and I selected seeds ",
                 sapply(candidate[[i]], function(x) x[[1]])))
    
  }

  return(candidate[[rounds]])
  }


# 
#  test_fast <- Sieve_fit_brulee_mlp_20(
#    
#    train = full_trainingData[,c('y',predictor.variable.names.new$selected.variables)] ,
#    test = standardized_data_test[,c('y',predictor.variable.names.new$selected.variables)],
#    formula_mod = new_frml ,
#    num_neurons = 8 ,
#    learning_rate = 0.22,  
#    activation_fun = 'elu',
#    epochs_init = 3 ,
#    rounds = 3  ,
#    seed_basis = 1234567)
# 
# 
# 
#  test_complete_sieve <- Sieve_fit_brulee_mlp_20(train = train_data,test =  test_data, formula_mod = frml,
#                                                 num_neurons= 4, learning_rate = 0.1, activation_fun = 'elu', 
#                                                 epochs_init = 5, rounds = 3, seed_basis = 100)
# 
# 
#  
# 
# 
#  fit_brulee_mlp(training_data = train_data,formula = frml,
#                 num_neurons= 4, learning_rate = 0.1, activation_fun = 'linear', 
#                 epochs_num = 5, seed_to_set  = 100)
#  
#  
#  
#  rece <-
#    recipe(frml,
#           data = train_data)
#  
#  set.seed(100)
#  
#  fit <- brulee_mlp(rece,
#                    data = train_data,
#                    hidden_units = 8,
#                    epochs = 3,
#                    verbose = FALSE,
#                    learn_rate = 0.22,
#                    stop_iter = 5
#  )
#  
#  
#  
#  fit$parameters
#  
#  
#  
#  
#  
#  
#  fit <- test_complete_sieve[[1]][[3]]
#  test_complete_sieve[[1]][[2]]
# 
# 
#   Metrics::mae(pull(predict(fit, test_data)),test_data$y)+Metrics::mae(pull(predict(fit, train_data)),train_data$y)
# 
#   
#   terms(frml)
# 
# 
# 
# MAEs[[2]] <-
#   lapply(sapply(candidate[[1]] , function(x) x[[1]]),
#          function(x) {
#            fit_b <- 
#              fit_brulee_mlp(train_data, frml, 4, learning_rate, epochs_init^(rounds-1), activation_fun, x)
#            MAE = Metrics::mae(pull(predict(fit_b, test_data)),test_data$y)+Metrics::mae(pull(predict(fit_b, train_data)),train_data$y)
#            
#            res_list <- list(x,MAE)
#            return(res_list)
#          }
#          
#   )
# 
# candidate[[2]] <-  extract_lowest(MAEs[[2]], epochs_init^(rounds-1))
# MAEs[[3]] <-
#   lapply(sapply(candidate[[2]] , function(x) x[[1]]),
#          function(x) {
#            fit_b <- 
#              fit_brulee_mlp(train_data, frml, 4, learning_rate, epochs_init^(rounds), activation_fun, x)
#            MAE = Metrics::mae(pull(predict(fit_b, test_data)),test_data$y)+Metrics::mae(pull(predict(fit_b, train_data)),train_data$y)
#            
#            res_list <- list(x,MAE)
#            return(res_list)
#          }
#          
#   )
# candidate[[3]] <-  extract_lowest(MAEs[[3]], epochs_init^(rounds-2))
# 

