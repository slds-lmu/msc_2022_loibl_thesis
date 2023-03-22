## batchtools correlation

get_sim_results_corr = function(data, job, instance, 
                                n.quantiles = 100, min.split = 50, maxdepth = 7, 
                                approximate = FALSE, pruning = "forward", impr.par = 0.1, alpha = 0.001, ... ){
  

  # The data used to train the trees and evaluate their performance is re-simulated with each repetition.
  data = instance$data
  
  
  # -- standalone model 
  
  # train test split
  split_point = nrow(data)/3*2
  train = data[1:split_point,]
  test = data[(split_point+1):nrow(data),]
  x_train = train[, colnames(train) != "y"]
  x_test = test[, colnames(test) != "y"]
  
  x_train = train[, colnames(train) != "y"]
  x_test = test[, colnames(test) != "y"]
  
  y_train = train$y
  y_test = test$y
  
  # fit trees to the original data
  result_original = fit_trees(x_train = x_train, y_train = y_train, x_test = x_test, y_test = y_test,  
                              min.split = min.split, maxdepth = maxdepth, impr.par = impr.par, alpha = alpha, 
                              pruning = pruning, approximate = approximate, n.quantiles = n.quantiles,
                              exclude.categoricals = FALSE, correct.bias = TRUE, 
                              data_stability = NULL, 
                              extract_variables = TRUE,  tree_methods = c("slim", "mob", "ctree", "guide", "slim_ridge"))
  result_original = cbind(surrogate = "standalone", result_original)
  
  # train linear model (blackbox model 1)
  fm = instance$fm
  lm = gam(fm, data = train)
  
  # extract fitted values (surrogates)
  y_hat_train_lm = lm$fitted.values
  y_hat_test_lm = predict(lm, x_test)
  
  # calculate performance of the lm model (as benchmark for the accuracy of the MBT models)
  mse_train_lm = mean((y_train - y_hat_train_lm)^2)
  r2_train_lm = r_2(y_train, y_hat_train_lm)
  mse_test_lm = mean((y_test - y_hat_test_lm)^2)
  r2_test_lm = r_2(y_test, y_hat_test_lm)
  
  # fit trees to the lm predictions 
  result_surrogate_lm  = fit_trees(x_train = x_train, y_train = y_hat_train_lm, x_test = x_test, y_test = y_hat_test_lm,  
                                   min.split = min.split, maxdepth = maxdepth, impr.par = impr.par, alpha = alpha, 
                                   pruning = pruning, approximate = approximate, n.quantiles = n.quantiles,
                                   exclude.categoricals = FALSE, correct.bias = TRUE, 
                                   data_stability = NULL, 
                                   extract_variables = TRUE,  tree_methods = c("slim", "mob", "ctree", "guide", "slim_ridge"))
  
  
  result_surrogate_lm = cbind(surrogate = "lm", result_surrogate_lm)
  res = cbind(type = as.character(job$prob.pars$type), n = nrow(data), rho = job$prob.pars$rho, biased = job$prob.pars$biased, 
              rbind(result_original, result_surrogate_lm))
  
  return(res)

 
}

marginals_copula = function(cor_matrix, list_distributions, n){
  l = length(list_distributions)
  # Correlated Gaussian variables
  Gauss = rmvnorm(n=n, mean = rep(0,l), sig=cor_matrix)
  # convert them to uniform distribution.
  Unif = pnorm(Gauss) 
  # Convert them to whatever I want
  vars = sapply(1:l, FUN = function(i) list_distributions[[i]](Unif[,i]))
  return(vars)
}