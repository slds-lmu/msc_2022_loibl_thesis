## batchtools correlation

get_sim_results_corr = function(data, job, instance, 
                                n.quantiles = 100, min.split = 50, maxdepth = 7, 
                                approximate = FALSE, pruning = "forward", impr.par = 0.1, alpha = 0.001, ... ){
  

  # The data used to train the trees and evaluate their performance is re-simulated with each repetition.
  data = instance$data
  
  
  # -- standalone model 
  
  # train test split
  sample = sample.split(data$y, SplitRatio = 2/3)
  train = subset(data, sample == TRUE)
  test = subset(data, sample == FALSE)
  
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
                              extract_variables = TRUE)
  result_original = cbind(surrogate = "standalone", result_original)
  res = cbind(type = as.character(job$prob.pars$type), n = nrow(data), rho = job$prob.pars$rho, biased = job$prob.pars$biased, result_original)
  
  
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