source("R/mob_fitting_functions.R")
source("R/simulations/batchtools/helper_simulations.R")

get_sim_results = function(data, job, instance, tree_methods = c("slim", "mob", "ctree", "guide"), n.quantiles = 100,
                           exclude.categoricals = FALSE, min.split = 50, maxdepth = 7, correct.bias = TRUE, approximate = FALSE,
                           pruning = "forward", impr.par = 0.1, alpha = 0.05, ... ){
  
  # The data used to compare the stability of the trees are identical across all replicates!
  data_stability = data[[job$prob.pars$type]]
  
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
  
  # fit trees to the original data (i.e. use MBTs as standalone ML model)
  result_original = fit_trees(x_train = x_train, y_train = y_train, x_test = x_test, y_test = y_test,  
                              min.split = min.split, maxdepth = maxdepth, impr.par = impr.par, alpha = alpha, 
                              pruning = pruning, approximate = approximate, n.quantiles = n.quantiles,
                              exclude.categoricals = exclude.categoricals, correct.bias = correct.bias, 
                              tree_methods = tree_methods, data_stability = data_stability)
  

  # -- surrogate 1 (correctly specified linear model) 

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
                                   exclude.categoricals = exclude.categoricals, correct.bias = correct.bias, 
                                   tree_methods = tree_methods, data_stability = data_stability)
  
  
  
  # -- surrogate 2 (xgboost model) 
  # train xgboost model (blackbox model 2)
  lrn = instance$lrn
  task_train = as_task_regr(x = train, target = "y")
  task_test = as_task_regr(x = test, target = "y")
  lrn$train(task_train)
  pred_xgboost_train = lrn$predict(task_train)
  pred_xgboost_test = lrn$predict(task_test)
  
  mse_train_xgboost = pred_xgboost_train$score(msr("regr.mse"))
  r2_train_xgboost = pred_xgboost_train$score(msr("regr.rsq"))
  mse_test_xgboost = pred_xgboost_test$score(msr("regr.mse"))
  r2_test_xgboost = pred_xgboost_test$score(msr("regr.rsq"))
  
  
  y_hat_train_xgboost = as.data.table(pred_xgboost_train)$response
  y_hat_test_xgboost = as.data.table(pred_xgboost_test)$response
  result_surrogate_xgboost  = fit_trees(x_train = x_train, y_train = y_hat_train_xgboost, x_test = x_test, y_test = y_hat_test_xgboost,  
                                        min.split = min.split, maxdepth = maxdepth, impr.par = impr.par, alpha = alpha, 
                                        pruning = pruning, approximate = approximate, n.quantiles = n.quantiles,
                                        exclude.categoricals = exclude.categoricals, correct.bias = correct.bias, 
                                        tree_methods = tree_methods, data_stability = data_stability)
  
  
  
  return(list(standalone = result_original, lm = result_surrogate_lm, xgboost = result_surrogate_xgboost))
}


fit_trees = function(x_train, y_train, x_test, y_test, data_stability, min.split, 
                     maxdepth, impr.par, alpha, pruning, approximate,
                     n.quantiles, exclude.categoricals, correct.bias, tree_methods){
  res = list()
  
  if("slim" %in% tree_methods){
    res$slim = list()
    
    slim = compute_tree_slim(y_train, x_train ,n.split = maxdepth - 1, pruning = pruning,  n.quantiles = n.quantiles,
                             impr.par = impr.par, min.split = min.split, approximate = approximate,
                             split.method = "slim")
    res$slim$split = extract_split_criteria(slim)
    res$slim$n_leaves = sum(res$slim$split$split.feature == "leafnode")
    
    res$slim$mse_train = mean((predict_slim(slim, x_train)- y_train)^2)
    res$slim$r2_train = r_2(y_train, predict_slim(slim, x_train))
    
    res$slim$mse_test = mean((predict_slim(slim, x_test)- y_test)^2)
    res$slim$r2_test = r_2(y_test, predict_slim(slim, x_test))
    
    
    if(!is.null(data_stability)){
      # tree varies across all repetitions due to slightly different data, but data_stability is identical across all repetitions
      res$slim$stability = predict_slim(slim, data_stability, type = "node")
    }
    
    
  } 
  if("guide" %in% tree_methods){
    res$guide = list()
    
    guide = compute_tree_slim(y_train, x_train ,n.split = maxdepth - 1, pruning = pruning, 
                              impr.par = impr.par, min.split = min.split, split.method = "guide",
                              exclude.categoricals = exclude.categoricals, correct.bias = correct.bias)
    res$guide$split = extract_split_criteria(guide)
    res$guide$n_leaves = sum(res$guide$split$split.feature == "leafnode")
    
    res$guide$mse_train = mean((predict_slim(guide, x_train)- y_train)^2)
    res$guide$r2_train = r_2(y_train, predict_slim(guide, x_train))
    
    res$guide$mse_test = mean((predict_slim(guide, x_test)- y_test)^2)
    res$guide$r2_test = r_2(y_test, predict_slim(guide, x_test))
    
    
    if(!is.null(data_stability)){
      res$guide$stability = predict_slim(guide, data_stability, type = "node")
    }
    
    
  } 
  if("mob" %in% tree_methods){
    res$mob = list()
    # formula mob
    fm_mob = formula(paste("y ~", paste(colnames(x_test), collapse = "+"), "|", paste(colnames(x_test), collapse = "+")))
    
    mob = lmtree(fm_mob, data = cbind(x_train, y = y_train), minsize = min.split, maxdepth = maxdepth, alpha = alpha)
    res$mob$n_leaves = width(mob)
    
    res$mob$mse_train = mean((predict(mob, x_train)- y_train)^2)
    res$mob$r2_train = r_2(y_train, predict(mob, x_train))
    
    res$mob$mse_test = mean((predict(mob, x_test)- y_test)^2)
    res$mob$r2_test = r_2(y_test, predict(mob, x_test))
    
    if(!is.null(data_stability)){
      res$mob$stability = as.character(predict(mob, data_stability, type = "node"))
    }
    
  } 
  if("ctree" %in% tree_methods){
    res$ctree = list()
    
    fm_ctree = formula(paste("y ~", paste(colnames(x_test), collapse = "+"), "|", paste(colnames(x_test), collapse = "+")))
    ctree = suppressWarnings(ctree(fm_mob,
                                   data = data.frame(x_train, y = y_train),
                                   ytrafo = fit_lm,
                                   control = ctree_control(minbucket = min.split, maxdepth = maxdepth - 1, alpha = alpha)))
    
    res$ctree$n_leaves =  width(ctree)
    
    fit_ctree = fit_ctree_leaves(ctree, x_train, y_train)
    
    res$ctree$mse_train = mean((predict_ctree(ctree, fit_ctree, x_train)- y_train)^2)
    res$ctree$r2_train = r_2(y_train, predict_ctree(ctree, fit_ctree, x_train))
    
    res$ctree$mse_test = mean((predict_ctree(ctree, fit_ctree, x_test)- y_test)^2)
    res$ctree$r2_test = r_2(y_test, predict_ctree(ctree, fit_ctree, x_test))
    
    if(!is.null(data_stability)){
      res$ctree$stability = as.character(predict(ctree, data_stability, type = "node"))
    }
  }
  
  return(res)
} 






# fit models to ctree leaves
fit_ctree_leaves = function(ctree, x, y){
  node_model = cbind(x, y = y, node = predict(ctree, type = "node"))
  node_model_list = split(node_model, node_model$node, drop = TRUE)
  node_model_list = lapply(node_model_list, function(node){
    x = node[, !(colnames(node) %in% c("y", "node"))]
    x = x %>% dplyr::select(where(~ n_distinct(.) > 1))
    fm = formula(paste("y ~ ", paste(colnames(x), collapse = "+")))
    lm(fm, data = node)
  })
  return(node_model_list)
}

# calculate r squared
r_2 = function(y_true, y_hat){
  ess <- sum((y_hat - mean(y_true)) ^ 2)  ## error sum of squares
  tss <- sum((y_true - mean(y_true)) ^ 2)  ## total sum of squares
  r_2 <- ess/tss
  return(r_2)
}


# get model predictions for ctree
predict_ctree = function(ctree, fit_ctree, newdata){
  newdata$row_id = 1:nrow(newdata)
  nodes = predict(ctree, newdata = newdata, type = "node")
  newdata_list = split(newdata, nodes)
  for(node in names(newdata_list)){
    newdata_list[[node]]$y_hat = predict(fit_ctree[[node]], newdata = newdata_list[[node]])
  }
  
  predictions = lapply(newdata_list, function(el) el[, c("row_id", "y_hat")])
  predictions = do.call(rbind, predictions)
  predictions = predictions[order(predictions$row_id),]
  rownames(predictions) = predictions$row_id
  return(predictions$y_hat)
}

