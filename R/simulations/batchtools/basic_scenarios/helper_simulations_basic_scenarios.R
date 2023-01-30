source("R/mob_fitting_functions.R")
source("R/simulations/batchtools/helper_simulations.R")

get_sim_results = function(data, job, instance, tree_methods = c("slim", "mob", "ctree", "guide"), n.quantiles = 100,
                           exclude.categoricals = FALSE, min.split = 50, maxdepth = 7, correct.bias = TRUE, approximate = FALSE,
                           pruning = "forward", impr.par = 0.1, alpha = 0.05, ... ){
  # The data used to compare the stability of the trees are identical across all replicates!
  data_stability = data[[as.character(job$prob.pars$type)]]
  
  # The data used to train the trees and evaluate their performance is re-simulated with each repetition.
  data = instance$data
  
  
  # -- standalone model 
  
  # deterministic train test split (to avoid variability in the algorithm)
  
  split_point = nrow(data)/3*2
  train = data[1:split_point,]
  test = data[(split_point+1):nrow(data),]
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
  result_original = cbind(surrogate = "standalone", result_original)
  

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
  
  result_surrogate_lm = rbind(result_surrogate_lm, c(mbt = "lm", n_leaves = list(NA), 
                                                     mse_train = list(mse_train_lm), r2_train = list(r2_train_lm), 
                                                     mse_test = list(mse_test_lm), r2_test = list(r2_test_lm),
                                                     stability = list(NA), stability_sem = list(NA)))
  result_surrogate_lm = cbind(surrogate = "lm", result_surrogate_lm)
  
  
  # -- surrogate 2 (xgboost model) 
  # train xgboost model (blackbox model 2)
  lrn = instance$lrn
  task_train = as_task_regr(x = train, target = "y")
  task_test = as_task_regr(x = test, target = "y")
  lrn$train(task_train)
  pred_xgboost_train = lrn$predict(task_train)
  pred_xgboost_test = lrn$predict(task_test)
  
  mse_train_xgboost = as.numeric(pred_xgboost_train$score(msr("regr.mse")))
  r2_train_xgboost = as.numeric(pred_xgboost_train$score(msr("regr.rsq")))
  mse_test_xgboost = as.numeric(pred_xgboost_test$score(msr("regr.mse")))
  r2_test_xgboost = as.numeric(pred_xgboost_test$score(msr("regr.rsq")))
  
  
  y_hat_train_xgboost = as.data.table(pred_xgboost_train)$response
  y_hat_test_xgboost = as.data.table(pred_xgboost_test)$response
  
  result_surrogate_xgboost  = fit_trees(x_train = x_train, y_train = y_hat_train_xgboost, x_test = x_test, y_test = y_hat_test_xgboost,  
                                        min.split = min.split, maxdepth = maxdepth, impr.par = impr.par, alpha = alpha, 
                                        pruning = pruning, approximate = approximate, n.quantiles = n.quantiles,
                                        exclude.categoricals = exclude.categoricals, correct.bias = correct.bias, 
                                        tree_methods = tree_methods, data_stability = data_stability)
  
  result_surrogate_xgboost = rbind(result_surrogate_xgboost, c(mbt = "xgboost", n_leaves = list(NA), 
                                                     mse_train = list(mse_train_xgboost), r2_train = list(r2_train_xgboost), 
                                                     mse_test = list(mse_test_xgboost), r2_test = list(r2_test_xgboost),
                                                     stability = list(NA), stability_sem = list(NA)))
  result_surrogate_xgboost = cbind(surrogate = "xgboost", result_surrogate_xgboost)
  
  res = rbind(result_original, result_surrogate_lm, result_surrogate_xgboost)
  res = cbind(type = as.character(job$prob.pars$type), n = nrow(data), alpha = alpha, impr = impr.par, res)
  
  return(res)
}


fit_trees = function(x_train, y_train, x_test, y_test, data_stability, min.split, 
                     maxdepth, impr.par, alpha, pruning, approximate,
                     n.quantiles, exclude.categoricals, correct.bias, tree_methods = c("slim", "mob", "ctree", "guide"),
                     extract_variables = FALSE){
  if("slim" %in% tree_methods){
    slim_res = list(mbt = "SLIM")
    
    slim = compute_tree_slim(y_train, x_train ,n.split = maxdepth - 1, pruning = pruning,  n.quantiles = n.quantiles,
                             impr.par = impr.par, min.split = min.split, approximate = approximate,
                             split.method = "slim")
    split = extract_split_criteria(slim)
    slim_res$n_leaves = sum(split$split.feature == "leafnode")
    
    slim_res$mse_train = mean((predict_slim(slim, x_train)- y_train)^2)
    slim_res$r2_train = r_2(y_train, predict_slim(slim, x_train))
    
    slim_res$mse_test = mean((predict_slim(slim, x_test)- y_test)^2)
    slim_res$r2_test = r_2(y_test, predict_slim(slim, x_test))
    
    if(extract_variables){
      slim_res$x1 = ifelse("x1" %in% unique(split[,"split.feature"]), TRUE, FALSE)
    }
    
    
    if(!is.null(data_stability)){
      # tree varies across all repetitions due to slightly different data, but data_stability is identical across all repetitions
      slim_res$stability = lapply(data_stability, function(dat){predict_slim(slim, dat, type = "node")})
      slim_res$stability_sem = lapply(data_stability, function(dat){predict_slim(slim, dat, type = "response")})
      
    }
    
    
  } 
  if("guide" %in% tree_methods){
    guide_res = list(mbt = "GUIDE")
    
    guide = compute_tree_slim(y_train, x_train ,n.split = maxdepth - 1, pruning = pruning, 
                              impr.par = impr.par, min.split = min.split, split.method = "guide",
                              exclude.categoricals = exclude.categoricals, correct.bias = correct.bias)
    split = extract_split_criteria(guide)
    
    guide_res$n_leaves = sum(split$split.feature == "leafnode")
    
    guide_res$mse_train = mean((predict_slim(guide, x_train)- y_train)^2)
    guide_res$r2_train = r_2(y_train, predict_slim(guide, x_train))
    
    guide_res$mse_test = mean((predict_slim(guide, x_test)- y_test)^2)
    guide_res$r2_test = r_2(y_test, predict_slim(guide, x_test))
    
    if(extract_variables){
      guide_res$x1 = ifelse("x1" %in% unique(split[,"split.feature"]), TRUE, FALSE)
    }
    
    if(!is.null(data_stability)){
      guide_res$stability = lapply(data_stability, function(dat){predict_slim(guide, dat, type = "node")})
      guide_res$stability_sem = lapply(data_stability, function(dat){predict_slim(guide, dat, type = "response")})
      
    }
    
    
  } 
  if("mob" %in% tree_methods){
    mob_res = list(mbt = "MOB")
    # formula mob
    fm_mob = formula(paste("y ~", paste(colnames(x_test), collapse = "+"), "|", paste(colnames(x_test), collapse = "+")))
    
    mob = lmtree(fm_mob, 
                 data = cbind(x_train, y = y_train), 
                 minsize = min.split, 
                 maxdepth = maxdepth, 
                 alpha = alpha)
    mob_res$n_leaves = width(mob)
    
    mob_res$mse_train = mean((predict(mob, x_train)- y_train)^2)
    mob_res$r2_train = r_2(y_train, predict(mob, x_train))
    
    mob_res$mse_test = mean((predict(mob, x_test)- y_test)^2)
    mob_res$r2_test = r_2(y_test, predict(mob, x_test))
    
    if(extract_variables){
      mobrule = partykit:::.list.rules.party(mob)
      mob_res$x1 = ifelse("x1" %in% unique(unlist(str_extract_all(mobrule,"(x+[1-9])"))), TRUE, FALSE)

    }
    
    if(!is.null(data_stability)){
      mob_res$stability = lapply(data_stability, function(dat){as.character(predict(mob, dat, type = "node"))})
      mob_res$stability_sem = lapply(data_stability, function(dat){as.character(predict(mob, dat, type = "response"))})
      
    }
    
  } 
  if("ctree" %in% tree_methods){
    ctree_res = list(mbt = "CTree")
    fm_ctree = formula(paste("y ~", paste(colnames(x_test), collapse = "+"), "|", paste(colnames(x_test), collapse = "+")))
    ctree = suppressWarnings(partykit::ctree(fm_ctree,
                                             data = cbind(x_train, y = y_train),
                                             ytrafo = fit_lm,
                                             control = partykit::ctree_control(minbucket = min.split, maxdepth = maxdepth - 1, alpha = alpha)))
    
    ctree_res$n_leaves =  width(ctree)
    
    fit_ctree = fit_ctree_leaves(ctree, x_train, y_train)
    
    ctree_res$mse_train = mean((predict_ctree(ctree, fit_ctree, x_train)- y_train)^2)
    ctree_res$r2_train = r_2(y_train, predict_ctree(ctree, fit_ctree, x_train))
    
    ctree_res$mse_test = mean((predict_ctree(ctree, fit_ctree, x_test)- y_test)^2)
    ctree_res$r2_test = r_2(y_test, predict_ctree(ctree, fit_ctree, x_test))
    
    
    if(extract_variables){
      ctreerule = partykit:::.list.rules.party(ctree)
      ctree_res$x1 = ifelse("x1" %in% unique(unlist(str_extract_all(ctreerule,"(x+[1-9])"))), TRUE, FALSE)
    }
    
    
    if(!is.null(data_stability)){
      ctree_res$stability = lapply(data_stability, function(dat){as.character(predict(ctree, dat, type = "node"))})
      ctree_res$stability_sem = lapply(data_stability, function(dat){as.character(predict_ctree(ctree, fit_ctree, dat))})
      
    }
  }
  
  res = rbind(slim_res, guide_res, mob_res, ctree_res)
  
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
  rss <- sum((y_hat - y_true) ^ 2)  ## residual sum of squares
  tss <- sum((y_true - mean(y_true)) ^ 2)  ## total sum of squares
  r_2 <- 1 - rss/tss
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

