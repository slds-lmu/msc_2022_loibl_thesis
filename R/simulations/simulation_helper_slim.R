source("R/load_packages.R")
source("R/helper_general.R")
source("R/mob_fitting_functions.R")
source("R/tree_splitting_slim.R")
source("R/simulations/simulation_setting_definition_slim.R")
library(caTools)


# helper functions simulation


# fit models to ctree leaves
fit_ctree_leaves = function(ctree, x, y){
  node_model = cbind(x, y = y, node = predict(ctree, type = "node"))
  node_model_list = split(node_model, node_model$node, drop = TRUE)
  node_model_list = lapply(node_model_list, function(node){
    x = node[, !(colnames(node) %in% c("y", "node"))]
    x = x %>% select(where(~ n_distinct(.) > 1))
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

fit_trees = function(x_train, y_train, x_test, y_test, minsize, maxdepth, impr, alpha, pruning, approximate){
  
  print("slim")
  slim = compute_tree_slim(y_train, x_train ,n.split = maxdepth - 1, pruning = pruning, 
                           impr.par = impr, min.split = minsize, approximate = approximate)
  split_slim = extract_split_criteria(slim)
  n_term_nodes_slim = sum(split_slim$split.feature == "leafnode")
  rule_slim = split_slim$child.type[split_slim$split.feature == "leafnode"]
  
  mse_train_slim = mean((predict_slim(slim, x_train)- y_train)^2)
  r2_train_slim = r_2(y_train, predict_slim(slim, x_train))
  
  mse_test_slim = mean((predict_slim(slim, x_test)- y_test)^2)
  r2_test_slim = r_2(y_test, predict_slim(slim, x_test))
  
  print("anova")
  slim_aov = compute_tree_slim(y_train, x_train ,n.split = maxdepth - 1, pruning = pruning, 
                              impr.par = impr, min.split = minsize, approximate = approximate, split.method = "anova")
  split_slim_aov = extract_split_criteria(slim_aov)
  n_term_nodes_slim_aov = sum(split_slim_aov$split.feature == "leafnode")
  rule_slim_aov = split_slim_aov$child.type[split_slim_aov$split.feature == "leafnode"]
  
  mse_train_slim_aov = mean((predict_slim(slim_aov, x_train)- y_train)^2)
  r2_train_slim_aov = r_2(y_train, predict_slim(slim_aov, x_train))
  
  mse_test_slim_aov = mean((predict_slim(slim_aov, x_test)- y_test)^2)
  r2_test_slim_aov = r_2(y_test, predict_slim(slim_aov, x_test))
  
  print("R2")
  slim_r2 = compute_tree_slim(y_train, x_train ,n.split = maxdepth - 1, pruning = pruning, 
                                 impr.par = impr, min.split = minsize, approximate = approximate, split.method = "R2")
  split_slim_r2 = extract_split_criteria(slim_r2)
  n_term_nodes_slim_r2 = sum(split_slim_r2$split.feature == "leafnode")
  rule_slim_r2 = split_slim_r2$child.type[split_slim_r2$split.feature == "leafnode"]
  
  mse_train_slim_r2 = mean((predict_slim(slim_r2, x_train)- y_train)^2)
  r2_train_slim_r2 = r_2(y_train, predict_slim(slim_r2, x_train))
  
  mse_test_slim_r2 = mean((predict_slim(slim_r2, x_test)- y_test)^2)
  r2_test_slim_r2 = r_2(y_test, predict_slim(slim_r2, x_test))
  
  print("R2_adj")
  slim_r2_adj = compute_tree_slim(y_train, x_train ,n.split = maxdepth - 1, pruning = pruning, 
                              impr.par = impr, min.split = minsize, approximate = approximate, split.method = "R2_adj")
  split_slim_r2_adj = extract_split_criteria(slim_r2_adj)
  n_term_nodes_slim_r2_adj = sum(split_slim_r2_adj$split.feature == "leafnode")
  rule_slim_r2_adj = split_slim_r2_adj$child.type[split_slim_r2_adj$split.feature == "leafnode"]
  
  mse_train_slim_r2_adj = mean((predict_slim(slim_r2_adj, x_train)- y_train)^2)
  r2_train_slim_r2_adj = r_2(y_train, predict_slim(slim_r2_adj, x_train))
  
  mse_test_slim_r2_adj = mean((predict_slim(slim_r2_adj, x_test)- y_test)^2)
  r2_test_slim_r2_adj = r_2(y_test, predict_slim(slim_r2_adj, x_test))
  
  
  # mob
  # formula mob
  fm_mob = formula(paste("y ~", paste(colnames(x_test), collapse = "+"), "|", paste(colnames(x_test), collapse = "+")))
  
  mob = lmtree(fm_mob, data = cbind(x_train, y = y_train), minsize = minsize, maxdepth = maxdepth, alpha = alpha)
  n_term_nodes_mob = width(mob)
  rule_mob = partykit:::.list.rules.party(mob)
  
  
  mse_train_mob = mean((predict(mob, x_train)- y_train)^2)
  r2_train_mob = r_2(y_train, predict(mob, x_train))
  
  mse_test_mob = mean((predict(mob, x_test)- y_test)^2)
  r2_test_mob = r_2(y_test, predict(mob, x_test))
  
  
  # ctree
  ctree = suppressWarnings(ctree(fm_mob,
                                 data = data.frame(x_train, y = y_train),
                                 ytrafo = fit_lm,
                                 control = ctree_control(minbucket = minsize, maxdepth = maxdepth - 1, alpha = alpha)))
  
  n_term_nodes_ctree =  width(ctree)
  rule_ctree = partykit:::.list.rules.party(ctree)
  
  fit_ctree = fit_ctree_leaves(ctree, x_train, y_train)
  
  mse_train_ctree = mean((predict_ctree(ctree, fit_ctree, x_train)- y_train)^2)
  r2_train_ctree = r_2(y_train, predict_ctree(ctree, fit_ctree, x_train))
  
  mse_test_ctree = mean((predict_ctree(ctree, fit_ctree, x_test)- y_test)^2)
  r2_test_ctree = r_2(y_test, predict_ctree(ctree, fit_ctree, x_test))
  
  return(list(n_term_nodes_slim = n_term_nodes_slim, n_term_nodes_slim_aov = n_term_nodes_slim_aov, n_term_nodes_slim_r2 = n_term_nodes_slim_r2, n_term_nodes_slim_r2_adj = n_term_nodes_slim_r2_adj,  n_term_nodes_mob = n_term_nodes_mob, n_term_nodes_ctree = n_term_nodes_ctree,
              rule_slim = rule_slim, rule_slim_aov = rule_slim_aov, rule_slim_r2 = rule_slim_r2, rule_slim_r2_adj = rule_slim_r2_adj, rule_ctree = rule_ctree, rule_mob = rule_mob,
              mse_train_slim = mse_train_slim, mse_train_slim_aov = mse_train_slim_aov, mse_train_slim_r2 = mse_train_slim_r2, mse_train_slim_r2_adj = mse_train_slim_r2_adj, mse_train_mob = mse_train_mob, mse_train_ctree = mse_train_ctree,
              mse_test_slim = mse_test_slim, mse_test_slim_aov = mse_test_slim_aov, mse_test_slim_r2 = mse_test_slim_r2, mse_test_slim_r2_adj = mse_test_slim_r2_adj, mse_test_mob = mse_test_mob, mse_test_ctree = mse_test_ctree,
              r2_train_slim = r2_train_slim, r2_train_slim_aov = r2_train_slim_aov, r2_train_slim_r2 = r2_train_slim_r2, r2_train_slim_r2_adj = r2_train_slim_r2_adj, r2_train_mob = r2_train_mob, r2_train_ctree = r2_train_ctree,
              r2_test_slim = r2_test_slim, r2_test_slim_aov = r2_test_slim_aov, r2_test_slim_r2 = r2_test_slim_r2, r2_test_slim_r2_adj = r2_test_slim_r2_adj, r2_test_mob = r2_test_mob, r2_test_ctree = r2_test_ctree))
}



# function to run different simulation scenarios

generate_data_sim = function(scenario, type = "performance", rep, n, minsize = 20, maxdepth = 3, impr = 0.1, alpha = 0.05, pruning = "forward", approximate = FALSE){
  
  pb = txtProgressBar(min = 0, max = rep, initial = 0) 
  
  mse_train_lm = c()
  r2_train_lm = c()
  mse_test_lm = c()
  r2_test_lm = c()
  
  mse_train_xgboost = c()
  r2_train_xgboost = c()
  mse_test_xgboost = c()
  r2_test_xgboost = c()
  
  result_original = list()
  result_surrogate_lm = list()
  result_surrogate_xgboost = list()
  
  if (type == "stability") {
    simulation_training_ml = create_sim_data_slim(n*3, type = scenario)
    data = simulation_training_ml$data
    # train linear model (and use it as black box model for all further simulations)
    fm_training_lm = simulation_training_ml$fm
    lm = gam(fm_training_lm, data = data)
    y_hat_lm = predict(lm, data[, colnames(data)!="y"])
    
    mse_lm = mean((data[,"y"] - y_hat_lm)^2)
    r2_lm = r_2(data[,"y"], y_hat_lm)
    
    
    # Train xgboost model
    
    task = as_task_regr(x = simulation_training_ml$data, target = "y")
    
    lrn = simulation_training_ml$lrn
    
    search_space = simulation_training_ml$search_space

    terminator = trm("evals", n_evals = 500)

    xgboost = AutoTuner$new(
      learner = lrn,
      resampling = rsmp("holdout"),
      measure = msr("regr.mse"),
      search_space = search_space,
      terminator = terminator,
      tuner = tnr("random_search")
    )

    xgboost$train(task)
    
    pred_xgboost = xgboost$predict(task)
    mse_xgboost = pred_xgboost$score(msr("regr.mse"))
    r2_xgboost = pred_xgboost$score(msr("regr.rsq"))
    
    
  }
  
  
  
  for(i in 1:rep){
    simulation = create_sim_data_slim(n, type = scenario)
    data = simulation$data
    
    # train test split
    sample = sample.split(data$y, SplitRatio = 2/3)
    train = subset(data, sample == TRUE)
    test = subset(data, sample == FALSE)
    
    x_train = train[, colnames(train) != "y"]
    x_test = test[, colnames(test) != "y"]
    
    y_train = train$y
    y_test = test$y
    
    if (type == "performance"){
      # train linear model
      fm = simulation$fm
      lm = gam(fm, data = train)
      
      # extract fitted values (surrogates)
      y_hat_train_lm = lm$fitted.values
      y_hat_test_lm = predict(lm, x_test)
      
      mse_train_lm = c(mse_train_lm, mean((y_train - y_hat_train_lm)^2))
      r2_train_lm = c(r2_train_lm, r_2(y_train, y_hat_train_lm))
      mse_test_lm = c(mse_test_lm, mean((y_test - y_hat_test_lm)^2))
      r2_test_lm = c(r2_test_lm, r_2(y_test, y_hat_test_lm))
      
      # train xgboost model
      lrn = simulation$lrn
      task_train = as_task_regr(x = train, target = "y")
      task_test = as_task_regr(x = test, target = "y")
      lrn$train(task_train)
      pred_xgboost_train = lrn$predict(task_train)
      pred_xgboost_test = lrn$predict(task_test)
      
      mse_train_xgboost = c(mse_train_xgboost, pred_xgboost_train$score(msr("regr.mse")))
      r2_train_xgboost = c(r2_train_xgboost, pred_xgboost_train$score(msr("regr.rsq")))
      mse_test_xgboost = c(mse_test_xgboost, pred_xgboost_test$score(msr("regr.mse")))
      r2_test_xgboost = c(r2_test_xgboost, pred_xgboost_test$score(msr("regr.rsq")))

      
      y_hat_train_xgboost = as.data.table(pred_xgboost_train)$response
      y_hat_test_xgboost = as.data.table(pred_xgboost_test)$response
      
      
    } else if (type == "stability"){
      y_hat_train_lm = predict(lm, x_train)
      y_hat_test_lm = predict(lm, x_test)
      
      task_train = as_task_regr(x = train, target = "y")
      task_test = as_task_regr(x = test, target = "y")      
      pred_xgboost_train = xgboost$predict(task_train)
      pred_xgboost_test = xgboost$predict(task_test)
      
      y_hat_train_xgboost = as.data.table(pred_xgboost_train)$response
      y_hat_test_xgboost = as.data.table(pred_xgboost_test)$response
    }
   
    
    result_original = rbind(result_original, fit_trees(x_train = x_train, y_train = y_train, x_test = x_test, y_test = y_test,  
                                                       minsize = minsize, maxdepth = maxdepth, impr = impr, alpha = alpha, 
                                                       pruning = pruning, approximate = approximate))
    
    result_surrogate_lm  = rbind(result_surrogate_lm, fit_trees(x_train = x_train, y_train = y_hat_train_lm, x_test = x_test, y_test = y_hat_test_lm,  
                                                                minsize = minsize, maxdepth = maxdepth, impr = impr, alpha = alpha, 
                                                                pruning = pruning, approximate = approximate))
    result_surrogate_xgboost  = rbind(result_surrogate_xgboost, fit_trees(x_train = x_train, y_train = y_hat_train_xgboost, x_test = x_test, y_test = y_hat_test_xgboost,  
                                                                          minsize = minsize, maxdepth = maxdepth, impr = impr, alpha = alpha, 
                                                                          pruning = pruning, approximate = approximate))
    
    setTxtProgressBar(pb,i)
  }
  close(pb)
  
  result = list(result_original = result_original, 
                result_surrogate_lm = result_surrogate_lm, 
                result_surrogate_xgboost = result_surrogate_xgboost)
  if (length(mse_train_lm)>0){
    result$lm = list(mse_train_lm = mse_train_lm,
                          r2_train_lm = r2_train_lm,
                          mse_test_lm = mse_test_lm,
                          r2_test_lm = r2_test_lm)
    
    result$xgboost = list(mse_train_xgboost = mse_train_xgboost,
                     r2_train_xgboost = r2_train_xgboost,
                     mse_test_xgboost = mse_test_xgboost,
                     r2_test_xgboost = r2_test_xgboost)

  } else if (length(mse_lm)>0){
    result$lm = list(mse_lm = mse_lm,
                     r2_lm = r2_lm)
    
    result$xgboost = list(mse_xgboost = mse_xgboost,
                          r2_xgboost = r2_xgboost)
  }
  
  return(result)
}
