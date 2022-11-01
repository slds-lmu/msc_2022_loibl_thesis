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
  
  slim = compute_tree_slim(y_train, x_train ,n.split = maxdepth - 1, pruning = pruning, 
                           impr.par = impr, min.split = minsize, approximate = approximate)
  split_slim = extract_split_criteria(slim)
  n_term_nodes_slim = sum(split_slim$split.feature == "leafnode")
  rule_slim = split_slim$child.type[split_slim$split.feature == "leafnode"]
  
  mse_slim_train = mean((predict_slim(slim, x_train)- y_train)^2)
  r2_slim_train = r_2(y_train, predict_slim(slim, x_train))
  
  mse_slim_test = mean((predict_slim(slim, x_test)- y_test)^2)
  r2_slim_test = r_2(y_test, predict_slim(slim, x_test))
  
  
  # mob
  # formula mob
  fm_mob = formula(paste("y ~", paste(colnames(x_test), collapse = "+"), "|", paste(colnames(x_test), collapse = "+")))
  
  mob = lmtree(fm_mob, data = cbind(x_train, y = y_train), minsize = minsize, maxdepth = maxdepth, alpha = alpha)
  n_term_nodes_mob = width(mob)
  rule_mob = partykit:::.list.rules.party(mob)
  
  
  mse_mob_train = mean((predict(mob, x_train)- y_train)^2)
  r2_mob_train = r_2(y_train, predict(mob, x_train))
  
  mse_mob_test = mean((predict(mob, x_test)- y_test)^2)
  r2_mob_test = r_2(y_test, predict(mob, x_test))
  
  
  # ctree
  ctree = suppressWarnings(ctree(fm_mob,
                                 data = data.frame(x_train, y = y_train),
                                 ytrafo = fit_lm,
                                 control = ctree_control(minbucket = minsize, maxdepth = maxdepth - 1, alpha = alpha)))
  
  n_term_nodes_ctree =  width(ctree)
  rule_ctree = partykit:::.list.rules.party(ctree)
  
  fit_ctree = fit_ctree_leaves(ctree, x_train, y_train)
  
  mse_ctree_train = mean((predict_ctree(ctree, fit_ctree, x_train)- y_train)^2)
  r2_ctree_train = r_2(y_train, predict_ctree(ctree, fit_ctree, x_train))
  
  mse_ctree_test = mean((predict_ctree(ctree, fit_ctree, x_test)- y_test)^2)
  r2_ctree_test = r_2(y_test, predict_ctree(ctree, fit_ctree, x_test))
  
  return(list(interpretability = c(slim = n_term_nodes_slim, mob = n_term_nodes_mob, ctree = n_term_nodes_ctree),
              rule = list(slim = rule_slim, ctree = rule_ctree, mob = rule_mob),
              mse_train = c(slim = mse_slim_train, mob = mse_mob_train, ctree = mse_ctree_train),
              mse_test = c(slim = mse_slim_test, mob = mse_mob_test, ctree = mse_ctree_test),
              r2_train = c(slim = r2_slim_train, mob = r2_mob_train, ctree = r2_ctree_train),
              r2_test = c(slim = r2_slim_test, mob = r2_mob_test, ctree = r2_ctree_test)))
}



# function to run different simulation scenarios

generate_data_sim = function(scenario, type = "performance", rep, n, minsize = 20, maxdepth = 3, impr = 0.1, alpha = 0.05, pruning = "forward", approximate = FALSE){
  
  pb = txtProgressBar(min = 0, max = rep, initial = 0) 
  

  mse_acc_train = c()
  mse_acc_test = c()
  r2_acc_train = c()
  r2_acc_test = c()
  interpr_orig = c()
  rules_orig = list()
  
  mse_fid_train = c()
  mse_fid_test = c()
  r2_fid_train = c()
  r2_fid_test = c()
  interpr_surr = c()
  rules_surr = list()
  
  mse_ml_train = c()
  r2_ml_train = c()
  mse_ml_test = c()
  r2_ml_test = c()
  
  if (type == "stability") {
    simulation_training_ml = create_sim_data_slim(n, type = scenario)
    data_training_ml = simulation_training_ml$data
    
    # train ML model (and use it as black box model for all further simulations)
    fm_training_ml = simulation_training_ml$fm
    ml = gam(fm_training_ml, data = data_training_ml)
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
      # train ML model
      fm = simulation$fm
      
      ml = gam(fm, data = train)
      
      # extract fitted values (surrogates)
      y_hat_train = ml$fitted.values
      y_hat_test = predict(ml, x_test)
      
      mse_ml_train = c(mse_ml_train, mean((y_train - y_hat_train)^2))
      r2_ml_train = c(r2_ml_train, r_2(y_train, y_hat_train))
      mse_ml_test = c(mse_ml_test, mean((y_test - y_hat_test)^2))
      r2_ml_test = c(r2_ml_test, r_2(y_test, y_hat_test))
      
    } else if (type == "stability"){
      y_hat_train = predict(ml, x_train)
      y_hat_test = predict(ml, x_test)
    }
   
    
    result_original = fit_trees(x_train = x_train, y_train = y_train, x_test = x_test, y_test = y_test,  
                                minsize = minsize, maxdepth = maxdepth, impr = impr, alpha = alpha, 
                                pruning = pruning, approximate = approximate)
    result_surrogate  = fit_trees(x_train = x_train, y_train = y_hat_train, x_test = x_test, y_test = y_hat_test,  
                                  minsize = minsize, maxdepth = maxdepth, impr = impr, alpha = alpha, 
                                  pruning = pruning, approximate = approximate)
    
    mse_acc_train = rbind(mse_acc_train, result_original$mse_train)
    mse_acc_test = rbind(mse_acc_test, result_original$mse_test)
    r2_acc_train = rbind(r2_acc_train, result_original$r2_train)
    r2_acc_test = rbind(r2_acc_test, result_original$r2_test)
    interpr_orig = rbind(interpr_orig, result_original$interpretability)
    rules_orig = c(rules_orig, list(result_original$rule))
    
    mse_fid_train = rbind(mse_fid_train, result_surrogate$mse_train)
    mse_fid_test = rbind(mse_fid_test, result_surrogate$mse_test)
    r2_fid_train = rbind(r2_fid_train, result_surrogate$r2_train)
    r2_fid_test = rbind(r2_fid_test, result_surrogate$r2_test)
    interpr_surr = rbind(interpr_surr, result_surrogate$interpretability)
    rules_surr = c(rules_surr, list(result_surrogate$rule))
    
    
    setTxtProgressBar(pb,i)
  }
  close(pb)
  
  result = list(mse_acc_train = mse_acc_train, mse_acc_test = mse_acc_test, 
                r2_acc_train = r2_acc_train, r2_acc_test = r2_acc_test, 
                interpr_orig = interpr_orig, rules_orig = rules_orig,
                mse_fid_train = mse_fid_train, mse_fid_test = mse_fid_test, 
                r2_fid_train = r2_fid_train, r2_fid_test = r2_fid_test, 
                interpr_surr = interpr_surr, rules_surr = rules_surr)
  if (length(mse_ml_train)>0){
    result[["mse_ml_train"]] = mse_ml_train
    result[["r2_ml_train"]] = r2_ml_train
    result[["mse_ml_test"]] = mse_ml_test
    result[["r2_ml_test"]] = r2_ml_test
    
  }
  
  return(result)
}
