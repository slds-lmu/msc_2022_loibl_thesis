# simulation helper

fit_trees = function(x_train, y_train, x_test, y_test, data_stability, min.split, 
                     maxdepth, impr.par, alpha, pruning, approximate,
                     n.quantiles, exclude.categoricals, correct.bias, 
                     tree_methods = c("slim", "mob", "ctree", "guide"),
                     extract_variables = FALSE, df.max = NULL){
  if ("slim_lasso" %in% tree_methods){
    x_wrong = paste0("x", 4:10)
  } else if ("slim_ridge" %in% tree_methods){
    x_wrong = "x1"
  }
  
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
      slim_res$x_wrong = ifelse(any(x_wrong %in% unique(split[,"split.feature"])), TRUE, FALSE)
    }
    
    if("slim_lasso" %in% tree_methods){
      slim_res$x3 = sum(split$split.feature == "x3")/sum(split$split.feature != "leafnode")
    }
    
    
    if(!is.null(data_stability)){
      # tree varies across all repetitions due to slightly different data, but data_stability is identical across all repetitions
      slim_res$stability = lapply(data_stability, function(dat){predict_slim(slim, dat, type = "node")})
    }
    
  } 
  if("slim_ridge" %in% tree_methods){
    slim_ridge_res = list(mbt = "SLIM Ridge")
    
    slim_ridge = compute_tree_slim(y_train, x_train ,n.split = maxdepth - 1, pruning = pruning,  n.quantiles = n.quantiles,
                                   impr.par = impr.par, min.split = min.split, approximate = FALSE,
                                   split.method = "slim", penalization = "L2")
    split = extract_split_criteria(slim_ridge)
    slim_ridge_res$n_leaves = sum(split$split.feature == "leafnode")

    slim_ridge_res$mse_train = mean((predict_slim(slim_ridge, x_train)- y_train)^2)
    slim_ridge_res$r2_train = r_2(y_train, predict_slim(slim_ridge, x_train))
    
    slim_ridge_res$mse_test = mean((predict_slim(slim_ridge, x_test)- y_test)^2)
    slim_ridge_res$r2_test = r_2(y_test, predict_slim(slim_ridge, x_test))
    
    if(extract_variables){
      slim_ridge_res$x_wrong = ifelse(any(x_wrong %in% unique(split[,"split.feature"])), TRUE, FALSE)
    }
    
    
    if(!is.null(data_stability)){
      # tree varies across all repetitions due to slightly different data, but data_stability is identical across all repetitions
      slim_ridge_res$stability = lapply(data_stability, function(dat){predict_slim(slim, dat, type = "node")})
    }
    
    
  } 
  if("slim_lasso" %in% tree_methods){
    slim_lasso_res = list(mbt = "SLIM Lasso")
    
    slim_lasso = compute_tree_slim(y_train, x_train ,n.split = maxdepth - 1, pruning = pruning,  n.quantiles = n.quantiles,
                                   impr.par = impr.par, min.split = min.split, approximate = FALSE,
                                   split.method = "slim", penalization = "L1", lambda = NULL, df.max = NULL)
    split = extract_split_criteria(slim_lasso)
    slim_lasso_res$n_leaves = sum(split$split.feature == "leafnode")

    slim_lasso_res$mse_train = mean((predict_slim(slim_lasso, x_train)- y_train)^2)
    slim_lasso_res$r2_train = r_2(y_train, predict_slim(slim_lasso, x_train))
    
    slim_lasso_res$mse_test = mean((predict_slim(slim_lasso, x_test)- y_test)^2)
    slim_lasso_res$r2_test = r_2(y_test, predict_slim(slim_lasso, x_test))
    
    slim_lasso_res$x_wrong = ifelse(any(x_wrong %in% unique(split[,"split.feature"])), TRUE, FALSE)
    
    slim_lasso_res$x3 = sum(split$split.feature == "x3")/sum(split$split.feature != "leafnode")
    

    
  } 
  if("slim_lasso_max_df" %in% tree_methods){
    slim_lasso_max_df_res_complete = data.frame()
    for(df in df.max){
      slim_lasso_max_df_res = list(mbt = paste("SLIM Lasso max df", df))
      
      slim_lasso_max_df = compute_tree_slim(y_train, x_train ,n.split = maxdepth - 1, pruning = pruning,  n.quantiles = n.quantiles,
                                            impr.par = impr.par, min.split = min.split, approximate = FALSE,
                                            split.method = "slim", penalization = "L1", lambda = NULL, df.max = df)
      split = extract_split_criteria(slim_lasso_max_df)
      slim_lasso_max_df_res$n_leaves = sum(split$split.feature == "leafnode")

      slim_lasso_max_df_res$mse_train = mean((predict_slim(slim_lasso_max_df, x_train)- y_train)^2)
      slim_lasso_max_df_res$r2_train = r_2(y_train, predict_slim(slim_lasso_max_df, x_train))
      
      slim_lasso_max_df_res$mse_test = mean((predict_slim(slim_lasso_max_df, x_test)- y_test)^2)
      slim_lasso_max_df_res$r2_test = r_2(y_test, predict_slim(slim_lasso_max_df, x_test))
      
      slim_lasso_max_df_res$x_wrong = ifelse(any(x_wrong %in% unique(split[,"split.feature"])), TRUE, FALSE)
      
      slim_lasso_max_df_res$x3 = sum(split$split.feature == "x3")/sum(split$split.feature != "leafnode")
      
      
      
      slim_lasso_max_df_res_complete = rbind(slim_lasso_max_df_res_complete,slim_lasso_max_df_res)
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
      guide_res$x_wrong = ifelse(any(x_wrong %in% unique(split[,"split.feature"])), TRUE, FALSE)
    }
    
    if(!is.null(data_stability)){
      guide_res$stability = lapply(data_stability, function(dat){predict_slim(guide, dat, type = "node")})
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
      mob_res$x_wrong = ifelse(any(x_wrong %in% unique(unlist(str_extract_all(mobrule,"(x+[1-9])")))), TRUE, FALSE)
      
    }
    
    if(!is.null(data_stability)){
      mob_res$stability = lapply(data_stability, function(dat){as.character(predict(mob, dat, type = "node"))})
      
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
      ctree_res$x_wrong = ifelse(any(x_wrong %in% unique(unlist(str_extract_all(ctreerule,"(x+[1-9])")))), TRUE, FALSE)
    }
    
    
    if(!is.null(data_stability)){
      ctree_res$stability = lapply(data_stability, function(dat){as.character(predict(ctree, dat, type = "node"))})
    }
  }
  
  res = rbind(slim_res, guide_res, mob_res, ctree_res)
  if ("slim_ridge" %in% tree_methods){
    res = rbind(res, slim_ridge_res)
  }
  if ("slim_lasso" %in% tree_methods){
    res = rbind(slim_res, slim_lasso_res, slim_lasso_max_df_res_complete, cbind(rbind(guide_res, mob_res, ctree_res), x3 = NA))
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
  y_true = unlist(y_true)
  y_hat = unlist(y_hat)
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
