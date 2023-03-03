source("R/mob_fitting_functions.R")
source("R/simulations/batchtools/helper_simulations.R")

get_sim_results_selection_bias = function(data, job, instance, tree_methods = c("slim", "mob", "ctree", "guide"), n.quantiles = NULL,
                                          exclude.categoricals = FALSE, mse_train = FALSE, mse_test = FALSE, min.split = 50, correct.bias = FALSE,
                                          pruning = "none", impr.par = 0.1, alpha = 0.05,...){
  if(is.null(data)){
    data = instance$data
  } else {
    data = as.data.frame(data)
  }

  if(mse_test){
    split_point = nrow(data)/3*2
    train = data[1:split_point,]
    test = data[(split_point+1):nrow(data),]
    x = train[, colnames(train) != "y"]
    x_test = test[, colnames(test) != "y"]
    
    y = train$y
    y_test = test$y
  } else{
    x = data[,colnames(data) != "y"]
    y = data$y
  }
  

  result = list()
  
  formals(compute_tree_slim)$pruning = pruning
  formals(compute_tree_slim)$impr.par = impr.par
  

  n.quantiles = unlist(n.quantiles)
  exclude.categoricals = unlist(exclude.categoricals)
  correct.bias = unlist(correct.bias)
  if ("slim" %in% tree_methods){
    if(!is.null(n.quantiles)){
      for(quantiles in n.quantiles){
        if(is.na(quantiles) | quantiles == 0L){
          quantiles = NULL
        }
        slim = compute_tree_slim(y, x ,n.split = 1, n.quantiles = quantiles, min.split = min.split)
        split_criteria_slim = extract_split_criteria(slim)
        
        split_slim = split_criteria_slim[1,"split.feature"]
        result$split_slim = split_slim
        names(result)[names(result) == "split_slim"] = paste0("split_slim_",ifelse(is.null(quantiles),"exact",quantiles))
        
        if(mse_train){
          mse_slim = mean((predict_slim(slim, x) - y)^2)
          result$mse_slim = mse_slim
          names(result)[names(result) == "mse_slim"] = paste0("mse_slim_",ifelse(is.null(quantiles),"exact",quantiles))
        }
        if(mse_test){
          result$mse_test_slim = mean((predict_slim(slim, x_test) - y_test)^2)
          names(result)[names(result) == "mse_test_slim"] = paste0("mse_test_slim_",ifelse(is.null(quantiles),"exact",quantiles))
        }
      }
    } else{
      slim = compute_tree_slim(y, x ,n.split = 1, n.quantiles = n.quantiles, min.split = min.split)
      split_slim = split_criteria_slim[1,"split.feature"]
      result$split_slim = split_slim
      if(mse_train){
        mse_slim = mean((predict_slim(slim, x) - y)^2)
        result$mse_slim = mse_slim
      }
      if(mse_test){
        result$mse_test_slim = sum((predict_slim(slim, x_test) - y_test)^2)
      }
    }
    
  }
  
  if("guide" %in% tree_methods){
    for(exc in exclude.categoricals){
      for(cor in correct.bias){
        guide = compute_tree_slim(y, x ,n.split = 1, n.quantiles = NULL, min.split = min.split, 
                                  split.method = "guide", exclude.categoricals = exc, correct.bias = cor)
        split_criteria_guide = extract_split_criteria(guide)
        
        split_guide = split_criteria_guide[1,"split.feature"]
        test_guide = split_criteria_guide[1,"guide.test"]

        result$split_guide = split_guide
        names(result)[names(result) == "split_guide"] = paste0("split_guide_",ifelse(exc,"excl_cat","incl_cat"), ifelse(cor, "_corr", "_biased"))
        
        result$test_guide = test_guide
        names(result)[names(result) == "test_guide"] = paste0("test_guide_",ifelse(exc,"excl_cat","incl_cat"), ifelse(cor, "_corr", "_biased"))
        
        if(mse_train){
          result$mse_guide = mean((predict_slim(guide, x) - y)^2)
          names(result)[names(result) == "mse_guide"] = paste0("mse_guide_",ifelse(exc,"excl_cat","incl_cat"), ifelse(cor, "_corr", "_biased"))
          
        }
      }
    }
  }
  
  
  if("mob" %in% tree_methods){
    fm_mob = formula(paste("y ~", paste(colnames(x), collapse = "+"), "|", paste(colnames(x), collapse = "+")))
    
    mob = lmtree(fm_mob, data = data, minsize = min.split, maxdepth = 2, alpha = alpha)
    mobrule = partykit:::.list.rules.party(mob)[1]
    split_mob = str_extract(mobrule,"^.*(?=( <=| %in))")
    result$split_mob = split_mob
    if(mse_train){
      result$mse_mob = mean((predict(mob, x)- y)^2)
    }
  }
  
  if("ctree" %in% tree_methods){
    ctree = suppressWarnings(ctree(fm_mob,
                                   data = data,
                                   ytrafo = fit_lm,
                                   control = ctree_control(minsplit  = min.split, maxdepth = 1, alpha = alpha)))
    ctreerule = partykit:::.list.rules.party(ctree)[1]
    split_ctree = str_extract(ctreerule,"^.*(?=( <=| %in))")
    result$split_ctree = split_ctree
    
    if(mse_train){
      fit_ctree = fit_ctree_leaves(ctree, x, y)
      result$mse_ctree = mean((predict_ctree(ctree, fit_ctree, x)- y)^2)     
    }
  }
  
  
  return(result)
}

  