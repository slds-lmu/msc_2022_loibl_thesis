source("R/mob_fitting_functions.R")

get_sim_results_selection_bias = function(data, job, instance, tree_methods = c("slim", "mob", "ctree", "guide"), n.quantiles = NULL,
                                          exclude.categoricals = FALSE, get.objective = FALSE, min.split = 50, correct.bias = FALSE,
                                          pruning = "none", impr.par = 0.1, alpha = 0.05,...){
  if(is.null(data)){
    data = instance$data
  } else {
    data = as.data.frame(data)
  }
  x = data[,colnames(data) != "y"]
  y = data$y
  
  result = list()
  
  formals(compute_tree_slim)$pruning = pruning
  formals(compute_tree_slim)$impr.par = impr.par
  

  
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
        if(get.objective){
          impr_slim = split_criteria_slim[1,"intImp"]
          result$impr_slim = impr_slim
          names(result)[names(result) == "impr_slim"] = paste0("impr_slim_",ifelse(is.null(quantiles),"exact",quantiles))
          
          sse_slim = split_criteria_slim[1, "objective.value"]
          result$sse_slim = sse_slim
          names(result)[names(result) == "sse_slim"] = paste0("sse_slim_",ifelse(is.null(quantiles),"exact",quantiles))
        }
      }
    } else{
      slim = compute_tree_slim(y, x ,n.split = 1, n.quantiles = n.quantiles, min.split = min.split)
      split_slim = split_criteria_slim[1,"split.feature"]
      result$split_slim = split_slim
      if(get.objective){
        sse_slim = split_criteria_slim[1,"objective.value"]
        result$sse_slim = sse_slim
        impr_slim = split_criteria_slim[1,"intImp"]
        result$impr_slim = impr_slim
      }
    }
    
  }
  
  if("guide" %in% tree_methods){
    for(exc in exclude.categoricals){
      for(cor in correct.bias){
        guide = compute_tree_slim(y, x ,n.split = 1, n.quantiles = NULL, min.split = min.split, 
                                  split.method = "guide", exclude.categoricals = exc, correct.bias = cor)
        split_criteria = extract_split_criteria(guide)
        
        split_guide = split_criteria[1,"split.feature"]
        test_guide = split_criteria[1,"guide.test"]
        impr_guide = split_criteria[1,"intImp"]
        
        result$split_guide = split_guide
        names(result)[names(result) == "split_guide"] = paste0("split_guide_",ifelse(exc,"excl_cat","incl_cat"), ifelse(cor, "_corr", "_biased"))
        
        result$test_guide = test_guide
        names(result)[names(result) == "test_guide"] = paste0("test_guide_",ifelse(exc,"excl_cat","incl_cat"), ifelse(cor, "_corr", "_biased"))
        
        result$impr_guide = impr_guide
        names(result)[names(result) == "impr_guide"] = paste0("impr_guide",ifelse(exc,"excl_cat","incl_cat"), ifelse(cor, "_corr", "_biased"))
        
      }
    }
    
    
  }
  
  
  if("mob" %in% tree_methods){
    fm_mob = formula(paste("y ~", paste(colnames(x), collapse = "+"), "|", paste(colnames(x), collapse = "+")))
    
    mob = lmtree(fm_mob, data = data, minsize = min.split, maxdepth = 2, alpha = alpha)
    mobrule = partykit:::.list.rules.party(mob)[1]
    split_mob = str_extract(mobrule,"^.*(?=( <=| %in))")
    result$split_mob = split_mob
  }
  
  if("ctree" %in% tree_methods){
    ctree = suppressWarnings(ctree(fm_mob,
                                   data = data,
                                   ytrafo = fit_lm,
                                   control = ctree_control(minsplit  = min.split, maxdepth = 1, alpha = alpha)))
    ctreerule = partykit:::.list.rules.party(ctree)[1]
    split_ctree = str_extract(ctreerule,"^.*(?=( <=| %in))")
    result$split_ctree = split_ctree
  }
  
  
  
  return(result)
}

  