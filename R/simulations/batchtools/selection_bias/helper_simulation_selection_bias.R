source("R/mob_fitting_functions.R")

get_sim_results_selection_bias = function(data, job, instance, tree_methods = c("slim", "mob", "ctree", "guide"), n.quantiles = NULL,
                                          exclude.categoricals = FALSE, get.objective = FALSE, min.split = 50,...){
  if(is.null(data)){
    data = instance$data
  } else {
    data = as.data.frame(data)
  }
  x = data[,colnames(data) != "y"]
  y = data$y
  
  result = list()
  
  if ("slim" %in% tree_methods){
    if(!is.null(n.quantiles)){
      for(quantiles in n.quantiles){
        if(is.na(quantiles) | quantiles == 0L){
          quantiles = NULL
        }
        slim = compute_tree_slim(y, x ,n.split = 1, pruning = "none", n.quantiles = quantiles, min.split = min.split)
        split_slim = slim[[1]][[1]][["split.feature"]]
        result$split_slim = split_slim
        names(result)[names(result) == "split_slim"] = paste0("split_slim_",ifelse(is.null(quantiles),"exact",quantiles))
        if(get.objective){
          sse_slim = slim[[1]][[1]][["objective.value"]]
          result$sse_slim = sse_slim
          names(result)[names(result) == "sse_slim"] = paste0("sse_slim_",ifelse(is.null(quantiles),"exact",quantiles))
        }
      }
    } else{
      slim = compute_tree_slim(y, x ,n.split = 1, pruning = "none", n.quantiles = n.quantiles, min.split = min.split)
      split_slim = slim[[1]][[1]][["split.feature"]]
      result$split_slim = split_slim
      if(get.objective){
        sse_slim = slim[[1]][[1]][["objective.value"]]
        result$sse_slim = sse_slim
      }
    }
    
  }
  
  if("guide" %in% tree_methods){
    guide = compute_tree_slim(y, x ,n.split = 1, pruning = "none", n.quantiles = NULL, min.split = min.split, 
                              split.method = "guide", exclude.categoricals = exclude.categoricals)
    split_guide = guide[[1]][[1]][["split.feature"]]
    test_guide = guide[[1]][[1]][["test.type"]]
    result$split_guide = split_guide
    result$test_guide = test_guide
  }
  
  
  if("mob" %in% tree_methods){
    fm_mob = formula(paste("y ~", paste(colnames(x), collapse = "+"), "|", paste(colnames(x), collapse = "+")))
    
    mob = lmtree(fm_mob, data = data, minsize = min.split, maxdepth = 2, alpha = 1)
    mobrule = partykit:::.list.rules.party(mob)[1]
    split_mob = str_extract(mobrule,"^.*(?=( <=| %in))")
    result$split_mob = split_mob
  }
  
  if("ctree" %in% tree_methods){
    ctree = suppressWarnings(ctree(fm_mob,
                                   data = data,
                                   ytrafo = fit_lm,
                                   control = ctree_control(minsplit  = 50, maxdepth = 1, alpha = 1)))
    ctreerule = partykit:::.list.rules.party(ctree)[1]
    split_ctree = str_extract(ctreerule,"^.*(?=( <=| %in))")
    result$split_ctree = split_ctree
  }
  
  
  
  return(result)
}

  