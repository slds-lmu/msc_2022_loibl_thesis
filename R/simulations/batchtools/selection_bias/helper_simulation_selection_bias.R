source("R/mob_fitting_functions.R")

get_sim_results_selection_bias = function(data, job, instance,  ...){
  if(is.null(data)){
    data = instance$data
  } else {
    data = as.data.frame(data)
  }
  x = data[,colnames(data) != "y"]
  y = data$y
  
  slim = compute_tree_slim(y, x ,n.split = 1, pruning = "none", n.quantiles = NULL, min.split = 50)
  split_slim = slim[[1]][[1]][["split.feature"]]
  
  guide = compute_tree_slim(y, x ,n.split = 1, pruning = "none", n.quantiles = NULL, min.split = 50, split.method = "guide")
  split_guide = guide[[1]][[1]][["split.feature"]]
  test_guide = guide[[1]][[1]][["test.type"]]
  
  fm_mob = formula(paste("y ~", paste(colnames(x), collapse = "+"), "|", paste(colnames(x), collapse = "+")))
  
  mob = lmtree(fm_mob, data = data, minsize = 50, maxdepth = 2, alpha = 1)
  mobrule = partykit:::.list.rules.party(mob)[1]
  split_mob = str_extract(mobrule,"^.*(?=( <=| %in))")
  
  ctree = suppressWarnings(ctree(fm_mob,
                                 data = data,
                                 ytrafo = fit_lm,
                                 control = ctree_control(minsplit  = 50, maxdepth = 1, alpha = 1)))
  ctreerule = partykit:::.list.rules.party(ctree)[1]
  split_ctree = str_extract(ctreerule,"^.*(?=( <=| %in))")
  
  
  return(list("split_slim" = split_slim, "split_mob" = split_mob, "split_ctree" = split_ctree, "split_guide" = split_guide, "test_guide" = test_guide))
}

  