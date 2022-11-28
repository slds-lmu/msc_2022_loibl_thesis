# Simulation Selection Bias
source("R/load_packages.R")
source("R/tree_splitting_slim.R")
source("R/helper_general.R")
source("R/simulations/batchtools/simulation_setting_definition.R")
source("R/mob_fitting_functions.R")
library(partykit)
library(REdaS)



run_simulation_selection_bias = function(type = "independence", rep, n, n.quantiles = NULL){
  split_slim = c()
  split_mob = c()
  split_ctree = c()
  split_slim_anova = c()
  split_slim_R2 = c()
  split_slim_R2_adj = c()
  pb = txtProgressBar(min = 0, max = rep, initial = 0) 
  
  for(i in 1:rep){
    data = create_sim_data(n = n, type = paste0("selection_bias_", type))$data
    x = data[,colnames(data) != "y"]
    y = data$y
    
    slim = compute_tree_slim(y, x ,n.split = 1, pruning = "none", n.quantiles = n.quantiles, min.split = 10)
    extract_split_criteria(slim)
    split_slim = c(split_slim, slim[[1]][[1]][["split.feature"]])
    
    fm_mob = formula(paste("y ~", paste(colnames(x), collapse = "+"), "|", paste(colnames(x), collapse = "+")))
    
    mob = lmtree(fm_mob, data = data, minsize = 10, maxdepth = 2, alpha = 1)
    mobrule = partykit:::.list.rules.party(mob)[1]
    split_mob = c(split_mob,str_extract(mobrule,"^.*(?=( <=| %in))"))
    
    ctree = suppressWarnings(ctree(fm_mob,
                                   data = data,
                                   ytrafo = fit_lm,
                                   control = ctree_control(minsplit  = 10, maxdepth = 1, alpha = 1)))
    ctreerule = partykit:::.list.rules.party(ctree)[1]
    split_ctree = c(split_ctree, str_extract(ctreerule,"^.*(?=( <=| %in))"))
    
    slim_anova = compute_tree_slim(y, x ,n.split = 1, pruning = "none", n.quantiles = n.quantiles, min.split = 10, split.method = "anova")
    extract_split_criteria(slim_anova)
    split_slim_anova = c(split_slim_anova, slim_anova[[1]][[1]][["split.feature"]])
    
    slim_R2 = compute_tree_slim(y, x ,n.split = 1, pruning = "none", n.quantiles = n.quantiles, min.split = 10, split.method = "R2")
    extract_split_criteria(slim_R2)
    split_slim_R2 = c(split_slim_R2, slim_R2[[1]][[1]][["split.feature"]])
    
    slim_R2_adj = compute_tree_slim(y, x ,n.split = 1, pruning = "none", n.quantiles = n.quantiles, min.split = 10, split.method = "R2_adj")
    extract_split_criteria(slim_R2_adj)
    split_slim_R2_adj = c(split_slim_R2_adj, slim_R2_adj[[1]][[1]][["split.feature"]])
    
    setTxtProgressBar(pb,i)
  }
  close(pb)
  return(data.frame(split_slim, split_slim_anova, split_slim_R2, split_slim_R2_adj, split_mob, split_ctree))
}

selection_bias_independence = run_simulation_selection_bias(type = "independence", rep = 1, n = 500, n.quantiles = NULL)
selection_bias_independence_small = run_simulation_selection_bias(type = "independence_small", rep = 1, n = 500, n.quantiles = NULL)
selection_bias_interaction = run_simulation_selection_bias(type = "interaction", rep = 1, n = 500, n.quantiles = NULL)
selection_bias_full_interaction = run_simulation_selection_bias(type = "full_interaction", rep = 1, n = 500, n.quantiles = NULL)
selection_bias_interaction_binary = run_simulation_selection_bias(type = "interaction_binary", rep = 1, n = 500, n.quantiles = NULL)
selection_bias_interaction_categorical = run_simulation_selection_bias(type = "interaction_categorical", rep = 1, n = 500, n.quantiles = NULL)
selection_bias_interaction_binary_categorical = run_simulation_selection_bias(type = "interaction_binary_categorical", rep = 1, n = 5000, n.quantiles = NULL)




save(selection_bias_independence,  file = "Data/simulations/simulation_study/selection_bias/selection_bias_independence.RData")
save(selection_bias_independence_small,  file = "Data/simulations/simulation_study/selection_bias/selection_bias_independence_small.RData")
save(selection_bias_interaction,  file = "Data/simulations/simulation_study/selection_bias/selection_bias_interaction.RData")
save(selection_bias_full_interaction,  file = "Data/simulations/simulation_study/selection_bias/selection_bias_full_interaction.RData")
