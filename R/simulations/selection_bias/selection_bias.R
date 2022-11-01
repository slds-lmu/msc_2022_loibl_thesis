# Simulation Selection Bias
source("R/load_packages.R")
source("R/tree_splitting_slim.R")
source("R/helper_general.R")
source("R/simulations/simulation_setting_definition_slim.R")
source("R/mob_fitting_functions.R")
library(partykit)
library(REdaS)


rep = 1000
n = 1000

split_slim = c()
split_mob = c()
split_mob_test = c()
split_ctree = c()
split_slim_anova = c()
pb = txtProgressBar(min = 0, max = rep, initial = 0) 

for(i in 1:rep){
  data = create_sim_data_slim(n, "selection_bias")$data
  x = data[,colnames(data) != "y"]
  y = data$y
  
  slim = compute_tree_slim(y, x ,n.split = 1, pruning = "none", n.quantiles = NULL, min.split = 10)
  extract_split_criteria(slim)
  split_slim = c(split_slim, slim[[1]][[1]][["split.feature"]])

  mob = lmtree(y~x1+x2+x3+x4+x5+x6|x1+x2+x3+x4+x5+x6, data = data, minsize = 10, maxdepth = 2, alpha = 1)
  mobrule = partykit:::.list.rules.party(mob)[1]
  split_mob = c(split_mob,str_extract(mobrule,"^.*(?=( <=| %in))"))
  
  ctree = suppressWarnings(ctree(as.formula(y~x1+x2+x3+x4+x5+x6|x1+x2+x3+x4+x5+x6),
                                 data = data,
                                 ytrafo = fit_lm,
                                 control = ctree_control(minsplit  = 10, maxdepth = 1, alpha = 1)))
  ctreerule = partykit:::.list.rules.party(ctree)[1]
  split_ctree = c(split_ctree, str_extract(ctreerule,"^.*(?=( <=| %in))"))

  slim_anova = compute_tree_slim(y, x ,n.split = 1, pruning = "none", n.quantiles = NULL, min.split = 10, split.method = "anova")
  extract_split_criteria(slim_anova)
  split_slim_anova = c(split_slim_anova, slim_anova[[1]][[1]][["split.feature"]])

  setTxtProgressBar(pb,i)
}
close(pb)


# save(split_slim,
#      split_mob, 
#      split_ctree,
#      split_slim_anova,
#      file = "Data/simulations/selection_bias.RData")
