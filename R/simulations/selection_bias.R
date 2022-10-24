# Simulation Selection Bias
source("R/load_packages.R")
source("R/tree_splitting_slim.R")
source("R/helper_general.R")
library(partykit)
library(REdaS)


rep = 10000
n = 100

split_slim = c()
split_mob = c()
split_ctree = c()

pb = txtProgressBar(min = 0, max = rep, initial = 0) 

for(i in 1:rep){
  x1 = runif(n, 0, 1)
  x2 = runif(n, 0, 1)
  x3 = round(runif(n, 0, 1), 1)  
  x4 = as.factor(sample(1:2, n, replace = TRUE))
  x5 = as.factor(sample(1:6, n, replace = TRUE))
  x6 = as.factor(sample(1:9, n, replace = TRUE))
  
  y = rnorm(n, 0, 1)
  x = cbind(x1,x2,x3,x4,x5,x6)
  data = as.data.frame(cbind(y,x))
  
  slim = compute_tree_slim(y, x ,n.split = 1, pruning = "none", n.quantiles = NULL, min.split = 10)
  extract_split_criteria(slim)
  split_slim = c(split_slim, slim[[1]][[1]][["split.feature"]])

  mob = lmtree(y~x1+x2+x3+x4+x5+x6|x1+x2+x3+x4+x5+x6, data = data, minsize = 10, maxdepth = 2, alpha = 1)
  mobrule = partykit:::.list.rules.party(mob)[1]
  split_mob = c(split_mob,str_extract(mobrule,"^.*(?=( <=))"))
  
  ctree = suppressWarnings(ctree(as.formula(y~x1+x2+x3+x4+x5+x6|x1+x2+x3+x4+x5+x6), 
                                 data = data, 
                                 ytrafo = model_lm, 
                                 control = ctree_control(minsplit  = 10, maxdepth = 1, alpha = 1)))
  ctreerule = partykit:::.list.rules.party(ctree)[1]
  split_ctree = c(split_ctree, str_extract(ctreerule,"^.*(?=( <=))"))
  
  setTxtProgressBar(pb,i)
}
close(pb)

slim_result = freqCI(split_slim, level = c(.95))
table(split_slim)

mob_result = freqCI(split_mob, level = c(.95))
table(split_mob)

ctree_result = freqCI(split_ctree, level = c(.95))
table(split_ctree)


save(slim_result, split_slim,  
     mob_result, split_mob, 
     ctree_result, split_ctree,
     file = "Data/simulations/selection_bias.RData")
