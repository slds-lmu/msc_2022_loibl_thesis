source("R/simulations/batchtools/simulation_setting_definition.R")
source("R/tree_splitting_slim.R")
library(mlr3)
library(mlr3extralearners)
library(iml)


data_interaction = create_sim_data(job = NULL, n = 10000, type = "selection_bias_full_interaction")$data
lm = lm(y ~ x1 + x2 + x3 + x4 + x1:x2 + x1:x3 + x1:x4 + x2:x3 + x2:x4 + x2:x4, data_interaction)
predictor_interaction = Predictor$new(lm, data = data_interaction[,colnames(data_interaction) != "y"], y = data_interaction$y)
interaction = Interaction$new(predictor_interaction)
plot(interaction)


split.feature = c()
split.value = c()
for(i in 1:100){
  n = 1000
  x1 = runif(n, 0, 1)
  x2 = runif(n, 0, 1)
  x3 = sample(seq(0,1,0.1), n, replace = TRUE) 
  y = x1 + x2 + x3 + x1*x2 + x1*x3 + x2*x3 + 0.01*rnorm(n)
  # y = x1 + x2 + x3 + 0.01*rnorm(n)
  
  x = cbind(x1, x2, x3)
  data = cbind(x1,x2, x3,y)
  # plot(x1,y)  
  # plot(x2,y)  
  
  tree = compute_tree_slim(y,x,n.split = 1)
  split.feature[i] = tree[[1]][[1]]$split.feature
  split.value[i] = tree[[1]][[1]]$split.value
  
}

table(split.feature)
table(split.value[split.feature == "x3"])
plot(x1,y)
plot(x2,y)
plot(x3,y)

lm = lm(y ~ x1 + x2 + x3 ,data = data.frame(data))

plot(x1, x1+x3 + 0.01*rnorm(n))
plot(x3, x1+x3 + 0.01*rnorm(n))
