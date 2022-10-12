source("R/load_packages.R")
source("R/tree_splitting_slim.R")
source("R/helper_general.R")
library(coin)
library(strucchange)

n = 10000
x1 = runif(n, 3, 5)
x2 = runif(n, 3, 5)
eps = rnorm(n)

# zwei verschiedene Datengenerierende Prozesse
y_interaction = 1 + x1 + x2 + ifelse(x1<4, 3*x2,0) + eps
y_linear = 1 + x1 + x2 +  eps


data_interaction = data.frame(y = y_interaction, x1, x2)
data_linear = data.frame(y = y_linear, x1, x2)

# Anpassen eines linearen Modells OHNE Interaktionsterm
lm_interaction = lm(y_interaction ~ x1 + x2)
lm_linear = lm(y_linear ~  x1 + x2)
scores_interaction = sandwich::estfun(lm_interaction)
scores_linear = sandwich::estfun(lm_linear)

# Visueller Vergleich der scores
plot_scores = function(scores, x, name){
  new_dir = paste0("Figures/Scores/", name, "/")
  dir.create(new_dir)
  for(col_s in colnames(scores)){
    for(col_x in colnames(x)){
      png(paste0(new_dir, name, "_scores_", col_s, "_", col_x, ".png"))
      score_spline = lm(scores[,col_s] ~poly(x[,col_x], degree = 3))
      plot(x[,col_x], scores[,col_s], xlab = col_x, ylab = paste0("score ",col_s))
      lines(stats::lowess(x[,col_x], scores[,col_s]), col = "red", lwd = 2)
      dev.off()
    }
  }
}

plot_scores(scores_linear, x = cbind(x1,x2), name = "correctly_specified_3_5")
plot_scores(scores_interaction, x = cbind(x1,x2), name = "misspecified_3_5")


# Vergleiche MOB, CTree und SLIM für LM 
tree_slim_linear = compute_tree_slim(y_linear, x = cbind(x1,x2), n.split = 2, impr.par = 0.05, min.split = round(n/100))
res_slim_linear = extract_split_criteria(tree_slim_linear)
tree_slim_interaction = compute_tree_slim(y_interaction, x = cbind(x1,x2), n.split = 2, impr.par = 0.05, min.split = round(n/100))
res_slim_interaction = extract_split_criteria(tree_slim_interaction)


model_lm = function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
  lm(y ~ 0 + x , ...)
}

fm_lm = as.formula("y ~ x1 + x2 | x1 + x2")

# MOB
mob_linear = mob(fm_lm, data = data_linear, fit = model_lm, control = mob_control(maxdepth = 3))
mob_interaction = mob(fm_lm, data = data_interaction, fit = model_lm, control = mob_control(maxdepth = 3))

# CTree
ctree_linear = partykit::ctree(fm_lm,
                               data = data_linear, 
                               ytrafo = model_lm,
                               control = partykit::ctree_control(maxdepth = 2, teststat =  "maximum"))
ctree_interaction = partykit::ctree(fm_lm,
                                    data = data_interaction, 
                                    ytrafo = model_lm,
                                    control = partykit::ctree_control(maxdepth = 2, teststat =  "maximum"))


independence_test(scores_interaction[,1] ~ x1)    
independence_test(scores_interaction[,1] ~ x2)                                     

sctest(ctree_interaction, node = 1)
