load("Data/simulations/simulation_study/selection_bias.RData")
library(REdaS)

table(split_slim)/length(split_slim)
freqCI(split_slim, level = c(.95))

table(split_mob)/length(split_mob)
freqCI(split_mob, level = c(.95))

table(split_ctree)/length(split_ctree)
freqCI(split_ctree, level = c(.95))

table(split_slim_anova)/length(split_slim_anova)
freqCI(split_slim_anova, level = c(.95))
