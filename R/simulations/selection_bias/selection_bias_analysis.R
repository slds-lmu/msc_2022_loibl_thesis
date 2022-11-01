load("Data/simulations/simulation_study/selection_bias.RData")
library(REdaS)
library(kableExtra)

table(split_slim)/length(split_slim)*100
freqCI(split_slim, level = c(.95))

table(split_mob)/length(split_mob)*100
freqCI(split_mob, level = c(.95))

table(split_ctree)/length(split_ctree)*100
freqCI(split_ctree, level = c(.95))

table(split_slim_anova)/length(split_slim_anova)
freqCI(split_slim_anova, level = c(.95))


frequency_table = cbind(SLIM = table(split_slim)/length(split_slim)*100,
                        MOB = table(split_mob)/length(split_mob)*100,
                        CTree = table(split_ctree)/length(split_ctree)*100)



frequency_table %>%
  kbl(caption="Relative frequency of selecting covariate Xi as splitting variable",
      format="latex",
      # col.names = c("Gender","Education","Count","Mean","Median","SD"),
      align="r") %>%
  kable_minimal(full_width = F)
