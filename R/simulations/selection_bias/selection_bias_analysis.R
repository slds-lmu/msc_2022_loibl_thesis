load("Data/simulations/simulation_study/selection_bias/selection_bias_independence.RData")
load("Data/simulations/simulation_study/selection_bias/selection_bias_independence_small.RData")

load("Data/simulations/simulation_study/selection_bias/selection_bias_interaction.RData")
load("Data/simulations/simulation_study/selection_bias/selection_bias_full_interaction.RData")

library(REdaS)
library(kableExtra)

table(split_slim)/length(split_slim)*100
freqCI(selection_bias_interaction$split_slim, level = c(.95))
freqCI(selection_bias_interaction$split_slim_anova, level = c(.95))
freqCI(selection_bias_interaction$split_mob, level = c(.95))
freqCI(selection_bias_interaction$split_ctree, level = c(.95))



# frequency_table_independence = apply(selection_bias_independence, 2, function(col){
#   table(col)/length(col)*100
# })
# frequency_table_independence = do.call(cbind, frequency_table_independence)
# 
# 
# frequency_table_independence_small = apply(selection_bias_independence_small, 2, function(col){
#   table(col)/length(col)*100
# })
# 
# 
# frequency_table_interaction = apply(selection_bias_interaction, 2, function(col){
#   table(col)/length(col)*100
# })
# 
# frequency_table_full_interaction = apply(selection_bias_full_interaction, 2, function(col){
#   table(col)/length(col)*100
# })
# 
# 
# 
# frequency_table_independence %>%
#   kbl(caption="Relative frequency of selecting covariate Xi as splitting variable",
#       format="latex",
#       # col.names = c("Gender","Education","Count","Mean","Median","SD"),
#       align="r") %>%
#   kable_minimal(full_width = F)
# 


ggplot(stack(selection_bias_independence),
       aes(x = values, color=ind, fill = ind)) +
  stat_count(position = "dodge") +
  ggtitle("Frequency of selection", subtitle = "Independence") +
  labs(x="selected variable", y="frequeny") +
  geom_boxplot()


ggplot(stack(selection_bias_independence_small),
       aes(x = values, color=ind, fill = ind)) +
  stat_count(position = "dodge") +
  ggtitle("Frequency of selection", subtitle = "Independence small") +
  labs(x="selected variable", y="frequeny") +
  geom_boxplot()

ggplot(stack(selection_bias_interaction),
       aes(x = values, color=ind, fill = ind)) +
  stat_count(position = "dodge") +
  ggtitle("Frequency of selection", subtitle = "Interaction between x1*x2 an x3*x4") +
  labs(x="selected variable", y="frequeny") +
  geom_boxplot()

ggplot(stack(selection_bias_full_interaction),
       aes(x = values, color=ind, fill = ind)) +
  stat_count(position = "dodge") +
  ggtitle("Frequency of selection", subtitle = "Interaction between all pairs") +
  labs(x="selected variable", y="frequeny") +
  geom_boxplot()

