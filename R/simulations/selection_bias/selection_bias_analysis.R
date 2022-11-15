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

library(ggplot2)
library(ggpubr)

if (!dir.exists("Figures/Selection_Bias")) dir.create("Figures/Selection_Bias", recursive = TRUE)


p_ind = ggplot(stack(selection_bias_independence),
               aes(x = values, color=ind, fill = ind)) +
  stat_count(position = "dodge") +
  ggtitle("Frequency of selection", subtitle = "Independence") +
  labs(x="selected variable", y="frequency", color = "surrogate", fill = "surrogate") 

ggexport(p_ind, filename = "Figures/Selection_Bias/independence_frequency.pdf", width = 8, height = 3.8)


p_ind_small = ggplot(stack(selection_bias_independence_small),
                     aes(x = values, color=ind, fill = ind)) +
  stat_count(position = "dodge") +
  ggtitle("Frequency of selection", subtitle = "Independence small") +
  labs(x="selected variable", y="frequency", color = "surrogate", fill = "surrogate") 

ggexport(p_ind_small, filename = paste0("Figures/Selection_Bias/independence_small_frequency.pdf"), width = 8, height = 3.8)


p_int = ggplot(stack(selection_bias_interaction),
       aes(x = values, color=ind, fill = ind)) +
  stat_count(position = "dodge") +
  ggtitle("Frequency of selection", subtitle = "Interaction between x1*x2 and x3*x4") +
  labs(x="selected variable", y="frequency", color = "surrogate", fill = "surrogate") 
ggexport(p_int, filename = paste0("Figures/Selection_Bias/interaction_frequency.pdf"), width = 8, height = 3.8)


p_int_full = ggplot(stack(selection_bias_full_interaction),
                    aes(x = values, color=ind, fill = ind)) +
  stat_count(position = "dodge") +
  ggtitle("Frequency of selection", subtitle = "Interaction between all pairs") +
  labs(x="selected variable", y="frequency", color = "surrogate", fill = "surrogate") 
ggexport(p_int_full, filename = paste0("Figures/Selection_Bias/interaction_full_frequency.pdf"), width = 8, height = 3.8)

