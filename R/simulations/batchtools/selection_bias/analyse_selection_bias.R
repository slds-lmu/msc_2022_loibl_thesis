# load results

list.files("Data/simulations/batchtools/selection_bias_general/results/", full.names = TRUE)

independence_small = readRDS("Data/simulations/batchtools/selection_bias_general/results/independence_small_n1000.rds")
lapply(independence_small, function(table){
  chisq.test(table, p = rep(0.25,4))
})

full_interaction = readRDS("Data/simulations/batchtools/selection_bias_general/results/full_interaction_n1000.rds") 
lapply(full_interaction, function(table){
  chisq.test(table, p = rep(0.25,4))
})

library(REdaS)

freqCI(independence_small$slim, level = c(.95))
freqCI(independence_small$guide, level = c(.95))


########
# guide selection bias

list.files("Data/simulations/batchtools/selection_bias_guide/results/", full.names = TRUE)
split_data = readRDS("Data/simulations/batchtools/selection_bias_guide/results/selection_bias_guide.rds")
nrow(split_data)
nrow(split_data[n == 1000 & type == "selection_bias_full_interaction"])
nrow(split_data[n == 2000 & type == "selection_bias_full_interaction"])
table(split_data[n == 1000 & type == "selection_bias_full_interaction", .(split_guide)])

sum(table(split_data[n == 1000 & type == "selection_bias_full_interaction", .(split_guide)]))


figuredir = "Figures/simulations/batchtools/selection_bias_guide/"
savedir ="Data/simulations/batchtools/selection_bias_guide/results/"

if (!dir.exists(figuredir)) dir.create(figuredir, recursive = TRUE)



for(t in unique(split_data$type)){
  for (exclude in unique(tab[type == t, exclude.categoricals])){
    tab_small = split_data[type == t & exclude.categoricals == exclude, ]
    
    saveRDS(tab_small, file = paste0(savedir, str_remove(t, "selection_bias_"), ifelse(exclude, "_categoricals_excl", "_categoricals_incl"), ".rds"))
    
    p = ggplot(stack(tab_small[,.(split_guide)]),
               aes(x = values, color=ind, fill = ind)) +
      stat_count(position = "dodge") +
      ggtitle("Frequency of selection", subtitle = paste(str_replace_all(str_remove(t, "selection_bias_"), "_", " "), ifelse(exclude, "categoricals excl", "categoricals incl"))) +
      labs(x="selected variable", y="frequency", color = "surrogate", fill = "surrogate")
    
    ggexport(p, filename = paste0(figuredir, str_remove(t, "selection_bias_"), ifelse(exclude, "_categoricals_excl", "_categoricals_incl"),".pdf"), width = 8, height = 3.8)
    
    for(test in c("curvature", "interaction")){
      
      p = ggplot(stack(tab_small[test_guide == test,.(split_guide)]),
                 aes(x = values, color=ind, fill = ind)) +
        stat_count(position = "dodge") +
        ggtitle("Frequency of selection", subtitle = paste(str_replace_all(str_remove(t, "selection_bias_"), "_", " "), ifelse(exclude, "categoricals excl", "categoricals incl"), test)) +
        labs(x="selected variable", y="frequency", color = "surrogate", fill = "surrogate")
      
      ggexport(p, filename = paste0(figuredir, str_remove(t, "selection_bias_"), ifelse(exclude, "_categoricals_excl", "_categoricals_incl"), "_", test,".pdf"), width = 8, height = 3.8)
      
    }
  }

  
}



#####################
# slim selection bias different values of n.quantiles
list.files("Data/simulations/batchtools/selection_bias_slim/results/", full.names = TRUE)
slim_full_interaction = readRDS("Data/simulations/batchtools/selection_bias_slim/results/full_interaction_n1000.rds")
sapply(slim_full_interaction, sum)
slim_independence_small = readRDS("Data/simulations/batchtools/selection_bias_slim/results/independence_small_n1000.rds")
sapply(slim_independence_small, sum)


library(kableExtra)
library(tidyr)
library(REdaS)


df_slim_full_interaction = lapply(slim_full_interaction, function(el){
  as.vector(el)
}) %>% as.data.frame()

df_slim_independence_small = lapply(slim_independence_small, function(el){
  as.vector(el)
}) %>% as.data.frame()


df_slim_full_interaction %>%
  kbl(caption="frequency of selecting covariate Xi as splitting variable",
      format="latex",
      row.names = TRUE,
      align="r") %>%
  kable_minimal(full_width = F)

df_slim_independence_small %>%
  kbl(caption="frequency of selecting covariate Xi as splitting variable",
      format="latex",
      row.names = TRUE,
      align="r") %>%
  kable_minimal(full_width = F)


