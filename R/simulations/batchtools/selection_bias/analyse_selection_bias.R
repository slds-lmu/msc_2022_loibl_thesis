# load results

list.files("Data/simulations/batchtools/selection_bias_general/results/", full.names = TRUE)

independence = readRDS("Data/simulations/batchtools/selection_bias_general/results/independence_n1000.rds")

independence_small = readRDS("Data/simulations/batchtools/selection_bias_general/results/independence_small_n1000.rds")
p_general_independence_small = lapply(independence_small, function(table){
  round(chisq.test(table, p = rep(0.25,4))[["p.value"]],4)
}) %>% as.data.frame()



full_interaction = readRDS("Data/simulations/batchtools/selection_bias_general/results/full_interaction_n1000.rds") 
p_general_full_interaction = lapply(full_interaction, function(table){
  round(chisq.test(table, p = rep(0.25,4))[["p.value"]],4)
}) %>% as.data.frame()

p_general_full_interaction %>%
  kbl(caption="p-values of $X_^2$ goodness-of-fit test",
      format="latex",
      row.names = FALSE,
      align="r") %>%
  kable_minimal(full_width = F)


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
  for (exclude in unique(split_data[type == t, exclude.categoricals])){
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
selection_bias_slim = readRDS("Data/simulations/batchtools/selection_bias_slim/results/selection_bias_slim.rds")

figuredir_slim = "Figures/simulations/batchtools/selection_bias_slim/"
if (!dir.exists(figuredir_slim)) dir.create(figuredir_slim, recursive = TRUE)

for(t in unique(selection_bias_slim$type)){
  data = selection_bias_slim[type == t]
  cols_sse = str_detect(names(data), "sse")
  data_long_sse = stack(data[, cols_sse, with = FALSE])
  data_long_sse$ind = str_remove_all(data_long_sse$ind, "sse_slim_")
  p_sse = ggplot(data_long_sse, mapping = aes(x = factor(ind, level=c("exact", "100", "75", "50", "25", "10", "8", "6", "4", "2")), y=values)) + 
    geom_point(stat='summary', fun='mean') +
    ggtitle("SSE for different values of n.quantiles", subtitle = str_replace_all(str_remove(t, "selection_bias_"), "_", " ")) +
    labs(x="number of quantiles", y = "SSE")
  
  
  cols_freq = str_detect(names(data), "split_slim")
  data_freq = data[, cols_freq, with = FALSE]
  table_list = lapply(data_freq, table)
  options = length(unique(unlist(data_freq)))
  
  if(t %in% c("selection_bias_interaction_binary_numeric", "selection_bias_interaction_categorical_numeric") ){
    prob = c(0.5,0.5,0)
  } else{
    prob = rep(1/options, options)
  }
    
  chi2 = sapply(table_list, function(table){
    chisq.test(table, p = prob)[["p.value"]]
  }) 
  
  names(chi2) = str_remove_all(names(chi2), "split_slim_")

  data_chi2 = data.frame(p_value = chi2, n.quantile = names(chi2))
  
  p_chi2 = ggplot(data_chi2) +
    geom_point(aes(x = factor(n.quantile, level = c("exact", "100", "75", "50", "25", "10", "8", "6", "4", "2")), y = chi2)) +
    ggtitle("Selection bias for different values of n.quantiles", subtitle = str_replace_all(str_remove(t, "selection_bias_"), "_", " ")) +
    labs(x="number of quantiles", y = "p value")
  
  ggarrange(p_chi2, p_sse, nrow = 2) %>%
    ggexport(filename = paste0(figuredir_slim, "sse_", str_remove(t, "selection_bias_"),".pdf"),
           width = 8, height = 3.8)
  
  
}

cols_sse = str_detect(names(selection_bias_slim[type == "selection_bias_independence_small"]), "sse")
data = stack(selection_bias_slim[type == "selection_bias_independence_small", cols_sse, with = FALSE])
ggplot(data, mapping=aes(x = ind, y=values))+geom_boxplot()

slim_full_interaction = readRDS("Data/simulations/batchtools/selection_bias_slim/results/full_interaction.rds")
slim_independence_small = readRDS("Data/simulations/batchtools/selection_bias_slim/results/independence_small.rds")
slim_independence_guide = readRDS("Data/simulations/batchtools/selection_bias_slim/results/guide.rds")
slim_interaction_small = readRDS("Data/simulations/batchtools/selection_bias_slim/results/interaction.rds")

library(kableExtra)
library(tidyr)
library(REdaS)


sb_slim_full_interaction = lapply(slim_full_interaction, function(el){
  as.vector(el[[1]])
}) %>% as.data.frame()

perf_slim_full_interaction = lapply(slim_full_interaction, function(el){
  as.vector(el[[2]])
}) %>% as.data.frame()

sb_slim_independence_small = lapply(slim_independence_small, function(el){
  as.vector(el[[1]])
}) %>% as.data.frame()

sb_slim_interaction_small = lapply(slim_interaction_small, function(el){
  as.vector(el[[1]])
}) %>% as.data.frame()

sb_slim_independence_guide = lapply(slim_independence_guide, function(el){
  as.vector(el[[1]])
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


