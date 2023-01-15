# load results

list.files("Data/simulations/batchtools/selection_bias_general/results/", full.names = TRUE)
tab = readRDS("Data/simulations/batchtools/selection_bias_general/results/selection_bias_general.rds")

savedir = "Data/simulations/batchtools/selection_bias_general/results/"
figuredir = "Figures/simulations/batchtools/selection_bias_general/"

if (!dir.exists(savedir)) dir.create(savedir, recursive = TRUE)

if (!dir.exists(figuredir)) dir.create(figuredir, recursive = TRUE)
cols_split = str_subset(colnames(tab), "split")
cols_split = c("split_slim_exact", "split_slim_100", "split_guide_excl_cat_corr", 
               "split_guide_incl_cat_corr", "split_mob", "split_ctree")


for (t in unique(tab$type)){
  for (n_data in unique(tab[type == t , n])){
    tab_t_n = tab[type == t & n == n_data, ]
    result = list(slim = table(tab_t_n$split_slim_exact),
                  slim_100 = table(tab_t_n$split_slim_100),
                  slim_10 = table(tab_t_n$split_slim_10),
                  mob = table(tab_t_n$split_mob),
                  ctree = table(tab_t_n$split_ctree),
                  guide_inclcat = table(tab_t_n$impr_guideincl_cat_corr),
                  guide_excllcat = table(tab_t_n$impr_guide_excl_cat_corr)
                  # ,
                  # guide_biased_inclcat = table(tab_t_n$impr_guideincl_cat_biased),
                  # guide_biased_excllcat = table(tab_t_n$impr_guide_excl_cat_biased)
                  
                  )
    saveRDS(result, file = paste0(savedir, str_remove(t, "selection_bias_"), ".rds"))
    
    
    
    p = ggplot(stack(tab_t_n[,cols_split, with = FALSE]),
               aes(x = values, color=ind, fill = ind)) +
      stat_count(position = "dodge") +
      ggtitle("Frequency of selection", subtitle = paste(str_replace_all(str_remove(t, "selection_bias_"), "_", " "))) +
      labs(x="selected variable", y="frequency", color = "surrogate", fill = "surrogate")
    
    ggexport(p, filename = paste0(figuredir, str_remove(t, "selection_bias_"), ".pdf"), width = 8, height = 3.8)
    
  }
}







independence = readRDS("Data/simulations/batchtools/selection_bias_general/results/independence_n1000.rds")

independence_small = readRDS("Data/simulations/batchtools/selection_bias_general/results/independence_small_n1000.rds")
p_general_independence_small = lapply(independence_small, function(table){
  round(chisq.test(table, p = rep(0.25,4))[["p.value"]],4)
}) %>% as.data.frame()



full_interaction = readRDS("Data/simulations/batchtools/selection_bias_general/results/full_interaction_n1000.rds") 
p_general_full_interaction = lapply(full_interaction, function(table){
  round(chisq.test(table, p = rep(0.25,4))[["p.value"]],4)
}) %>% as.data.frame()

p_general_independence_small %>%
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

figuredir = "Figures/simulations/batchtools/selection_bias_guide/"
savedir ="Data/simulations/batchtools/selection_bias_guide/results/"

if (!dir.exists(figuredir)) dir.create(figuredir, recursive = TRUE)

table(split_data[impr_guideexcl_cat_biased < 0.01, split_guide_excl_cat_biased])
table(split_data[impr_guideexcl_cat_corr < 0.01, split_guide_excl_cat_corr])
table(split_data[impr_guideincl_cat_corr < 0.02, split_guide_incl_cat_corr])


ggplot(split_data,
       aes(x = split_guide_excl_cat_biased, y = impr_guideexcl_cat_biased)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot()

ggplot(split_data,
       aes(x = split_guide_excl_cat_biased)) +
  stat_count(position = "dodge") +
  ggtitle("Frequency of selection") +
  labs(x="selected variable", y="frequency", color = "surrogate", fill = "surrogate")

ggplot(split_data,
       aes(x = split_guide_excl_cat_corr, y = impr_guideexcl_cat_corr)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot()

ggplot(split_data,
       aes(x = split_guide_incl_cat_corr, y = impr_guideincl_cat_corr)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot()

ggplot(split_data,
       aes(x = split_guide_incl_cat_biased, y = impr_guideincl_cat_biased)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot()


library(ggplot2)
library(ggpubr)
library(stringr)


for (t in unique(split_data$type)){
  for (n_data in unique(split_data[type == t , n])){
    tab_t_n = split_data[type == t & n == n_data, ]

    
    cols_split = str_detect(colnames(tab_t_n), "split")
    
    p = ggplot(stack(tab_t_n[,cols_split, with = FALSE]),
               aes(x = values, color=ind, fill = ind)) +
      stat_count(position = "dodge") +
      ggtitle("Frequency of selection", subtitle = paste(str_replace_all(str_remove(t, "selection_bias_"), "_", " "))) +
      labs(x="selected variable", y="frequency", color = "surrogate", fill = "surrogate")
    
    ggexport(p, filename = paste0(figuredir, str_remove(t, "selection_bias_"), ".pdf"), width = 8, height = 3.8)
    
  }
}

# for (split in col_split){
#   split_small = split_data[, split, with = FALSE]
#   
#   saveRDS(split_small, file = paste0(savedir, split, ".rds"))
#   
#   p = ggplot(stack(split_data[, split, with = FALSE]),
#              aes(x = values)) +
#     stat_count(position = "dodge") +
#     ggtitle("Frequency of selection", subtitle = split) +
#     labs(x="selected variable", y="frequency", color = "surrogate", fill = "surrogate")
#   
#   ggexport(p, filename = paste0(figuredir, split, ".pdf"), width = 8, height = 3.8)
# 
# }

# for(n.data in unique(split_data$n)){
#   for (exclude in c(TRUE, FALSE)){
#     for (correct in c(TRUE, FALSE)){
#       tab_small = split_data[n == n.data, exclude.categoricals == exclude & correct.bias == correct, ]
# 
#       saveRDS(tab_small, file = paste0(savedir, str_remove(t, "selection_bias_"), ifelse(exclude, "_categoricals_excl", "_categoricals_incl"), ".rds"))
# 
#       p = ggplot(stack(tab_small[,.(split_guide)]),
#                  aes(x = values, color=ind, fill = ind)) +
#         stat_count(position = "dodge") +
#         ggtitle("Frequency of selection", subtitle = paste(str_replace_all(str_remove(t, "selection_bias_"), "_", " "),
#                                                            ifelse(exclude, "categoricals excl", "categoricals incl"),
#                                                            ifelse(correct, "_bias_corrected", "_biased"))) +
#         labs(x="selected variable", y="frequency", color = "surrogate", fill = "surrogate")
# 
#       ggexport(p, filename = paste0(figuredir, str_remove(t, "selection_bias_"), ifelse(exclude, "_categoricals_excl", "_categoricals_incl"),
#                                     ifelse(correct, "_bias_corrected", "_biased"), ".pdf"), width = 8, height = 3.8)
# 
#       for(test in c("curvature", "interaction")){
# 
#         p = ggplot(stack(tab_small[test_guide == test,.(split_guide)]),
#                    aes(x = values, color=ind, fill = ind)) +
#           stat_count(position = "dodge") +
#           ggtitle("Frequency of selection", subtitle = paste(str_replace_all(str_remove(t, "selection_bias_"), "_", " "),
#                                                              ifelse(exclude, "categoricals excl", "categoricals incl"),
#                                                              ifelse(correct, "_bias_corrected", "_biased"), test)) +
#           labs(x="selected variable", y="frequency", color = "surrogate", fill = "surrogate")
# 
#         ggexport(p, filename = paste0(figuredir, str_remove(t, "selection_bias_"),
#                                       ifelse(exclude, "_categoricals_excl", "_categoricals_incl"),
#                                       ifelse(correct, "_bias_corrected", "_biased"), "_", test,".pdf"), width = 8, height = 3.8)
# 
#       }
#     }
#   }
# }



#####################
# slim selection bias different values of n.quantiles
list.files("Data/simulations/batchtools/selection_bias_slim/results/", full.names = TRUE)
selection_bias_slim = rbind(readRDS("Data/simulations/batchtools/selection_bias_slim/results/selection_bias_slim.rds"))

table(selection_bias_slim[type == ])

figuredir_slim = "Figures/simulations/batchtools/selection_bias_slim/"
savedir_slim = "Data/simulations/batchtools/selection_bias_slim/results/"

if (!dir.exists(figuredir_slim)) dir.create(figuredir_slim, recursive = TRUE)

for(t in unique(selection_bias_slim$type)){
  data = selection_bias_slim[type == t]
  cols_sse = str_detect(names(data), "sse")
  data_long_sse = stack(data[, cols_sse, with = FALSE])
  data_long_sse$ind = str_remove_all(data_long_sse$ind, "sse_slim_")
  p_sse = ggplot(data_long_sse, mapping = aes(x = factor(ind, level=c("exact", "100", "75", "50", "25", "10", "6", "2")), y=values)) + 
    geom_point(stat='summary', fun='mean') +
    ggtitle("mean SSE for different values of n.quantiles", subtitle = str_replace_all(str_remove(t, "selection_bias_"), "_", " ")) +
    labs(x="number of quantiles", y = "SSE")
  
  
  cols_freq = str_detect(names(data), "split_slim")
  data_freq = data[, cols_freq, with = FALSE]
  
  table_empty = rep(0, length(unique(unlist(data_freq))))
  names(table_empty) = sort(unique(unlist(data_freq)))
  
  table_list = sapply(data_freq, function(el){
    res = tapply(c(table_empty, table(el)), names(c(table_empty, table(el))), sum)
  })
  saveRDS(table_list, paste0(savedir_slim, str_remove(t, "selection_bias_"),".rds"))
  options = length(unique(unlist(data_freq)))
  
  if(t %in% c("selection_bias_interaction_binary_numeric", "selection_bias_interaction_categorical_numeric") ){
    prob = c(0.45,0.45,0.1)
  } else{
    prob = rep(1/options, options)
  }
    
  chi2 = apply(table_list, 2, function(table){
    chisq.test(table, p = prob)[["p.value"]]
  }) 
  
  names(chi2) = str_remove_all(names(chi2), "split_slim_")

  data_chi2 = data.frame(p_value = chi2, n.quantile = names(chi2))
  
  p_chi2 = ggplot(data_chi2) +
    geom_point(aes(x = factor(n.quantile, level = c("exact", "100", "75", "50", "25", "10", "8", "6", "4", "2")), y = chi2)) +
    ggtitle("Selection bias for different values of n.quantiles", subtitle = str_replace_all(str_remove(t, "selection_bias_"), "_", " ")) +
    labs(x="number of quantiles", y = "p value")
  
  ggarrange(p_chi2, p_sse, nrow = 2) %>%
    ggexport(filename = paste0(figuredir_slim, str_remove(t, "selection_bias_"),".pdf"),
           width = 8, height = 3.8)
  
  
}

cols_sse = str_detect(names(selection_bias_slim[type == "selection_bias_independence_small"]), "sse")
data = stack(selection_bias_slim[type == "selection_bias_independence_small", cols_sse, with = FALSE])
ggplot(data, mapping=aes(x = ind, y=values))+geom_boxplot()

slim_full_interaction = readRDS("Data/simulations/batchtools/selection_bias_slim/results/full_interaction.rds")
slim_independence_small = readRDS("Data/simulations/batchtools/selection_bias_slim/results/independence_small.rds")
slim_independence_small_25 = readRDS("Data/simulations/batchtools/selection_bias_slim/results/independence_small_25.rds")
slim_independence_small_50 = readRDS("Data/simulations/batchtools/selection_bias_slim/results/independence_small_50.rds")

slim_independence_guide = readRDS("Data/simulations/batchtools/selection_bias_slim/results/guide.rds")
slim_interaction_small = readRDS("Data/simulations/batchtools/selection_bias_slim/results/interaction.rds")

slim_interaction_25 = readRDS("Data/simulations/batchtools/selection_bias_slim/results/interaction_25.rds")
slim_interaction_50 = readRDS("Data/simulations/batchtools/selection_bias_slim/results/interaction_50.rds")
slim_interaction_10 = readRDS("Data/simulations/batchtools/selection_bias_slim/results/interaction_25.rds")
slim_interaction_100 = readRDS("Data/simulations/batchtools/selection_bias_slim/results/interaction_50.rds")
slim_interaction_binary_numeric = readRDS("Data/simulations/batchtools/selection_bias_slim/results/interaction_binary_numeric.rds")
slim_interaction_categorical_numeric = readRDS("Data/simulations/batchtools/selection_bias_slim/results/interaction_categorical_numeric.rds")

slim_interaction_categorical_numeric = sapply(slim_interaction_categorical_numeric, function(el){
  if(length(el) == 2){
    res = c(el,0)
  } else{
    res = el
  }
})


library(kableExtra)
library(tidyr)
library(REdaS)




colnames(slim_full_interaction_three) = str_remove(colnames(slim_full_interaction_three), "split_slim_")
colnames(slim_interaction_binary_numeric) = str_remove(colnames(slim_interaction_binary_numeric), "split_slim_")
colnames(slim_interaction_categorical_numeric) = str_remove(colnames(slim_interaction_categorical_numeric), "split_slim_")


slim_interaction_categorical_numeric %>%
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



# pruning selection bias

list.files("Data/simulations/batchtools/selection_bias_pruning/results/", full.names = TRUE)
split_pruning = readRDS("Data/simulations/batchtools/selection_bias_pruning/results/selection_bias_pruning.rds")

savedir = "Data/simulations/batchtools/selection_bias_pruning/results/"
figuredir = "Figures/simulations/batchtools/selection_bias_pruning/"

if (!dir.exists(savedir)) dir.create(savedir, recursive = TRUE)

if (!dir.exists(figuredir)) dir.create(figuredir, recursive = TRUE)
cols_split = str_subset(colnames(tab), "split")
cols_split = c("split_slim_exact", "split_slim_100", "split_slim_50", "split_slim_10", "split_guide_excl_cat_corr", 
               "split_guide_incl_cat_corr", "split_mob", "split_ctree")

pruning_pairs = list(c(alpha = 1, impr.par = 0),
                    c(alpha = 0.05, impr.par = 0.05),
                    c(alpha = 0.01, impr.par = 0.1))

# replace splitvariable "leafnode" with NA

for(j in seq_along(split_pruning)){
  set(split_pruning, i=which(split_pruning[[j]]=="leafnode"), j=j, value=NA)
}


for (t in unique(split_pruning$type)){
  for(i in 1:length(pruning_pairs)){
    split_t_p = split_pruning[type == t  & alpha == pruning_pairs[[i]]["alpha"] & impr.par == pruning_pairs[[i]]["impr.par"], ]
    result = list(slim = table(split_t_p$split_slim_exact, useNA = "ifany"),
                  slim_100 = table(split_t_p$split_slim_100, useNA = "ifany"),
                  slim_10 = table(split_t_p$split_slim_10, useNA = "ifany"),
                  mob = table(split_t_p$split_mob, useNA = "ifany"),
                  ctree = table(split_t_p$split_ctree, useNA = "ifany"),
                  guide_inclcat = table(split_t_p$split_guide_incl_cat_corr, useNA = "ifany"),
                  guide_excllcat = table(split_t_p$split_guide_excl_cat_corr, useNA = "ifany")
                  # ,
                  # guide_biased_inclcat = table(tab_t_n$impr_guideincl_cat_biased),
                  # guide_biased_excllcat = table(tab_t_n$impr_guide_excl_cat_biased)
                  
    )
    saveRDS(result, file = paste0(savedir, str_remove(t, "selection_bias_"), "_alpha_",pruning_pairs[[i]]["alpha"], ".rds"))
    
    
    
    p = ggplot(stack(split_t_p[,cols_split, with = FALSE]),
               aes(x = values, color=ind, fill = ind)) +
      stat_count(position = "dodge") +
      ggtitle("Frequency of selection", subtitle = paste(str_replace_all(str_remove(t, "selection_bias_"), "_", " "))) +
      labs(x="selected variable", y="frequency", color = "surrogate", fill = "surrogate")
    
    ggexport(p, filename = paste0(figuredir, str_remove(t, "selection_bias_"), "_alpha_",pruning_pairs[[i]]["alpha"], ".pdf"), width = 8, height = 3.8)
    
  }
  
  
}






