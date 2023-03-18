library(dplyr)
library(REdaS)
library(kableExtra)
library(batchtools)
library(data.table)
library(GGally)
library(ggpubr)
library(stringr)
# load results

list.files("Data/simulations/batchtools/selection_bias_general/results/", full.names = TRUE)
tab = readRDS("Data/simulations/batchtools/selection_bias_general/results/selection_bias_general.rds")

savedir = "Data/simulations/batchtools/selection_bias_general/results/"
figuredir = "Figures/simulations/batchtools/selection_bias_general/"

if (!dir.exists(savedir)) dir.create(savedir, recursive = TRUE)

if (!dir.exists(figuredir)) dir.create(figuredir, recursive = TRUE)
cols_split = c("SLIM", "MOB", "CTree")
setnames(tab, c("split_slim_exact",  
                "split_mob", "split_ctree"), c("SLIM", "MOB", "CTree"))

colors_mbt =c("SLIM" = 'purple', "SLIM low symmetry" = "purple3", "GUIDE" ='olivedrab3',  "GUIDE excl cat" = 'olivedrab3', "GUIDE incl cat" = 'olivedrab4', 
              "MOB" ='skyblue', "CTree" = 'salmon')


# ----- 1. Selection Bias Independence -----


# Independence numerical
res_independence_numerical = tab[type == "selection_bias_independence_small", ]

setnames(res_independence_numerical, "split_guide_incl_cat_corr","GUIDE")

p_independence_numerical = ggplot(stack(res_independence_numerical[,c(cols_split, "GUIDE"), with = FALSE]),
           aes(x = values, fill = ind)) +
  stat_count(position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = colors_mbt) +
  theme_bw() +
  labs(x="splitting variable", y="frequency", fill = "MBT")

ggexport(p_independence_numerical, filename = paste0(figuredir, "independence_numerical.png"), width = 800, height = 300)


# Independence mixed

res_independence_mixed = tab[type == "selection_bias_independence", ]

setnames(res_independence_mixed, c("split_guide_excl_cat_corr", "split_guide_incl_cat_corr"),c("GUIDE excl cat", "GUIDE incl cat"))

p_independence_mixed = ggplot(stack(res_independence_mixed[,c(cols_split, "GUIDE excl cat", "GUIDE incl cat"), with = FALSE]),
                                  aes(x = values, fill = ind)) +
  stat_count(position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = colors_mbt) +
  theme_bw() +
  labs(x="splitting variable", y="frequency", fill = "MBT")

ggexport(p_independence_mixed, filename = paste0(figuredir, "independence_mixed.png"), width = 800, height = 300)



# ---- 2. Selection Bias / Splitting behaviour Interactions ----




plot_list = list()
interaction_types = c("selection_bias_interaction_numerical_vs_numrical",
                      "selection_bias_interaction_binary_vs_categorical",
                      "selection_bias_interaction_numerical_vs_binary",
                      "selection_bias_interaction_numerical_vs_categorical")
type_names = c("numerical vs numerical", "binary vs categorical", "numerical vs binary", "numerical vs categorical")
for (t in seq_along(interaction_types)){
    res_t = tab[type == interaction_types[t], ]

      setnames(res_t, c("split_guide_incl_cat_corr"),c("GUIDE"))
    
    plot_list[[t]] = ggplot(stack(res_t[,c(cols_split, "GUIDE"), with = FALSE]),
                            aes(x = values, fill = ind)) +
      stat_count(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = colors_mbt) +
      ggtitle(type_names[t]) + 
      scale_y_continuous(limits = c(0, 1000)) +
      theme_bw() +
      labs(x="splitting variable", y="frequency", fill = "MBT")
}

p_interactions = ggarrange(plotlist = plot_list, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")
ggexport(p_interactions, filename = paste0(figuredir, "interactions.png"), width = 800, height = 400)









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



for (t in unique(split_data$type)){
  for (n_data in unique(split_data[type == t , n])){
    tab_t_n = split_data[type == t & n == n_data, ]

    
    cols_split = str_detect(colnames(tab_t_n), "split")
    
    p = ggplot(stack(tab_t_n[,cols_split, with = FALSE]),
               aes(x = values, color=ind, fill = ind)) +
      stat_count(position = "dodge") +
      ggtitle("Frequency of selection", subtitle = paste(str_replace_all(str_remove(t, "selection_bias_"), "_", " "))) +
      theme_bw() +
      labs(x="selected variable", y="frequency", color = "surrogate", fill = "surrogate")
    
    ggexport(p, filename = paste0(figuredir, str_remove(t, "selection_bias_"), ".pdf"), width = 8, height = 3.8)
    
  }
}





# ---- 4. Slim selection bias correction approach - different values of n.quantiles ----
list.files("Data/simulations/batchtools/selection_bias_slim/results/", full.names = TRUE)
selection_bias_slim = readRDS("Data/simulations/batchtools/selection_bias_slim/results/selection_bias_slim.rds")

figuredir_slim = "Figures/simulations/batchtools/selection_bias_slim/"
savedir_slim = "Data/simulations/batchtools/selection_bias_slim/results/"

if (!dir.exists(figuredir_slim)) dir.create(figuredir_slim, recursive = TRUE)

type_independence = c("selection_bias_independence_10", "selection_bias_independence_25", 
                      "selection_bias_independence_50", "selection_bias_independence_100")
p_list_independence = list()
for(t in type_independence){
  data = selection_bias_slim[type == t]
  cols_mse = str_detect(names(data), "mse_slim")
  data_long_mse_train = stack(data[, cols_mse, with = FALSE])
  data_long_mse_train$ind = str_remove_all(data_long_mse_train$ind, "mse_slim_")

  cols_mse_test = str_detect(names(data), "mse_test_slim")
  data_long_mse_test = stack(data[, cols_mse_test, with = FALSE])
  data_long_mse_test$ind = str_remove_all(data_long_mse_test$ind, "mse_test_slim_")
  
  data_long_mse = rbind(cbind(data_long_mse_train, type = "train"), cbind(data_long_mse_test, type = "test"))
  p_mse = ggplot(data_long_mse, mapping = aes(x = factor(ind, level=c("exact", "100", "75", "50", "25", "10", "6", "2")), 
                                              y=values, color = type)) + 
    geom_point(stat='summary', fun='mean') +
    theme_bw() +
    labs(x=NULL, y = "MSE") 
    
  
  if(t == "selection_bias_independence_100"){
    p_mse = p_mse + labs(x="number of quantiles", y = "mean MSE", color = "data set") +
      theme(legend.position="bottom")
  } else{
    p_mse = p_mse + theme(axis.title.x=element_blank(),
                            legend.position="none")
  }
  
  cols_freq = str_detect(names(data), "split_slim")
  data_freq = data[, cols_freq, with = FALSE]
  
  table_empty = rep(0, length(unique(unlist(data_freq))))
  names(table_empty) = sort(unique(unlist(data_freq)))
  
  table_list = sapply(data_freq, function(el){
    res = tapply(c(table_empty, table(el)), names(c(table_empty, table(el))), sum)
  }, simplify = TRUE)
  saveRDS(table_list, paste0(savedir_slim, str_remove(t, "selection_bias_"),".rds"))

  options = length(unique(unlist(data_freq)))
  prob = rep(1/options, options)

    
  chi2 = apply(table_list, 2, function(table){
    chisq.test(table, p = prob)[["p.value"]]
  }) 
  
  names(chi2) = str_remove_all(names(chi2), "split_slim_")
  data_chi2 = data.frame(p_value = chi2, n.quantile = names(chi2))
  
  p_chi2 = ggplot(data_chi2) +
    geom_point(aes(x = factor(n.quantile, level = c("exact", "100", "75", "50", "25", "10", "8", "6", "4", "2")), y = chi2)) +
    ggtitle(label = NULL, subtitle = str_replace_all(str_remove(t, "selection_bias_"), "_", " ")) +
    theme_bw() +
    ylim(0,1) +
    # geom_hline(yintercept = 0.05, color="green") +
    labs(x=NULL, y = "p value") +
    theme(axis.title.x=element_blank())
    

  p_list_independence[[t]] =  list(p_chi2,p_mse)

  
  
  if(t == "selection_bias_independence_100"){
    ggexport(ggarrange(p_chi2, p_mse, nrow = 2, heights = c(1,1.3)), filename = paste0(figuredir_slim, str_remove(t, "selection_bias_"),".png"),
             width = 700, height = 300)
  } else{
    ggexport(ggarrange(p_chi2, p_mse, nrow = 2), filename = paste0(figuredir_slim, str_remove(t, "selection_bias_"),".png"),
             width = 700, height = 250)
  }
}

cols_mse = str_detect(names(selection_bias_slim[type == "selection_bias_independence_small"]), "mse")
data = stack(selection_bias_slim[type == "selection_bias_independence_small", cols_mse, with = FALSE])
ggplot(data, mapping=aes(x = ind, y=values))+geom_boxplot()

slim_full_interaction = readRDS("Data/simulations/batchtools/selection_bias_slim/results/full_interaction.rds")
slim_independence_small = readRDS("Data/simulations/batchtools/selection_bias_slim/results/independence_small.rds")
slim_independence_100 = readRDS("Data/simulations/batchtools/selection_bias_slim/results/independence_100.rds")
slim_independence_50 = readRDS("Data/simulations/batchtools/selection_bias_slim/results/independence_50.rds")

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

list.files("Data/simulations/batchtools/selection_bias_basic_pruning/results/", full.names = TRUE)
split_pruning = readRDS("Data/simulations/batchtools/selection_bias_basic_pruning/results/selection_bias_basic_pruning.rds")

savedir = "Data/simulations/batchtools/selection_bias_basic_pruning/results/"
figuredir = "Figures/simulations/batchtools/selection_bias_basic_pruning/"

if (!dir.exists(savedir)) dir.create(savedir, recursive = TRUE)

if (!dir.exists(figuredir)) dir.create(figuredir, recursive = TRUE)
cols_split = str_subset(colnames(split_pruning), "split")
cols_split = c("split_slim_exact", "split_slim_100", "split_slim_50", "split_slim_10", 
               "split_guide_incl_cat_corr", "split_mob", "split_ctree")

# pruning_pairs = list(c(alpha = 1, impr.par = 0),
#                     c(alpha = 0.05, impr.par = 0.04),
#                     c(alpha = 0.01, impr.par = 0.06))

pruning_pairs = list(c(alpha = 1, impr.par = 0),
                    c(alpha = 0.05, impr.par = 0.02))



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






