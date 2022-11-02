# Analysis of performance

library(ggpubr)
library(stringr)

load("Data/simulations/simulation_study/performance/performance_linear.RData")


simulation_list = list(linear_smooth = linear_smooth,
                       linear_abrupt = linear_abrupt,
                       categorical_linear = categorical_linear,
                       linear_mixed = linear_mixed)

if (!dir.exists("Figures")) dir.create("Figures", recursive = TRUE)
if (!dir.exists("Figures/Performance")) dir.create("Figures/Performance", recursive = TRUE)


for(el in names(simulation_list)){
  if (!dir.exists(paste0("Figures/Performance/", el))) dir.create(paste0("Figures/Performance/", el), recursive = TRUE)
  
  p_nofnodes = ggplot(stack(data.frame(original = simulation_list[[el]]$interpr_orig, 
                                       surrogate = simulation_list[[el]]$interpr_surr)),
                      aes(x = ind, y = values)) +
    stat_boxplot(geom = "errorbar", width = 0.5) +
    ggtitle("Number of terminal nodes", subtitle = str_replace(el, "_", " ")) +
    labs(x="model", y="number of terminal nodes") +
    geom_boxplot()
  
  ggexport(p_nofnodes, filename = paste0("Figures/Performance/", el, "/nofnodes.pdf"), width = 7, height = 3.8)
  
  
  p_r2_train = ggplot(stack(data.frame(blackbox = simulation_list[[el]]$r2_ml_train,
                                       original = simulation_list[[el]]$r2_acc_train,
                                       surrogate = simulation_list[[el]]$r2_fid_train)),
                      aes(x = ind, y = values)) +
    stat_boxplot(geom = "errorbar", width = 0.5) +
    ggtitle("R squared - Training", subtitle = str_replace(el, "_", " ")) +
    labs(x="model", y="R2") +
    geom_boxplot()
  
  ggexport(p_r2_train, filename = paste0("Figures/Performance/", el, "/r2_train.pdf"), width = 7, height = 3.8)
  
  p_r2_test = ggplot(stack(data.frame(blackbox = simulation_list[[el]]$r2_ml_test,
                                      original = simulation_list[[el]]$r2_acc_test, 
                                      surrogate = simulation_list[[el]]$r2_fid_test)),
                     aes(x = ind, y = values)) +
    stat_boxplot(geom = "errorbar", width = 0.5) +
    ggtitle("R squared - Test", subtitle = str_replace(el, "_", " ")) +
    labs(x="model", y="R2") +
    geom_boxplot()
  
  ggexport(p_r2_test, filename = paste0("Figures/Performance/", el, "/r2_test.pdf"), width = 7, height = 3.8)
  
  
}
