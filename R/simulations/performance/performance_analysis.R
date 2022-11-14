# Analysis of performance

library(ggpubr)
library(stringr)

load("Data/simulations/simulation_study/performance/performance_linear.RData")


simulation_list = list(linear_smooth = linear_smooth,
                       linear_abrupt = linear_abrupt,
                       categorical_linear = categorical_linear,
                       linear_mixed = linear_mixed,
                       mixed_large = mixed_large
)


if (!dir.exists("Figures")) dir.create("Figures", recursive = TRUE)
if (!dir.exists("Figures/Performance")) dir.create("Figures/Performance", recursive = TRUE)


for(el in names(simulation_list)){
  if (!dir.exists(paste0("Figures/Performance/", el))) dir.create(paste0("Figures/Performance/", el), recursive = TRUE)
  
  col_res_orig = colnames(simulation_list[[el]]$result_original)
  col_res_surr_lm = colnames(simulation_list[[el]]$result_surrogate_lm)
  col_res_surr_xgboost = colnames(simulation_list[[el]]$result_surrogate_xgboost)
  
  p_nofnodes = ggplot(stack(data.frame(orig = simulation_list[[el]]$result_original[,str_detect(col_res_orig, "n_term_nodes")], 
                                       surr_lm = simulation_list[[el]]$result_surrogate_lm[,str_detect(col_res_surr_lm, "n_term_nodes")],
                                       surr_xgboost = simulation_list[[el]]$result_surrogate_xgboost[,str_detect(col_res_surr_xgboost, "n_term_nodes")])),
                      aes(x = ind, y = values)) +
    stat_boxplot(geom = "errorbar", width = 0.5) +
    ggtitle("Number of terminal nodes", subtitle = str_replace(el, "_", " ")) +
    labs(x="model", y="number of terminal nodes") +
    geom_boxplot()
  
  ggexport(p_nofnodes, filename = paste0("Figures/Performance/", el, "/nofnodes.pdf"), width = 12, height = 3.8)
  
  
  r2_train_orig = simulation_list[[el]]$result_original[,str_detect(col_res_orig,"r2_train")]
  colnames(r2_train_orig) = str_remove(colnames(r2_train_orig), "r2_train_")
  
  r2_train_surr_lm = simulation_list[[el]]$result_surrogate_lm[,str_detect(col_res_surr_lm, "r2_train")]
  colnames(r2_train_surr_lm) = str_remove(colnames(r2_train_surr_lm), "r2_train_")
  
  p_r2_train_lm = ggplot(stack(data.frame(lm = simulation_list[[el]]$lm$r2_train_lm,
                                          orig = r2_train_orig,
                                          surr = r2_train_surr_lm)),
                         aes(x = ind, y = values)) +
    stat_boxplot(geom = "errorbar", width = 0.5) +
    ggtitle("R squared - Training", subtitle = str_replace(el, "_", " ")) +
    labs(x="model", y="R2") +
    geom_boxplot()
  
  ggexport(p_r2_train_lm, filename = paste0("Figures/Performance/", el, "/lm_r2_train.pdf"), width = 12, height = 3.8)
  
  r2_train_surr_xgboost = simulation_list[[el]]$result_surrogate_xgboost[,str_detect(col_res_surr_xgboost, "r2_train")]
  colnames(r2_train_surr_xgboost) = str_remove(colnames(r2_train_surr_xgboost), "r2_train_")
  
  p_r2_train_xgboost = ggplot(stack(data.frame(xgboost = simulation_list[[el]]$xgboost$r2_train_xgboost,
                                          orig = r2_train_orig,
                                          surr = r2_train_surr_xgboost)),
                         aes(x = ind, y = values)) +
    stat_boxplot(geom = "errorbar", width = 0.5) +
    ggtitle("R squared - Training", subtitle = str_replace(el, "_", " ")) +
    labs(x="model", y="R2") +
    geom_boxplot()
  
  ggexport(p_r2_train_xgboost, filename = paste0("Figures/Performance/", el, "/xgboost_r2_train.pdf"), width = 12, height = 3.8)
  

  
  p_r2_test_lm = ggplot(stack(data.frame(lm = simulation_list[[el]]$lm$r2_test_lm,
                                          orig = simulation_list[[el]]$result_original[,str_detect(col_res_orig, "r2_test")],
                                          surr = simulation_list[[el]]$result_surrogate_lm[,str_detect(col_res_surr_lm, "r2_test")])),
                         aes(x = ind, y = values)) +
    stat_boxplot(geom = "errorbar", width = 0.5) +
    ggtitle("R squared - testing", subtitle = str_replace(el, "_", " ")) +
    labs(x="model", y="R2") +
    geom_boxplot()
  
  ggexport(p_r2_test_lm, filename = paste0("Figures/Performance/", el, "/lm_r2_test.pdf"), width = 12, height = 3.8)
  
  p_r2_test_xgboost = ggplot(stack(data.frame(xgboost = simulation_list[[el]]$xgboost$r2_test_xgboost,
                                               orig = simulation_list[[el]]$result_original[,str_detect(col_res_orig, "r2_test")],
                                               surr = simulation_list[[el]]$result_surrogate_xgboost[,str_detect(col_res_surr_xgboost, "r2_test")])),
                              aes(x = ind, y = values)) +
    stat_boxplot(geom = "errorbar", width = 0.5) +
    ggtitle("R squared - testing", subtitle = str_replace(el, "_", " ")) +
    labs(x="model", y="R2") +
    geom_boxplot()
  
  ggexport(p_r2_test_xgboost, filename = paste0("Figures/Performance/", el, "/xgboost_r2_test.pdf"), width = 12, height = 3.8)
  
  
  
}
