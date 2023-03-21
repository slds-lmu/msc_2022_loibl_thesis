library(kableExtra)



# Reduce results
source("R/simulations/batchtools/reduce_results.R")
reg_corr = loadRegistry("Data/simulations/batchtools/correlated_data/batchtools/"
                        ,conf.file = NA
)
ades_corr = NULL
pdes_corr = expand.grid(n = c(1500), type = c("linear_smooth_corr"), rho = c(0.1, 0.5, 0.9))

savedir_corr = "Data/simulations/batchtools/correlated_data/results/"

result_corr = reduce_trees(ades_corr, pdes_corr, savedir_corr, reg_corr)$mean


# analyse results correlated data
result_corr = readRDS("Data/simulations/batchtools/correlated_data/results/result_summary.rds")
result_corr_sd = result_corr$sd
setnames(result_corr_sd, c("r2_train", "r2_test"), c("r2_train_sd", "r2_test_sd"))
result_corr_mean = cbind(result_corr$mean, result_corr_sd[,.(r2_train_sd, r2_test_sd)])



result_corr_mean[,.(surrogate,mbt, rho, x_wrong, n_leaves, n_leaves_min, n_leaves_max, r2_train, r2_train_sd, r2_test, r2_test_sd)] %>%
  arrange(.,desc(surrogate), rho, desc(mbt)) %>%
  kbl(caption="Mean simulation results on 250 simulation runs as stand alone model and surrogate on lm predictions on scenario Linear Smooth - Correlated with n = 1000, alpha = 0.001, impr = 0.01",
      format="latex",
      col.names = c("black box", "MBT", "rho", "x1", "n leaves", "n leaves min", "n leaves max", "R2 train", "R2 train sd", "R2 test", "R2 test sd"),
      align="r",
      digits = 4) %>%
  kable_minimal(full_width = F)

