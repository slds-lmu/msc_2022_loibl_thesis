library(REdaS)
library(kableExtra)


ades_basic = data.frame(alpha = c(0.001, 0.01, 0.05), impr.par = c(0.15, 0.1, 0.05))
pdes_basic = expand.grid(n = c(1500, 7500, 15000), type = c("linear_smooth", "linear_abrupt", "linear_mixed"))
experiments = merge(ades_basic, pdes_basic, by = NULL)


# ----- 1. Linear Smooth -----
# stand-alone

result_basic = readRDS("Data/simulations/batchtools/basic_scenarios/results/result_summary.rds")

list.files("Data/simulations/batchtools/basic_scenarios/results/")


result_basic$mean[n == 1500 & type == "linear_smooth" & surrogate == "standalone" & mbt == "GUIDE",
.(impr, n_leaves, r2_train, r2_test, ari)] %>%
  kbl(caption="Mean simulation results on 100 simulation runs for SLIM as stand alone model on scenario Linear smooth with n = 5000 for different values of impr ",
      format="latex",
      col.names = c("impr","number of leave nodes","R2 train","R2 test","ARI"),
      align="r") %>%
  kable_minimal(full_width = F)

# Comparison of performance and Stability for similar number of leaf nodes
# SLIM and GUIDE impr = 0.1, MOB and CTREE alpha = 0.001
parcoords(rbind(result_basic$mean[n == 1500 & type == "linear_smooth" & surrogate == "standalone" & mbt %in% c("SLIM", "GUIDE") & impr == 0.1,
                        .(mbt, n_leaves, r2_train, r2_test, ari)],
      result_basic$mean[n == 1500 & type == "linear_smooth" & surrogate == "standalone" & mbt %in% c("MOB", "CTree") & alpha == 0.001,
                        .(mbt, n_leaves, r2_train, r2_test, ari)]),
      rownames = FALSE, 
                color = list(colorBy = "mbt",
                             colorScale = "scaleOrdinal",
                             colorScheme = "schemeCategory10"),
                withD3 = TRUE)

res_ls_n1500_alpha001 = readRDS("Data/simulations/batchtools/basic_scenarios/results/1_res_experiments.rds")
mob_ls_n1500_alpha001 = unique(res_ls_n1500_alpha001[mbt == "MOB" & surrogate == "lm", .(job.id, n_leaves)])
setnames(mob_ls_n1500_alpha001, "n_leaves", "n_leaves_001")

res_ls_n1500_alpha01 = readRDS("Data/simulations/batchtools/basic_scenarios/results/2_res_experiments.rds")
mob_ls_n1500_alpha01 = unique(res_ls_n1500_alpha01[mbt == "MOB" & surrogate == "lm", .(job.id, n_leaves)])
setnames(mob_ls_n1500_alpha01, "n_leaves", "n_leaves_01")

stability_ls = rbind(res_ls_n1500_alpha001[mbt %in% c("MOB", "CTree") & surrogate == "standalone",.(ari, mbt)],
                     res_ls_n1500_alpha01[mbt %in% c("SLIM", "GUIDE") & surrogate == "standalone",.(ari, mbt)], use.names = FALSE)
ggplot(stability_ls,
       aes(x = mbt, y = ari)) +
  stat_boxplot(geom = "boxplot") 

performance_ls = rbind(unique(res_ls_n1500_alpha001[mbt %in% c("MOB", "CTree") & surrogate == "standalone",.(r2_train, r2_test, mbt, job.id, config_id)]),
                       unique(res_ls_n1500_alpha01[mbt %in% c("SLIM", "GUIDE") & surrogate == "standalone",.(r2_train, r2_test, mbt, job.id, config_id)]), use.names = FALSE)

ggplot(performance_ls,
       aes(x = mbt, y = r2_train)) +
  stat_boxplot(geom = "boxplot") 

ggplot(performance_ls,
       aes(x = mbt, y = r2_test)) +
  stat_boxplot(geom = "boxplot") 




# LM as underlying blackbox model

result_basic$mean[n == 1500 & type == "linear_mixed" & surrogate == "xgboost" & mbt == "MOB",
                  .(alpha, n_leaves, r2_train, r2_test, ari)] %>%
  kbl(caption="Mean simulation results on 100 simulation runs for SLIM as stand alone model on scenario Linear smooth with n = 5000 for different values of impr ",
      format="latex",
      col.names = c("impr","number of leave nodes","R2 train","R2 test","ARI"),
      align="r") %>%
  kable_minimal(full_width = F)




View(cbind(mob_ls_n1500_alpha001,mob_ls_n1500_alpha01$n_leaves_01, mob_ls_n1500_alpha01$n_leaves_01 == mob_ls_n1500_alpha001$n_leaves_001))
mean(as.numeric(mob_ls_n1500_alpha01$n_leaves_01))



# ---- 2. Linear abrupt
View(result_basic$mean[n == 7500 & type == "linear_abrupt" & surrogate == "standalone" ,
                  .(mbt, impr, n_leaves, r2_train, r2_test, ari)])


result_basic$mean[n == 1500 & type == "linear_abrupt" & surrogate == "standalone" & mbt == "GUIDE",
                  .(impr, n_leaves, r2_train, r2_test, ari)] %>%
  kbl(caption="Mean simulation results on 100 simulation runs for SLIM as stand alone model on scenario Linear smooth with n = 5000 for different values of impr ",
      format="latex",
      col.names = c("impr","number of leave nodes","R2 train","R2 test","ARI"),
      align="r") %>%
  kable_minimal(full_width = F)
