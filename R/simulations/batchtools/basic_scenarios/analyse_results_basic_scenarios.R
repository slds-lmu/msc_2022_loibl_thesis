library(REdaS)
library(kableExtra)


ades_basic = data.frame(alpha = c(0.001, 0.01, 0.05), impr.par = c(0.15, 0.1, 0.05))
pdes_basic = expand.grid(n = c(1500, 7500, 15000), type = c("linear_smooth", "linear_abrupt", "linear_mixed"))
experiments = merge(ades_basic, pdes_basic, by = NULL)


# ----- 1. Linear Smooth -----
# stand-alone

result_basic = readRDS("Data/simulations/batchtools/basic_scenarios/results/result_summary.rds")
result_basic_mean = result_basic$mean
result_basic_mean[type == "linear_mixed" & mbt == "lm" & n == 1500]
list.files("Data/simulations/batchtools/basic_scenarios/results/")

# mbt_group = as.factor(result_basic$mean[n == 1500 & type == "linear_smooth" & surrogate == "standalone" ,mbt])
# alpha_group = as.factor(result_basic$mean[n == 1500 & type == "linear_smooth" & surrogate == "standalone" ,alpha])
# pairs(result_basic$mean[n == 1500 & type == "linear_smooth" & surrogate == "standalone",
#                         .(n_leaves, r2_train, ari)],
#       col = c("red", "cornflowerblue", "purple", "green")[mbt_group],
#       pch = c(19, 20, 22)[alpha_group], 
#       labels = c("leaves", "r2", "ari"))



result_basic$mean[n == 1500 & type == "linear_smooth" & surrogate == "standalone" & mbt == "MOB",
.(impr, n_leaves, r2_train, r2_test, ari)] %>%
  kbl(caption="Mean simulation results on 100 simulation runs for SLIM as stand alone model on scenario Linear smooth with n = 1000 for different values of impr ",
      format="latex",
      col.names = c("impr","number of leaf nodes","R2 train","R2 test","ARI"),
      align="r",
      digits = 4) %>%
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

res_ls_n1500_alpha01 = readRDS("Data/simulations/batchtools/basic_scenarios/results_test/2_res_experiments.rds")
mob_ls_n1500_alpha01 = unique(res_ls_n1500_alpha01[mbt == "MOB" & surrogate == "lm", .(job.id, n_leaves)])
setnames(mob_ls_n1500_alpha01, "n_leaves", "n_leaves_01")


test = unique(res_ls_n1500_alpha01[mbt %in% c("SLIM", "GUIDE", "MOB", "CTree") & surrogate == "lm",.(n_leaves, r2_train, mbt, job.id, config_id, ri, ari_fixed, rc, stability_same_size)])
table(test$mbt)
library(parcoords)
parcoords(test[!is.na(ri),.(mbt, n_leaves, ri)], rownames = FALSE,  
          color = list(colorBy = "mbt",colorScale = "scaleOrdinal",colorScheme = "schemeCategory10"),
          withD3 = TRUE,
          brushMode = "1D-axes-multi")

library(GGally)

ggpairs(test[!is.na(ri) & stability_same_size == TRUE ,.(n_leaves, ri, r2_train, mbt)],
        columns = 1:3,        # Columns
        aes(color = mbt,  # Color by group (cat. variable)
            alpha = 0.5))

ggpairs(unique(test[ ,.(n_leaves, r2_train, mbt, job.id, config_id)]),
        columns = 1:2,        # Columns
        # aes(color = mbt,  # Color by group (cat. variable)
        #     alpha = 0.5)
        )


save_dir = "Figures/simulations/batchtools/basic_scenarios/linear_smooth/"
if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

# --- Interpretability ----
interpretabiliy_ls = rbind(unique(res_ls_n1500_alpha001[mbt %in% c("MOB", "CTree"),.(n_leaves, mbt, job.id, config_id, surrogate)]),
                           unique(res_ls_n1500_alpha01[mbt %in% c("SLIM", "GUIDE"),.(n_leaves, mbt, job.id, config_id, surrogate)]), use.names = FALSE)

# Standalone
p_ls_1000_standalone_int = ggplot(interpretabiliy_ls[surrogate == "standalone",],
       aes(x = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")), y = n_leaves)) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(fun="mean", color = "grey") +
  ggtitle("Interpretability of stand alone MBTs Linear Smooth", subtitle = "n = 1000, alpha = 0.001, impr = 0.1") +
  labs(x="MBT", y="Number of terminal leaves")

ggexport(p_ls_1000_standalone_int, filename = paste0(save_dir, "ls_1000_standalone_int.pdf"), width = 7, height = 4)

# Standalone & as surrogate on lm
interpretabiliy_ls[, mbt_surrogate := paste(mbt, surrogate)]
p_ls_1000_standalone_lm_int = ggplot(interpretabiliy_ls[surrogate %in% c("standalone", "lm"),],
                                  aes(x = factor(mbt_surrogate, levels = 
                                                   c("SLIM standalone", "SLIM lm", 
                                                     "GUIDE standalone", "GUIDE lm",  
                                                     "MOB standalone", "MOB lm",
                                                     "CTree standalone", "CTree lm")), y = n_leaves)) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(fun="mean", color = "grey") +
  ggtitle("Interpretability of stand alone MBTs Linear Smooth", subtitle = "n = 1000, alpha = 0.001, impr = 0.1") +
  labs(x="MBT as standalone or as surrogate on lm predictions", y="Number of terminal leaves")
ggexport(p_ls_1000_standalone_lm_int, filename = paste0(save_dir, "ls_1000_standalone_lm_int.pdf"), width = 10, height = 4)



# ---- Stability -----
stability_ls = rbind(res_ls_n1500_alpha001[mbt %in% c("MOB", "CTree") ,.(ari, mbt, surrogate)],
                     res_ls_n1500_alpha01[mbt %in% c("SLIM", "GUIDE") ,.(ari, mbt, surrogate)], use.names = FALSE)


# Standalone
p_ls_1000_standalone_sta = ggplot(stability_ls[surrogate == "standalone"],
                                  aes(x = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")), y = ari)) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(fun="mean", color = "grey")+
  ggtitle("Stability of stand alone MBTs Linear Smooth", subtitle = "n = 1000, alpha = 0.001, impr = 0.1") +
  labs(x="MBT", y="ARI")

ggexport(p_ls_1000_standalone_sta, filename = paste0(save_dir, "ls_1000_standalone_sta.pdf"), width = 7, height = 4)


# Standalone & as surrogate on lm

stability_ls[, mbt_surrogate := paste(mbt, surrogate)]
p_ls_1000_standalone_lm_sta = ggplot(stability_ls[surrogate %in% c("standalone", "lm")],
                                  aes(x = factor(mbt_surrogate, levels = c("SLIM standalone", "SLIM lm", 
                                                                 "GUIDE standalone", "GUIDE lm",  
                                                                 "MOB standalone", "MOB lm",
                                                                 "CTree standalone", "CTree lm")), y = ari)) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(fun="mean", color = "grey")+
  ggtitle("Stability Linear Smooth", subtitle = "n = 1000, alpha = 0.001, impr = 0.1") +
  labs(x="MBT as standalone or as surrogate on lm predictions", y="ARI")
ggexport(p_ls_1000_standalone_lm_sta, filename = paste0(save_dir, "ls_1000_standalone_lm_sta.pdf"), width = 10, height = 4)


# ---- Performance ----
performance_ls = rbind(unique(res_ls_n1500_alpha001[mbt %in% c("MOB", "CTree"),.(r2_train, r2_test, mbt, job.id, config_id, surrogate)]),
                       unique(res_ls_n1500_alpha01[mbt %in% c("SLIM", "GUIDE"),.(r2_train, r2_test, mbt, job.id, config_id, surrogate)]),
                       unique(res_ls_n1500_alpha01[mbt %in% c("lm", "xgboost") ,.(r2_train, r2_test, mbt, job.id, config_id, surrogate)]), use.names = FALSE)

# Standalone
p_ls_1000_standalone_r2_train = ggplot(performance_ls[surrogate == "standalone"|mbt %in% c("lm", "xgboost")],
                                       aes(x = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree", "lm", "xgboost")), y = r2_train)) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(fun="mean", color = "grey")+
  ggtitle("Accuracy of stand alone MBTs Linear Smooth Train", subtitle = "n = 1000, alpha = 0.001, impr = 0.1") +
  labs(x="MBT", y="R2")

ggexport(p_ls_1000_standalone_r2_train, filename = paste0(save_dir, "ls_1000_standalone_r2_train.pdf"), width = 7, height = 4)


# Standalone & as surrogate on lm

performance_ls[, mbt_surrogate := paste(mbt, surrogate)]

p_ls_1000_standalone_lm_r2_train = ggplot(performance_ls[surrogate %in% c("standalone", "lm") & !(mbt %in% c("lm", "xgboost"))],
                                          aes(x = factor(mbt_surrogate, levels = c("SLIM standalone", "SLIM lm", 
                                                                                   "GUIDE standalone", "GUIDE lm",  
                                                                                   "MOB standalone", "MOB lm",
                                                                                   "CTree standalone", "CTree lm")), y = r2_train)) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(fun="mean", color = "grey")+
  ggtitle("Accuracy and Fidelity Linear Smooth Train", subtitle = "n = 1000, alpha = 0.001, impr = 0.1") +
  labs(x="MBT as standalone or as surrogate on lm predictions", y="R2")
ggexport(p_ls_1000_standalone_lm_r2_train, filename = paste0(save_dir, "ls_1000_standalone_lm_r2_train.pdf"), width = 10, height = 4)



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





# ------ 2. Linear abrupt ------
result_basic = readRDS("Data/simulations/batchtools/basic_scenarios/results/result_summary.rds")

View(result_basic$mean[n == 1500 & type == "linear_abrupt" & surrogate == "lm" & mbt %in% c("GUIDE", "SLIM"),
                  .(mbt, impr, n_leaves, r2_train, r2_test, ari)])


result_basic$mean[n == 1500 & type == "linear_abrupt" & surrogate == "standalone" & mbt %in% c("CTree"),
                  .(alpha,n_leaves, r2_train, r2_test, ari)] %>%
  kbl(caption="Mean simulation results on 100 simulation runs for CTree as stand alone model on scenario Linear abrupt with n = 1000 for different values of impr ",
      format="latex",
      col.names = c("alpha", "number of leaf nodes","R2 train","R2 test","ARI"),
      align="r",
      digits = 4) %>%
  kable_minimal(full_width = F)


# alpha = 0.001 for mob and ctree
res_la_n1500_alpha001 = readRDS("Data/simulations/batchtools/basic_scenarios/results/10_res_experiments.rds")

# alpha = 0.05 i.e. impr = 0.05 for slim and guide
res_la_n1500_alpha05 = readRDS("Data/simulations/batchtools/basic_scenarios/results/12_res_experiments.rds")

save_dir = "Figures/simulations/batchtools/basic_scenarios/linear_abrupt/"
if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

# --- Interpretability ----
interpretabiliy_la = rbind(unique(res_la_n1500_alpha001[mbt %in% c("MOB", "CTree"),.(n_leaves, mbt, job.id, config_id, surrogate)]),
                           unique(res_la_n1500_alpha05[mbt %in% c("SLIM", "GUIDE"),.(n_leaves, mbt, job.id, config_id, surrogate)]), use.names = FALSE)


# Standalone & as surrogate on lm
interpretabiliy_la[, mbt_surrogate := paste(mbt, surrogate)]
p_la_1000_standalone_lm_int = ggplot(interpretabiliy_la[surrogate %in% c("standalone", "lm"),],
                                     aes(x = factor(mbt_surrogate, levels = 
                                                      c("SLIM standalone", "SLIM lm", 
                                                        "GUIDE standalone", "GUIDE lm",  
                                                        "MOB standalone", "MOB lm",
                                                        "CTree standalone", "CTree lm")), y = n_leaves)) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(fun="mean", color = "grey") +
  ggtitle("Interpretability of stand alone MBTs Linear Abrupt", subtitle = "n = 1000, alpha = 0.001, impr = 0.05") +
  labs(x="MBT as standalone or as surrogate on lm predictions", y="Number of terminal leaves")
ggexport(p_la_1000_standalone_lm_int, filename = paste0(save_dir, "la_1000_standalone_lm_int.pdf"), width = 10, height = 4)


p_la_1000_standalone_xgboost_int = ggplot(interpretabiliy_la[surrogate %in% c("standalone", "xgboost"),],
                                     aes(x = factor(mbt_surrogate, levels = 
                                                      c("SLIM standalone", "SLIM xgboost", 
                                                        "GUIDE standalone", "GUIDE xgboost",  
                                                        "MOB standalone", "MOB xgboost",
                                                        "CTree standalone", "CTree xgboost")), y = n_leaves)) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(fun="mean", color = "grey") +
  ggtitle("Interpretability of stand alone MBTs Linear Abrupt", subtitle = "n = 1000, alpha = 0.001, impr = 0.05") +
  labs(x="MBT as standalone or as surrogate on xgboost predictions", y="Number of terminal leaves")
ggexport(p_la_1000_standalone_xgboost_int, filename = paste0(save_dir, "la_1000_standalone_xgboost_int.pdf"), width = 10, height = 4)


# ---- Stability -----
stability_la = rbind(res_la_n1500_alpha001[mbt %in% c("MOB", "CTree") ,.(ari, mbt, surrogate)],
                     res_la_n1500_alpha05[mbt %in% c("SLIM", "GUIDE") ,.(ari, mbt, surrogate)], use.names = FALSE)



# Standalone & as surrogate on lm

stability_la[, mbt_surrogate := paste(mbt, surrogate)]
p_la_1000_standalone_lm_sta = ggplot(stability_la[surrogate %in% c("standalone", "lm")],
                                     aes(x = factor(mbt_surrogate, levels = c("SLIM standalone", "SLIM lm", 
                                                                              "GUIDE standalone", "GUIDE lm",  
                                                                              "MOB standalone", "MOB lm",
                                                                              "CTree standalone", "CTree lm")), y = ari)) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(fun="mean", color = "grey")+
  ggtitle("Stability Linear Abrupt", subtitle = "n = 1000, alpha = 0.001, impr = 0.05") +
  labs(x="MBT as standalone or as surrogate on xgboost predictions", y="ARI")
ggexport(p_la_1000_standalone_lm_sta, filename = paste0(save_dir, "la_1000_standalone_lm_sta.pdf"), width = 10, height = 4)


stability_la[, mbt_surrogate := paste(mbt, surrogate)]
p_la_1000_standalone_xgboost_sta = ggplot(stability_la[surrogate %in% c("standalone", "xgboost")],
                                     aes(x = factor(mbt_surrogate, levels = c("SLIM standalone", "SLIM xgboost", 
                                                                              "GUIDE standalone", "GUIDE xgboost",  
                                                                              "MOB standalone", "MOB xgboost",
                                                                              "CTree standalone", "CTree xgboost")), y = ari)) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(fun="mean", color = "grey")+
  ggtitle("Stability Linear Abrupt", subtitle = "n = 1000, alpha = 0.001, impr = 0.05") +
  labs(x="MBT as standalone or as surrogate on xgboost predictions", y="ARI")
ggexport(p_la_1000_standalone_xgboost_sta, filename = paste0(save_dir, "la_1000_standalone_xgboost_sta.pdf"), width = 10, height = 4)


# ---- Performance ----
performance_la = rbind(unique(res_la_n1500_alpha001[mbt %in% c("MOB", "CTree"),.(r2_train, r2_test, mbt, job.id, config_id, surrogate)]),
                       unique(res_la_n1500_alpha05[mbt %in% c("SLIM", "GUIDE"),.(r2_train, r2_test, mbt, job.id, config_id, surrogate)]),
                       unique(res_la_n1500_alpha05[mbt %in% c("lm", "xgboost") ,.(r2_train, r2_test, mbt, job.id, config_id, surrogate)]), use.names = FALSE)

# Standalone & as surrogate on lm

performance_la[, mbt_surrogate := paste(mbt, surrogate)]

performance_la[, mbt_surrogate := str_replace_all(mbt_surrogate, "lm lm", "lm")]
performance_la[, mbt_surrogate := str_replace_all(mbt_surrogate, "xgboost xgboost", "xgboost")]
unique(performance_la[surrogate %in% c("standalone", "lm"), mbt_surrogate  ])

p_la_1000_standalone_lm_r2_train = ggplot(performance_la[surrogate %in% c("standalone", "lm")  ],
                                          aes(x = factor(mbt_surrogate, levels = c("SLIM standalone", "SLIM lm", 
                                                                                   "GUIDE standalone", "GUIDE lm",  
                                                                                   "MOB standalone", "MOB lm",
                                                                                   "CTree standalone", "CTree lm",
                                                                                   "lm")), y = r2_train)) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(fun="mean", color = "grey")+
  ggtitle("Accuracy and Fidelity Linear Abrupt Train", subtitle = "n = 1000, alpha = 0.001, impr = 0.05") +
  labs(x="MBT as standalone or as surrogate on lm predictions", y="R2")
ggexport(p_la_1000_standalone_lm_r2_train, filename = paste0(save_dir, "la_1000_standalone_lm_r2_train.pdf"), width = 10, height = 4)

p_la_1000_standalone_xgboost_r2_train = ggplot(performance_la[surrogate %in% c("standalone", "xgboost") ],
                                          aes(x = factor(mbt_surrogate, levels = c("SLIM standalone", "SLIM xgboost", 
                                                                                   "GUIDE standalone", "GUIDE xgboost",  
                                                                                   "MOB standalone", "MOB xgboost",
                                                                                   "CTree standalone", "CTree xgboost",
                                                                                   "xgboost")), y = r2_train)) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(fun="mean", color = "grey")+
  ggtitle("Accuracy Linear Abrupt Train", subtitle = "n = 1000, alpha = 0.001, impr = 0.05") +
  labs(x="MBT as standalone or as surrogate on xgboost predictions", y="R2")
ggexport(p_la_1000_standalone_xgboost_r2_train, filename = paste0(save_dir, "la_1000_standalone_xgboost_r2_train.pdf"), width = 10, height = 4)




# ----- 3. Linear Mixed -----
result_basic = readRDS("Data/simulations/batchtools/basic_scenarios/results/result_summary.rds")

View(result_basic$mean[n == 1500 & type == "linear_mixed" & surrogate == "lm" ,
                       .(mbt, impr, n_leaves, r2_train, r2_test, ari)])


result_basic$mean[n == 1500 & type == "linear_mixed" & surrogate == "standalone" & mbt %in% c("CTree"),
                  .(alpha,n_leaves, r2_train, r2_test, ari)] %>%
  kbl(caption="Mean simulation results on 100 simulation runs for CTree as stand alone model on scenario Linear Mixed with n = 1000 for different values of impr ",
      format="latex",
      col.names = c("alpha", "number of leaf nodes","R2 train","R2 test","ARI"),
      align="r",
      digits = 4) %>%
  kable_minimal(full_width = F)



res_lm_n1500_alpha001 = readRDS("Data/simulations/batchtools/basic_scenarios/results/19_res_experiments.rds")

res_lm_n1500_alpha05 = readRDS("Data/simulations/batchtools/basic_scenarios/results/21_res_experiments.rds")


save_dir = "Figures/simulations/batchtools/basic_scenarios/linear_mixed/"
if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

# --- Interpretability ----
interpretabiliy_lm = rbind(unique(res_lm_n1500_alpha001[mbt %in% c("MOB", "CTree"),.(n_leaves, mbt, job.id, config_id, surrogate)]),
                           unique(res_lm_n1500_alpha05[mbt %in% c("SLIM", "GUIDE"),.(n_leaves, mbt, job.id, config_id, surrogate)]), use.names = FALSE)

# Standalone
p_lm_1000_standalone_int = ggplot(interpretabiliy_lm[surrogate == "standalone",],
                                  aes(x = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")), y = n_leaves)) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(fun="mean", color = "grey") +
  ggtitle("Interpretability of stand alone MBTs Linear Mixed", subtitle = "n = 1000, alpha = 0.001, impr = 0.05") +
  labs(x="MBT", y="Number of terminal leaves")

ggexport(p_lm_1000_standalone_int, filename = paste0(save_dir, "lm_1000_standalone_int.pdf"), width = 7, height = 4)

# Standalone & as surrogate on lm
interpretabiliy_lm[, mbt_surrogate := paste(mbt, surrogate)]
p_lm_1000_standalone_lm_int = ggplot(interpretabiliy_lm[surrogate %in% c("standalone", "lm"),],
                                     aes(x = factor(mbt_surrogate, levels = 
                                                      c("SLIM standalone", "SLIM lm", 
                                                        "GUIDE standalone", "GUIDE lm",  
                                                        "MOB standalone", "MOB lm",
                                                        "CTree standalone", "CTree lm")), y = n_leaves)) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(fun="mean", color = "grey") +
  ggtitle("Interpretability of stand alone MBTs Linear Mixed", subtitle = "n = 1000, alpha = 0.001, impr = 0.05") +
  labs(x="MBT as standalone or as surrogate on lm predictions", y="Number of terminal leaves")
ggexport(p_lm_1000_standalone_lm_int, filename = paste0(save_dir, "lm_1000_standalone_lm_int.pdf"), width = 10, height = 4)



# ---- Stability -----
stability_lm = rbind(res_lm_n1500_alpha001[mbt %in% c("MOB", "CTree") ,.(ari, mbt, surrogate)],
                     res_lm_n1500_alpha05[mbt %in% c("SLIM", "GUIDE") ,.(ari, mbt, surrogate)], use.names = FALSE)


# Standalone
p_lm_1000_standalone_sta = ggplot(stability_lm[surrogate == "standalone"],
                                  aes(x = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")), y = ari)) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(fun="mean", color = "grey")+
  ggtitle("Stability of stand alone MBTs Linear Mixed", subtitle = "n = 1000, alpha = 0.001, impr = 0.05") +
  labs(x="MBT", y="ARI")

ggexport(p_lm_1000_standalone_sta, filename = paste0(save_dir, "lm_1000_standalone_sta.pdf"), width = 7, height = 4)


# Standalone & as surrogate on lm

stability_lm[, mbt_surrogate := paste(mbt, surrogate)]
p_lm_1000_standalone_lm_sta = ggplot(stability_lm[surrogate %in% c("standalone", "lm")],
                                     aes(x = factor(mbt_surrogate, levels = c("SLIM standalone", "SLIM lm", 
                                                                              "GUIDE standalone", "GUIDE lm",  
                                                                              "MOB standalone", "MOB lm",
                                                                              "CTree standalone", "CTree lm")), y = ari)) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(fun="mean", color = "grey")+
  ggtitle("Stability Linear Mixed", subtitle = "n = 1000, alpha = 0.001, impr = 0.05") +
  labs(x="MBT as standalone or as surrogate on lm predictions", y="ARI")
ggexport(p_lm_1000_standalone_lm_sta, filename = paste0(save_dir, "lm_1000_standalone_lm_sta.pdf"), width = 10, height = 4)


# ---- Performance ----
performance_lm = rbind(unique(res_lm_n1500_alpha001[mbt %in% c("MOB", "CTree"),.(r2_train, r2_test, mbt, job.id, config_id, surrogate)]),
                       unique(res_lm_n1500_alpha05[mbt %in% c("SLIM", "GUIDE"),.(r2_train, r2_test, mbt, job.id, config_id, surrogate)]),
                       unique(res_lm_n1500_alpha05[mbt %in% c("lm", "xgboost") ,.(r2_train, r2_test, mbt, job.id, config_id, surrogate)]), use.names = FALSE)

# Standalone
p_lm_1000_standalone_r2_train = ggplot(performance_lm[surrogate == "standalone"|mbt %in% c("lm", "xgboost")],
                                       aes(x = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree", "lm", "xgboost")), y = r2_train)) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(fun="mean", color = "grey")+
  ggtitle("Accuracy of stand alone MBTs Linear Mixed Train", subtitle = "n = 1000, alpha = 0.001, impr = 0.05") +
  labs(x="MBT", y="R2")

ggexport(p_lm_1000_standalone_r2_train, filename = paste0(save_dir, "lm_1000_standalone_r2_train.pdf"), width = 7, height = 4)


# Standalone & as surrogate on lm

performance_lm[, mbt_surrogate := paste(mbt, surrogate)]

p_lm_1000_standalone_lm_r2_train = ggplot(performance_lm[surrogate %in% c("standalone", "lm") & !(mbt %in% c("lm", "xgboost"))],
                                          aes(x = factor(mbt_surrogate, levels = c("SLIM standalone", "SLIM lm", 
                                                                                   "GUIDE standalone", "GUIDE lm",  
                                                                                   "MOB standalone", "MOB lm",
                                                                                   "CTree standalone", "CTree lm")), y = r2_train)) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(fun="mean", color = "grey")+
  ggtitle("Accuracy and Fidelity Linear Mixed Train", subtitle = "n = 1000, alpha = 0.001, impr = 0.05") +
  labs(x="MBT as standalone or as surrogate on lm predictions", y="R2")
ggexport(p_lm_1000_standalone_lm_r2_train, filename = paste0(save_dir, "lm_1000_standalone_lm_r2_train.pdf"), width = 10, height = 4)


p_lm_1000_standalone_lm_r2_test = ggplot(performance_lm[surrogate %in% c("standalone", "lm") & !(mbt %in% c("lm", "xgboost"))],
                                          aes(x = factor(mbt_surrogate, levels = c("SLIM standalone", "SLIM lm", 
                                                                                   "GUIDE standalone", "GUIDE lm",  
                                                                                   "MOB standalone", "MOB lm",
                                                                                   "CTree standalone", "CTree lm")), y = r2_test)) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(fun="mean", color = "grey")+
  ggtitle("Accuracy and Fidelity Linear Mixed Test", subtitle = "n = 1000, alpha = 0.001, impr = 0.05") +
  labs(x="MBT as standalone or as surrogate on lm predictions", y="R2")
ggexport(p_lm_1000_standalone_lm_r2_test, filename = paste0(save_dir, "lm_1000_standalone_lm_r2_test.pdf"), width = 10, height = 4)

