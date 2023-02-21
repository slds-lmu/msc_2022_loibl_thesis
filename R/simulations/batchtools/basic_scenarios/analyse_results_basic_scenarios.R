library(dplyr)
library(REdaS)
library(kableExtra)
library(batchtools)
library(data.table)
library(GGally)




colors_mbt =c("SLIM" = 'purple', "GUIDE" = 'olivedrab3', "MOB" ='skyblue', "CTree" = 'salmon')
colors_surrogate = c("standalone" = "white", "lm" = "lightgrey", xgboost = "cornsilk")



ades_basic = data.frame(alpha = c(0.001, 0.01, 0.05), impr.par = c(0.15, 0.1, 0.05))
pdes_basic = expand.grid(n = c(1500, 7500), type = c("linear_smooth", "linear_abrupt", "linear_mixed"))
experiments = merge(ades_basic, pdes_basic, by = NULL)


# ----- 1. Linear Smooth -----
# stand-alone

result_basic = readRDS("Data/simulations/batchtools/basic_scenarios/results/result_summary.rds")
result_basic_mean = result_basic$mean
result_basic_sd = result_basic$sd
setnames(result_basic_sd, c("r2_train", "r2_test"), c("r2_train_sd", "r2_test_sd"))
result_basic_mean = cbind(result_basic_mean, result_basic_sd[,.(r2_train_sd, r2_test_sd)])
result_basic_mean[type == "linear_mixed" & mbt == "SLIM" & n == 1500]

list.files("Data/simulations/batchtools/basic_scenarios/results/")



View(result_basic$mean[n == 1500 & type == "linear_smooth" & surrogate == "standalone",
                  .(mbt, alpha , impr, n_leaves, r2_train, r2_test, ri)])


result_basic_mean[n == 1500 & type == "linear_smooth" &  ((surrogate == "standalone" & mbt %in% c("SLIM", "GUIDE")) |
                                                            (mbt == "lm" & surrogate =="lm") |
                                                            (mbt == "xgboost" & surrogate =="xgboost")),
.(mbt, impr, n_leaves, n_leaves_min, n_leaves_max, r2_train, r2_train_sd, r2_test, r2_test_sd)] %>% 
  arrange(.,desc(mbt))%>%
  kbl(caption="Mean simulation results on 100 simulation runs for SLIM and GUIDE as stand alone model on scenario Linear smooth with n = 1000 for different values of impr ",
      format="latex",
      col.names = c("MBT", "alpha","mean n leaves", "n leaves min", "n leaves max", "mean R2 train", "sd R2 train", "mean R2 test", "sd R2 test"),
      align="r",
      digits = 4) %>%
  kable_minimal(full_width = F)

# --- overview ----

save_dir = "Figures/simulations/batchtools/basic_scenarios/linear_smooth/"
if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

res_ls_n1500_alpha001 = readRDS("Data/simulations/batchtools/basic_scenarios/results/1_res_experiments.rds")

res_ls_n1500_alpha01 = readRDS("Data/simulations/batchtools/basic_scenarios/results/2_res_experiments.rds")

# standalone
overview_ls = rbind(unique(res_ls_n1500_alpha01[mbt %in% c("SLIM", "GUIDE"),
                                                .(n_leaves, r2_train, r2_test, mse_train, mse_test, mbt, job.id, config_id, ri, stability_same_size, surrogate)]),
                    unique(res_ls_n1500_alpha001[mbt %in% c("MOB", "CTree") ,
                                                 .(n_leaves, r2_train, r2_test, mse_train, mse_test, mbt, job.id, config_id, ri, stability_same_size, surrogate)]))





p_ls_1000_standalone_overview =  ggpairs(overview_ls[!is.na(ri) & surrogate == "standalone" ,.(n_leaves, ri, r2_train, mbt)],
                                         columns = 1:3,        # Columns
                                         aes(color = mbt,  # Color by group (cat. variable)
                                             alpha = 0.1)) 
ggexport(p_ls_1000_standalone_overview, filename = paste0(save_dir, "ls_1000_standalone_overview.pdf"), width = 7, height = 4)


# lm



p_ls_1000_lm_overview =  ggpairs(overview_ls[!is.na(ri)  & surrogate == "lm" ,.(n_leaves, ri, r2_train, mbt)],
                                         columns = 1:3,        # Columns
                                         aes(color = mbt,  # Color by group (cat. variable)
                                             alpha = 0.1)) 
ggexport(p_ls_1000_lm_overview, filename = paste0(save_dir, "ls_1000_lm_overview.pdf"), width = 7, height = 6)



# --- Interpretability ----
interpretabiliy_ls = rbind(unique(res_ls_n1500_alpha001[mbt %in% c("MOB", "CTree"),.(n_leaves, mbt, job.id, config_id, surrogate)]),
                           unique(res_ls_n1500_alpha01[mbt %in% c("SLIM", "GUIDE"),.(n_leaves, mbt, job.id, config_id, surrogate)]), use.names = FALSE)

# Standalone
p_ls_1000_standalone_int = ggplot(interpretabiliy_ls[surrogate == "standalone",],
       aes(x = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")), y = n_leaves,
           color = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")))) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(fun="mean", color = "grey") +
  scale_color_manual(values = colors_mbt) +
  labs(x="MBT", y="Number of leafnodes") +
  theme(legend.position = "none")

ggexport(p_ls_1000_standalone_int, filename = paste0(save_dir, "ls_1000_standalone_lm_int.pdf"), width = 7, height = 4)

# Standalone & as surrogate on lm
p_ls_1000_int = ggplot(interpretabiliy_ls[surrogate %in% c("standalone", "lm", "xgboost"),],
                                  aes(x = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")), y = n_leaves,
                                      fill = factor(surrogate, levels = c("standalone", "lm", "xgboost")), 
                                      color = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")))) +
  stat_boxplot(geom = "boxplot") +
  scale_color_manual(values = colors_mbt, guide = "none") +
  scale_fill_manual(values = colors_surrogate)+
  labs(x="MBT", y="Number of leafnodes",
       fill = "surrogate")
ggexport(p_ls_1000_int, filename = paste0(save_dir, "ls_1000_int.pdf"), width = 10, height = 4)



# ---- Stability -----
stability_ls = rbind(res_ls_n1500_alpha001[mbt %in% c("MOB", "CTree") ,.(ri, mbt, surrogate, stability_same_size, n_leaves)],
                     res_ls_n1500_alpha01[mbt %in% c("SLIM", "GUIDE") ,.(ri, mbt, surrogate, stability_same_size, n_leaves)], use.names = FALSE)



# Standalone
p_ls_1000_standalone_sta = ggplot(stability_ls[surrogate == "standalone" & stability_same_size == TRUE & n_leaves > 7 & n_leaves < 12],
                                  aes(x = as.factor(n_leaves), 
                                      y = ri,
                                      fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                                      )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.6,
                        label = length(y)/2))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  labs(x="number of leafnodes", y="RI", fill = "MBT")

ggexport(p_ls_1000_standalone_sta, filename = paste0(save_dir, "ls_1000_standalone_sta.pdf"), width = 10, height = 4)


# LM
p_ls_1000_lm_sta = ggplot(stability_ls[surrogate == "lm" & stability_same_size == TRUE 
                                       & n_leaves > 10 & n_leaves < 17
                                       ],
                                  aes(x = as.factor(n_leaves), 
                                      y = ri, 
                                      fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")))) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(
    fun.data =  function(y){
      return(data.frame(y = 0.8,
                        label = length(y)/2))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    fun = 0.75,
    position = position_dodge(width = 0.75)
  ) +
  scale_fill_manual(values = colors_mbt) +
  labs(x="number of leafnodes", y="RI", fill = "MBT")

ggexport(p_ls_1000_standalone_sta, filename = paste0(save_dir, "ls_1000_standalone_sta.pdf"), width = 7, height = 4)



# Standalone & as surrogate on lm

p_ls_1000_lm_sta = ggplot(stability_ls[surrogate %in% c("standalone", "lm")& stability_same_size == TRUE & 
                                         n_leaves >= 10 & n_leaves <=14],
                                  aes(x = as.factor(n_leaves), y = ri, 
                                      color = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")),
                                      fill = factor(surrogate, levels = c("standalone", "lm")))) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.755,
                        label = length(y)/2))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75))+
  scale_color_manual(values = colors_mbt) +
  scale_fill_manual(values = colors_surrogate)+
  labs(x="MBT", y="RI", fill = "surrogate", color = "MBT")
ggexport(p_ls_1000_lm_sta, filename = paste0(save_dir, "ls_1000_lm_sta.pdf"), width = 15, height = 4)


# Standalone & as surrogate on xgboost

p_ls_1000_xgboost_sta = ggplot(stability_ls[surrogate %in% c("standalone", "xgboost")& stability_same_size == TRUE & 
                                         n_leaves >= 10 & n_leaves <=14],
                          aes(x = as.factor(n_leaves), y = ri, 
                              color = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")),
                              fill = factor(surrogate, levels = c("standalone", "xgboost")))) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.755,
                        label = length(y)/2))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75))+
  scale_color_manual(values = colors_mbt) +
  scale_fill_manual(values = c("standalone" = "white", "xgboost" = "lightgrey"))+
  labs(x="MBT", y="RI", fill = "surrogate", color = "MBT")
ggexport(p_ls_1000_xgboost_sta, filename = paste0(save_dir, "ls_1000_standalone_xgboost_sta.pdf"), width = 10, height = 4)




# ---- Performance ----
p_ls_1000_standalone = ggpairs(unique(overview_ls[ surrogate == "standalone",.(n_leaves, r2_train, r2_test, mbt, job.id, config_id)]),
                               columns = 1:3,        # Columns
                               aes(color = mbt,  # Color by group (cat. variable)
                                   alpha = 0.9),
                               columnLabels = c("n leaves", "R2 train", "R2 test"))
ggexport(p_ls_1000_standalone, filename = paste0(save_dir, "ls_1000_standalone_r2_nleaves.pdf"), width = 7, height = 4)




performance_ls = rbind(unique(res_ls_n1500_alpha001[mbt %in% c("MOB", "CTree"),.(r2_train, r2_test, mbt, job.id, config_id, surrogate, n_leaves)]),
                       unique(res_ls_n1500_alpha01[mbt %in% c("SLIM", "GUIDE"),.(r2_train, r2_test, mbt, job.id, config_id, surrogate, n_leaves)]),
                       unique(res_ls_n1500_alpha01[mbt %in% c("lm", "xgboost") ,.(r2_train, r2_test, mbt, job.id, config_id, surrogate, n_leaves)]), use.names = FALSE)

# Standalone
p_ls_1000_standalone_r2_train = ggplot(performance_ls[surrogate == "standalone" & n_leaves %in% 8:11],
                                  aes(x = as.factor(n_leaves), 
                                      y = r2_train,
                                      fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree", "lm", "xgboost"))
                                  )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = c(colors_mbt, colors_surrogate)) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.97,
                        label = length(y)))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  labs(x="number of leafnodes", y="R2", fill = "MBT")

ggexport(p_ls_1000_standalone_r2_train, filename = paste0(save_dir, "ls_1000_standalone_r2_train.pdf"), width = 10, height = 4)


p_ls_1000_standalone_r2_test = ggplot(performance_ls[surrogate == "standalone" & n_leaves > 7 & n_leaves < 12],
                                       aes(x = as.factor(n_leaves), 
                                           y = r2_test,
                                           fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                                       )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.965,
                        label = length(y)))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  labs(x="number of leafnodes", y="R2", fill = "MBT")

ggexport(p_ls_1000_standalone_r2_test, filename = paste0(save_dir, "ls_1000_standalone_r2_test.pdf"), width = 10, height = 4)



# LM as underlying blackbox model
p_ls_1000_lm_r2_train = ggplot(performance_ls[surrogate == "lm" & n_leaves > 12 & n_leaves < 17],
                                       aes(x = as.factor(n_leaves), 
                                           y = r2_train,
                                           fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                                       )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.997,
                        label = length(y)))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  labs(x="number of leafnodes", y="R2", fill = "MBT")

ggexport(p_ls_1000_lm_r2_train, filename = paste0(save_dir, "ls_1000_lm_r2_train.pdf"), width = 10, height = 4)

p_ls_1000_lm_r2_test = ggplot(performance_ls[surrogate == "lm" & n_leaves > 13 & n_leaves < 17],
                               aes(x = as.factor(n_leaves), 
                                   y = r2_test,
                                   fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                               )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.9985,
                        label = length(y)))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  labs(x="number of leaf nodes", y="R2", fill = "MBT")

ggexport(p_ls_1000_lm_r2_test, filename = paste0(save_dir, "ls_1000_lm_r2_test.pdf"), width = 10, height = 4)



# ------ 2. Linear abrupt ------



result_basic_mean[n == 1500 & type == "linear_abrupt" &  ((surrogate == "standalone" & mbt %in% c("SLIM", "GUIDE")) |
                                                            (mbt == "lm" & surrogate =="lm") |
                                                            (mbt == "xgboost" & surrogate =="xgboost")),
                  .(mbt, impr, n_leaves, n_leaves_min, n_leaves_max, r2_train, r2_train_sd, r2_test, r2_test_sd)] %>% 
  arrange(.,desc(mbt))%>%
  kbl(caption="Mean simulation results on 100 simulation runs for MOB and CTree as stand alone model on scenario Linear smooth with n = 1000 for different values of impr ",
      format="latex",
      col.names = c("MBT", "impr","mean n leaves", "n leaves min", "n leaves max", "mean R2 train", "sd R2 train", "mean R2 test", "sd R2 test"),
      align="r",
      digits = 4) %>%
  kable_minimal(full_width = F)

# --- overview ----
save_dir = "Figures/simulations/batchtools/basic_scenarios/linear_abrupt/"
if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)


# alpha = 0.001 for mob and ctree
res_la_n1500_alpha001 = readRDS("Data/simulations/batchtools/basic_scenarios/results/7_res_experiments.rds")

# alpha = 0.05 i.e. impr = 0.05 for slim and guide
res_la_n1500_alpha05 = readRDS("Data/simulations/batchtools/basic_scenarios/results/9_res_experiments.rds")

# standalone
overview_la = rbind(unique(res_la_n1500_alpha05[mbt %in% c("SLIM", "GUIDE") ,
                                                .(n_leaves, r2_train, r2_test, mse_train, mse_test, mbt, job.id, config_id, ri, stability_same_size, surrogate)]),
                    unique(res_la_n1500_alpha001[mbt %in% c("MOB", "CTree") ,
                                                .(n_leaves, r2_train, r2_test, mse_train, mse_test, mbt, job.id, config_id, ri, stability_same_size, surrogate)]))



p_la_1000_standalone_overview =  ggpairs(overview_la[!is.na(ri) & stability_same_size == TRUE & surrogate == "standalone" ,.(n_leaves, ri, r2_train, mbt)],
                                         columns = 1:3,        # Columns
                                         aes(color = mbt,  # Color by group (cat. variable)
                                             alpha = 0.1)) 
ggexport(p_la_1000_standalone_overview, filename = paste0(save_dir, "la_1000_standalone_overview.pdf"), width = 7, height = 4)



# --- Interpretability ----
interpretabiliy_la = rbind(unique(res_la_n1500_alpha001[mbt %in% c("MOB", "CTree"),.(n_leaves, mbt, job.id, config_id, surrogate)]),
                           unique(res_la_n1500_alpha05[mbt %in% c("SLIM", "GUIDE"),.(n_leaves, mbt, job.id, config_id, surrogate)]), use.names = FALSE)


# Standalone & as surrogate on lm and xgboost
p_la_1000_int = ggplot(interpretabiliy_la[surrogate %in% c("standalone", "lm", "xgboost"),],
                                     aes(x = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")), 
                                         y = n_leaves, 
                                         color = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")),
                                         fill = factor(surrogate, levels = c("standalone", "lm", "xgboost")))) +
  stat_boxplot(geom = "boxplot") +
  scale_color_manual(values = colors_mbt, guide = "none") +
  scale_fill_manual(values = colors_surrogate)+
  labs(x="MBT", y="Number of leafnodes", fill = "surrogate")
ggexport(p_la_1000_int, filename = paste0(save_dir, "la_1000_int.pdf"), width = 10, height = 4)



# ---- Stability -----
stability_la = rbind(res_la_n1500_alpha001[mbt %in% c("MOB", "CTree") ,.(ri, mbt, surrogate, stability_same_size, n_leaves)],
                     res_la_n1500_alpha05[mbt %in% c("SLIM", "GUIDE") ,.(ri, mbt, surrogate, stability_same_size, n_leaves)], use.names = FALSE)



# Standalone
p_la_1000_standalone_sta = ggplot(stability_la[surrogate == "standalone" & stability_same_size == TRUE 
                                               & n_leaves > 3 & n_leaves < 15],
                                  aes(x = as.factor(n_leaves), 
                                      y = ri,
                                      fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                                  )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.88,
                        label = length(y)/2))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  labs(x="number of leafnodes", y="RI", fill = "MBT")

ggexport(p_la_1000_standalone_sta, filename = paste0(save_dir, "la_1000_standalone_sta.pdf"), width = 10, height = 4)



# Standalone & as surrogate on lm

p_la_1000_standalone_lm_sta = ggplot(stability_la[surrogate %in% c("standalone", "lm")& stability_same_size == TRUE 
                                                  & n_leaves > 3 & n_leaves < 15],
                                     aes(x = as.factor(n_leaves), y = ri, 
                                         color = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")),
                                         fill = factor(surrogate, levels = c("standalone", "lm")))) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.8,
                        label = length(y)/2))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75))+
  scale_color_manual(values = colors_mbt) +
  scale_fill_manual(values = colors_surrogate)+
  labs(x="MBT", y="RI", fill = "surrogate", color = "MBT")
ggexport(p_la_1000_standalone_lm_sta, filename = paste0(save_dir, "la_1000_standalone_lm_sta.pdf"), width = 20, height = 4)



# ---- Performance ----
performance_la = rbind(unique(res_la_n1500_alpha001[mbt %in% c("MOB", "CTree"),.(r2_train, r2_test, mbt, job.id, config_id, surrogate, n_leaves)]),
                       unique(res_la_n1500_alpha05[mbt %in% c("SLIM", "GUIDE"),.(r2_train, r2_test, mbt, job.id, config_id, surrogate, n_leaves)]),
                       unique(res_la_n1500_alpha05[mbt %in% c("lm", "xgboost") ,.(r2_train, r2_test, mbt, job.id, config_id, surrogate, n_leaves)]), use.names = FALSE)

p_la_1000_standalone = ggpairs(unique(overview_la[ surrogate == "standalone",.(n_leaves, r2_train, r2_test, mbt, job.id, config_id)]),
                               columns = 1:3,        # Columns
                               aes(color = mbt,  # Color by group (cat. variable)
                                   alpha = 0.9))
ggexport(p_la_1000_standalone, filename = paste0(save_dir, "la_1000_standalone_r2_nleaves.pdf"), width = 7, height = 4)


# Standalone
p_la_1000_standalone_r2_train = ggplot(performance_la[surrogate == "standalone"],
                                       aes(x = as.factor(n_leaves), 
                                           y = r2_train,
                                           fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                                       )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = c(colors_mbt)) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.935,
                        label = length(y)))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  labs(x="number of leafnodes", y="R2", fill = "MBT")

ggexport(p_la_1000_standalone_r2_train, filename = paste0(save_dir, "la_1000_standalone_r2_train.pdf"), width = 10, height = 4)

p_la_1000_standalone_r2_test = ggplot(performance_la[surrogate == "standalone"],
                                       aes(x = as.factor(n_leaves), 
                                           y = r2_test,
                                           fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree", "lm", "xgboost"))
                                       )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = c(colors_mbt, colors_surrogate)) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.92,
                        label = length(y)))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  labs(x="number of leafnodes", y="R2", fill = "Model")

ggexport(p_la_1000_standalone_r2_test, filename = paste0(save_dir, "la_1000_standalone_r2_test.pdf"), width = 10, height = 4)


# LM as underlying blackbox model
p_la_1000_lm_r2_train = ggplot(performance_la[surrogate == "lm" & mbt %in% c("SLIM", "GUIDE", "MOB", "CTree")],
                               aes(x = as.factor(n_leaves), 
                                   y = r2_train,
                                   fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                               )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.97,
                        label = length(y)))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  ggtitle("Fidelity of MBTs on lm predictions Linear Abrupt Train", subtitle = "n = 1000, alpha = 0.001, impr = 0.1") +
  labs(x="number of leafnodes", y="R2", fill = "MBT")

ggexport(p_la_1000_lm_r2_train, filename = paste0(save_dir, "la_1000_lm_r2_train.pdf"), width = 15, height = 4)

p_la_1000_lm_r2_test = ggplot(performance_la[surrogate == "lm" & mbt %in% c("SLIM", "GUIDE", "MOB", "CTree")],
                               aes(x = as.factor(n_leaves), 
                                   y = r2_test,
                                   fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                               )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.96,
                        label = length(y)))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  ggtitle("Fidelity of MBTs on lm predictions Linear Abrupt test", subtitle = "n = 1000, alpha = 0.001, impr = 0.1") +
  labs(x="number of leafnodes", y="R2", fill = "MBT")

ggexport(p_la_1000_lm_r2_test, filename = paste0(save_dir, "la_1000_lm_r2_test.pdf"), width = 15, height = 4)


# ----- 3. Linear Mixed -----

result_basic_mean[n == 1500 & type == "linear_mixed" & surrogate == "standalone" & mbt %in% c("MOB", "CTree"),
                  .(mbt, impr, n_leaves, n_leaves_min, n_leaves_max, r2_train, r2_train_sd, r2_test, r2_test_sd)] %>% 
  arrange(.,desc(mbt))%>%
  kbl(caption="Mean simulation results on 100 simulation runs for MOB and CTree as stand alone model on scenario Linear smooth with n = 1000 for different values of impr ",
      format="latex",
      col.names = c("MBT", "alpha","mean n leaves", "n leaves min", "n leaves max", "mean R2 train", "sd R2 train", "mean R2 test", "sd R2 test"),
      align="r",
      digits = 4) %>%
  kable_minimal(full_width = F)


# --- overview ---
save_dir = "Figures/simulations/batchtools/basic_scenarios/linear_mixed/"
if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

res_lm_n1500_alpha001 = readRDS("Data/simulations/batchtools/basic_scenarios/results/13_res_experiments.rds")

res_lm_n1500_alpha05 = readRDS("Data/simulations/batchtools/basic_scenarios/results/15_res_experiments.rds")


# --- Overview ----
overview_lm = rbind(unique(res_lm_n1500_alpha05[mbt %in% c("SLIM", "GUIDE") ,
                                                .(n_leaves, r2_train, r2_test, mse_train, mse_test, mbt, job.id, config_id, ri, stability_same_size, surrogate)]),
                    unique(res_lm_n1500_alpha001[mbt %in% c("MOB", "CTree") & surrogate == "standalone",
                                                 .(n_leaves, r2_train, r2_test, mse_train, mse_test, mbt, job.id, config_id, ri, stability_same_size, surrogate)]))



p_lm_1000_standalone_overview =  ggpairs(overview_lm[!is.na(ri) & stability_same_size == TRUE & surrogate == "standalone" ,.(n_leaves, ri, r2_train, mbt)],
                                         columns = 1:3,        # Columns
                                         aes(color = mbt,  # Color by group (cat. variable)
                                             alpha = 0.1)) 


ggexport(p_lm_1000_standalone_overview, filename = paste0(save_dir, "lm_1000_standalone_overview.pdf"), width = 7, height = 4)



# --- Interpretability ----
interpretabiliy_lm = rbind(unique(res_lm_n1500_alpha001[mbt %in% c("MOB", "CTree"),.(n_leaves, mbt, job.id, config_id, surrogate)]),
                           unique(res_lm_n1500_alpha05[mbt %in% c("SLIM", "GUIDE"),.(n_leaves, mbt, job.id, config_id, surrogate)]), use.names = FALSE)

# Standalone & as surrogate on lm and xgboost
p_lm_1000_int = ggplot(interpretabiliy_lm[surrogate %in% c("standalone", "lm", "xgboost"),],
                       aes(x = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")), 
                           y = n_leaves, 
                           color = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")),
                           fill = factor(surrogate, levels = c("standalone", "lm", "xgboost")))) +
  stat_boxplot(geom = "boxplot") +
  scale_color_manual(values = colors_mbt, guide = "none") +
  scale_fill_manual(values = colors_surrogate)+
  ggtitle("Interpretability of stand alone MBTs Linear Abrupt", subtitle = "n = 1000, alpha = 0.001, impr = 0.05") +
  labs(x="MBT as standalone or as surrogate on lm or xgboost predictions", y="Number of terminal leaves", fill = "surrogate")
ggexport(p_lm_1000_int, filename = paste0(save_dir, "lm_1000_int.pdf"), width = 10, height = 4)





# ---- Stability -----
stability_lm = rbind(res_lm_n1500_alpha001[mbt %in% c("MOB", "CTree") ,.(ri, mbt, surrogate, stability_same_size, n_leaves)],
                     res_lm_n1500_alpha05[mbt %in% c("SLIM", "GUIDE") ,.(ri, mbt, surrogate, stability_same_size, n_leaves)], use.names = FALSE)



# Standalone
p_lm_1000_standalone_sta = ggplot(stability_lm[surrogate == "standalone" & stability_same_size == TRUE 
                                               & n_leaves > 12 & n_leaves < 17],
                                  aes(x = as.factor(n_leaves), 
                                      y = ri,
                                      fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                                  )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.88,
                        label = length(y)/2))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  ggtitle("Stability of stand alone MBTs Linear Smooth", subtitle = "n = 1000, alpha = 0.001, impr = 0.1") +
  labs(x="number of leaf nodes", y="RI", fill = "MBT")

ggexport(p_lm_1000_standalone_sta, filename = paste0(save_dir, "lm_1000_standalone_sta.pdf"), width = 7, height = 4)






# Standalone & as surrogate on lm

p_lm_1000_standalone_lm_sta = ggplot(stability_lm[surrogate %in% c("standalone", "lm")& stability_same_size == TRUE 
                                                    & n_leaves > 12 & n_leaves < 17],
                          aes(x = as.factor(n_leaves), y = ri, 
                              color = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree")),
                              fill = factor(surrogate, levels = c("standalone", "lm")))) +
  stat_boxplot(geom = "boxplot") +
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.88,
                        label = length(y)/2))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75))+
  scale_color_manual(values = colors_mbt) +
  scale_fill_manual(values = colors_surrogate)+
  ggtitle("Stability Linear Smooth", subtitle = "n = 1000, alpha = 0.001, impr = 0.1") +
  labs(x="MBT as standalone or as surrogate on lm predictions", y="RI", fill = "surrogate", color = "MBT")
ggexport(p_lm_1000_standalone_lm_sta, filename = paste0(save_dir, "lm_1000_standalone_lm_sta.pdf"), width = 15, height = 4)


# ---- Performance ----
performance_lm = rbind(unique(res_lm_n1500_alpha001[mbt %in% c("MOB", "CTree"),.(r2_train, r2_test, mbt, job.id, config_id, surrogate, n_leaves)]),
                       unique(res_lm_n1500_alpha05[mbt %in% c("SLIM", "GUIDE"),.(r2_train, r2_test, mbt, job.id, config_id, surrogate, n_leaves)]),
                       unique(res_lm_n1500_alpha05[mbt %in% c("lm", "xgboost") ,.(r2_train, r2_test, mbt, job.id, config_id, surrogate, n_leaves)]), use.names = FALSE)

p_lm_1000_standalone = ggpairs(unique(overview_lm[ surrogate == "standalone",.(n_leaves, r2_train, r2_test, mbt, job.id, config_id)]),
                               columns = 1:3,        # Columns
                               aes(color = mbt,  # Color by group (cat. variable)
                                   alpha = 0.9))
ggexport(p_lm_1000_standalone, filename = paste0(save_dir, "lm_1000_standalone_r2_nleaves.pdf"), width = 7, height = 4)

# Standalone
p_lm_1000_standalone_r2_train = ggplot(performance_lm[surrogate == "standalone"],
                                       aes(x = as.factor(n_leaves), 
                                           y = r2_train,
                                           fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                                       )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.972,
                        label = length(y)))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  ggtitle("Accuracy of stand alone MBTs Linear Mixed Train", subtitle = "n = 1000, alpha = 0.001, impr = 0.1") +
  labs(x="number of leaf nodes", y="R2", fill = "MBT")

ggexport(p_lm_1000_standalone_r2_train, filename = paste0(save_dir, "lm_1000_standalone_r2_train.pdf"), width = 10, height = 4)

p_lm_1000_standalone_r2_test = ggplot(performance_lm[surrogate == "standalone"],
                                      aes(x = as.factor(n_leaves), 
                                          y = r2_test,
                                          fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                                      )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.96,
                        label = length(y)))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  ggtitle("Accuracy of stand alone MBTs Linear Mixed test", subtitle = "n = 1000, alpha = 0.001, impr = 0.1") +
  labs(x="number of leaf nodes", y="R2", fill = "MBT")

ggexport(p_lm_1000_standalone_r2_test, filename = paste0(save_dir, "lm_1000_standalone_r2_test.pdf"), width = 10, height = 4)


# Standalone & as surrogate on lm

p_lm_1000_lm_r2_train = ggplot(performance_lm[surrogate == "lm" & mbt %in% c("SLIM", "GUIDE", "MOB", "CTree")],
                                       aes(x = as.factor(n_leaves), 
                                           y = r2_train,
                                           fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                                       )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.982,
                        label = length(y)))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  ggtitle("Accuracy of stand alone MBTs Linear Mixed Train", subtitle = "n = 1000, alpha = 0.001, impr = 0.1") +
  labs(x="number of leaf nodes", y="R2", fill = "MBT")

ggexport(p_lm_1000_lm_r2_train, filename = paste0(save_dir, "lm_1000_lm_r2_train.pdf"), width = 10, height = 4)

p_lm_1000_lm_r2_test = ggplot(performance_lm[surrogate == "lm"& mbt %in% c("SLIM", "GUIDE", "MOB", "CTree")],
                                      aes(x = as.factor(n_leaves), 
                                          y = r2_test,
                                          fill = factor(mbt, levels = c("SLIM", "GUIDE", "MOB", "CTree"))
                                      )) +
  stat_boxplot(geom = "boxplot") +
  scale_fill_manual(values = colors_mbt) +
  # stat_summary(fun="mean", color = "grey")+
  stat_summary(
    fun.data = function(y){
      return(data.frame(y = 0.976,
                        label = length(y)))}, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9,
    position = position_dodge(width = 0.75)
  ) +
  ggtitle("Accuracy of stand alone MBTs Linear Mixed test", subtitle = "n = 1000, alpha = 0.001, impr = 0.1") +
  labs(x="number of leaf nodes", y="R2", fill = "MBT")

ggexport(p_lm_1000_lm_r2_test, filename = paste0(save_dir, "lm_1000_lm_r2_test.pdf"), width = 10, height = 4)

