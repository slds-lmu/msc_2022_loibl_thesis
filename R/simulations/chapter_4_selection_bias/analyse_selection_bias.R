library(dplyr)
library(REdaS)
library(kableExtra)
library(batchtools)
library(data.table)
library(GGally)
library(ggpubr)
library(stringr)
# load results

list.files("Data/simulations/chapter_4_selection_bias/selection_bias_general/results/", full.names = TRUE)
tab = readRDS("Data/simulations/chapter_4_selection_bias/selection_bias_general/results/selection_bias_general.rds")

savedir = "Data/simulations/chapter_4_selection_bias/selection_bias_general/results/"
figuredir = "Figures/simulations/chapter_4_selection_bias/selection_bias_general/"

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

ggexport(p_independence_numerical, filename = paste0(figuredir, "independence_numerical.png"), width = 550, height = 200)


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






# ---- 3. guide selection bias ----

list.files("Data/simulations/chapter_4_selection_bias/selection_bias_guide/results/", full.names = TRUE)
split_data = readRDS("Data/simulations/chapter_4_selection_bias/selection_bias_guide/results/selection_bias_guide.rds")
nrow(split_data)

# frequency of selected splitvariables for example guide (biased and bootstrap corrected)
split_guide = split_data[type == "selection_bias_guide", .(split_guide_excl_cat_biased, split_guide_excl_cat_corr)]
guide_table = sapply(split_guide, table)

guide_table%>%
  kbl(format="latex",
      align="r",
      digits = 1) %>%
  kable_minimal(full_width = F)





# ---- 4. Slim selection bias correction approach - different values of n.quantiles ----
selection_bias_slim = readRDS("Data/simulations/chapter_4_selection_bias/selection_bias_slim/results/selection_bias_slim.rds")
savedir_slim = "Data/simulations/chapter_4_selection_bias/selection_bias_slim/results/"

if (!dir.exists(savedir_slim)) dir.create(savedir_slim, recursive = TRUE)

type_independence = c("selection_bias_independence_10", "selection_bias_independence_25", 
                      "selection_bias_independence_50", "selection_bias_independence_100")


n_quantiles_results = list()
for(t in unique(selection_bias_slim$type)){
  frequency_table = sapply(selection_bias_slim[type == t, .(split_slim_exact,
                                                            split_slim_100,
                                                            split_slim_75,
                                                            split_slim_50,
                                                            split_slim_25,
                                                            split_slim_10,
                                                            split_slim_2)], table)

  
  mse_train = sapply(selection_bias_slim[type == t, .(mse_slim_exact,
                                                      mse_slim_100,
                                                      mse_slim_75,
                                                      mse_slim_50,
                                                      mse_slim_25,
                                                      mse_slim_10,
                                                      mse_slim_2)], mean)
  
  mse_test = sapply(selection_bias_slim[type == t, .(mse_test_slim_exact,
                                                     mse_test_slim_100,
                                                     mse_test_slim_75,
                                                     mse_test_slim_50,
                                                     mse_test_slim_25,
                                                     mse_test_slim_10,
                                                     mse_test_slim_2)], mean)
  n_quantiles_results[[t]] = rbind(frequency_table, mse_train, mse_test)
  # chi2 = apply(frequency_table, 2, function(table){
  #   chisq.test(table, p = prob)[["p.value"]]
  # }) 
}


# Summarise results of independence scenarios with different gridsize of x3 in one table
n_quantile_independence_scenarios = do.call(rbind, n_quantiles_results[type_independence])
n_quantile_independence_scenarios = cbind(gridsize = c(rep(10, 5),rep(25, 5),rep(50, 5),rep(100, 5)),
                                          rownames(n_quantile_independence_scenarios),
                                          n_quantile_independence_scenarios)

n_quantile_independence_scenarios %>%
  kbl(format="latex",
      align="r",
      digits = 4,
      row.names = FALSE) %>%
  kable_minimal(full_width = F)

# scenario independence small
cbind(rbind(n_quantiles_results$selection_bias_independence_small, n_quantiles_results$selection_bias_independence)) %>%
  kbl(format="latex",
      align="r",
      digits = 4,
      row.names = TRUE) %>%
  kable_minimal(full_width = F)

# scenario interaction_numerical_vs_numrical
n_quantiles_results$selection_bias_interaction_numerical_vs_numrical %>%
  kbl(format="latex",
      align="r",
      digits = 4,
      row.names = TRUE) %>%
  kable_minimal(full_width = F)
