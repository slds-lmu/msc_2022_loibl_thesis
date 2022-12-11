# load results

list.files("Data/simulations/batchtools/selection_bias_results/", full.names = TRUE)

independence_small = readRDS("Data/simulations/batchtools/selection_bias_results/independence_small_n1000.rds")

library(REdaS)

freqCI(independence_small$slim, level = c(.95))
freqCI(independence_small$guide, level = c(.95))


########
# guide selection bias

list.files("Data/simulations/batchtools/selection_bias_guide/results/", full.names = TRUE)
split_data = readRDS("Data/simulations/batchtools/selection_bias_guide/results/selection_bias_guide.rds")

figuredir = "Figures/simulations/batchtools/selection_bias_guide/"
if (!dir.exists(figuredir)) dir.create(figuredir, recursive = TRUE)



for(t in c("selection_bias_independence_small", "selection_bias_full_interaction")){
  for(n_data in c(1000, 2000)){
    for(test in c("curvature", "interaction")){
      p = ggplot(stack(split_data[type == t & n == n_data & test_guide == test,.(split_guide)]),
                 aes(x = values, color=ind, fill = ind)) +
        stat_count(position = "dodge") +
        ggtitle("Frequency of selection", subtitle = paste(str_replace_all(str_remove(t, "selection_bias_"), "_", " "), "n", n_data, test)) +
        labs(x="selected variable", y="frequency", color = "surrogate", fill = "surrogate")
      
      ggexport(p, filename = paste0(figuredir, str_remove(t, "selection_bias_"), "_n", n_data, "_", test,".pdf"), width = 8, height = 3.8)
      
    }
  }
}



#####################
# slim selection bias different values of n.quantiles
list.files("Data/simulations/batchtools/selection_bias_slim/results/", full.names = TRUE)
split_data_full_interaction = readRDS("Data/simulations/batchtools/selection_bias_slim/results/full_interaction_n1000.rds")
split_data_independence_small = readRDS("Data/simulations/batchtools/selection_bias_slim/results/independence_small_n1000.rds")
