library(batchtools)
source("R/load_packages.R")

# --- 1. SETUP REGISTRY ---
if (!dir.exists("Data/simulations/batchtools/selection_bias_guide")) dir.create("Data/simulations/batchtools/selection_bias_guide", recursive = TRUE)

reg = makeExperimentRegistry(file.dir = "Data/simulations/batchtools/selection_bias_guide/batchtools",
                                 source = c("R/simulations/batchtools/simulation_setting_definition.R", "R/tree_splitting_slim.R"),
                             seed = 1)

# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

source("R/simulations/batchtools/simulation_setting_definition.R")

# add problems and setting definitions
addProblem(name = "selection_bias", fun = create_sim_data, reg = reg)
pdes = expand.grid(n = c(1000,2000), type = rep(c("selection_bias_independence_small",
                                                   "selection_bias_full_interaction"), each = 1))
pdes = list("selection_bias" = pdes)


# add algorithm
source("R/simulations/batchtools/selection_bias/helper_simulation_selection_bias.R")
addAlgorithm(name = "selection_bias", fun = get_sim_results_selection_bias)



# add experiments
addExperiments(
  reg = reg, 
  prob.designs = pdes,
  algo.designs = list(selection_bias = data.frame(tree_methods = "guide")), 
  repls = 2000L)

summarizeExperiments()
summarizeExperiments(by = c("problem", "algorithm", "n", "type"))


# test jobs
id1 = head(findExperiments(algo.name = "selection_bias"), 1)
print(id1)

testJob(id = 20)



# submit jobs
submitJobs()


# reduce jobs/ summarise results
reduce = function(res) res
results = unwrap(reduceResultsDataTable(fun = reduce, reg = reg))
head(results)

pars = unwrap(getJobPars(reg = reg))
tab = ijoin(pars, results)
head(tab)

# ggplot(stack(tab[type == "selection_bias_independence_small" & n == 1000 & test_guide == "curvature",.(split_guide)]),
#            aes(x = values, color=ind, fill = ind)) +
#   stat_count(position = "dodge") +
#   ggtitle("Frequency of selection", subtitle = "independence small - curvature") +
#   labs(x="selected variable", y="frequency", color = "surrogate", fill = "surrogate") 
# 
# ggplot(stack(tab[type == "selection_bias_independence_small" & n == 1000 & test_guide == "interaction",.(split_guide)]),
#        aes(x = values, color=ind, fill = ind)) +
#   stat_count(position = "dodge") +
#   ggtitle("Frequency of selection", subtitle = "independence small - interaction") +
#   labs(x="selected variable", y="frequency", color = "surrogate", fill = "surrogate") 
# 
# 



library(ggplot2)
library(ggpubr)
library(stringr)

savedir = "Data/simulations/batchtools/selection_bias_guide/results/"

if (!dir.exists(savedir)) dir.create(savedir, recursive = TRUE)

for (t in unique(tab$type)){
  for (n in unique(tab[type == t , n])){
    tab_t_n = tab[type == t & n == n, ]
    result = list(guide = table(tab_t_n$split_guide),
                  guide_test = table(tab_t_n$test_guide))
    saveRDS(result, file = paste0(savedir, str_remove(t, "selection_bias_"), "_n", n, ".rds"))
    
  }
}

