library(batchtools)
source("R/load_packages.R")

# --- 1. SETUP REGISTRY ---
if (!dir.exists("Data/simulations/batchtools/selection_bias_general")) dir.create("Data/simulations/batchtools/selection_bias_general", recursive = TRUE)

reg = makeExperimentRegistry(file.dir = "Data/simulations/batchtools/selection_bias_general/batchtools",
                             source = c("R/simulations/batchtools/simulation_setting_definition.R", "R/tree_splitting_slim.R",
                                        "R/mob_fitting_functions.R"),
                             seed = 1)

# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

source("R/simulations/batchtools/simulation_setting_definition.R")

# add problems and setting definitions
addProblem(name = "selection_bias", fun = create_sim_data, reg = reg)
pdes = expand.grid(n = 1000, type = rep(c("selection_bias_independence", 
                                          "selection_bias_independence_small",
                                          "selection_bias_full_interaction",
                                          "selection_bias_guide",
                                          "selection_bias_interaction_categorical_numeric",
                                          "selection_bias_interaction_numeric",
                                          "selection_bias_interaction_numeric_2"
                                          ), each = 1))
pdes = list("selection_bias" = pdes)


# add algorithm
source("R/simulations/batchtools/selection_bias/helper_simulation_selection_bias.R")
formals(get_sim_results_selection_bias)$n.quantiles = c(NA, 100, 50, 10)
formals(get_sim_results_selection_bias)$exclude.categoricals = c(TRUE, FALSE)
formals(get_sim_results_selection_bias)$correct.bias = c(TRUE, FALSE)

addAlgorithm(name = "selection_bias", fun = get_sim_results_selection_bias)



# add experiments
addExperiments(
  reg = reg, 
  prob.designs = pdes,
  algo.designs = NULL, 
  repls = 1000L)

summarizeExperiments()
summarizeExperiments(by = c("problem", "algorithm", "n", "type"))


# test jobs
id1 = head(findExperiments(algo.name = "selection_bias"), 1)
print(id1)

testJob(id = 5)



# submit jobs
submitJobs(reg = reg)

# reg = loadRegistry("Data/simulations/batchtools/selection_bias_general/batchtools")

# reduce jobs/ summarise results
reduce = function(res) res
results = unwrap(reduceResultsDataTable(fun = reduce, reg = reg))
head(results)

pars = unwrap(getJobPars(reg = reg))
tab = ijoin(pars, results)
head(tab)


library(ggplot2)
library(ggpubr)
library(stringr)

savedir = "Data/simulations/batchtools/selection_bias_general/results/"
figuredir = "Figures/simulations/batchtools/selection_bias_general/"

if (!dir.exists(savedir)) dir.create(savedir, recursive = TRUE)
saveRDS(tab, paste0(savedir,"selection_bias_general.rds"))

# if (!dir.exists(figuredir)) dir.create(figuredir, recursive = TRUE)
# 
# for (t in unique(tab$type)){
#   for (n_data in unique(tab[type == t , n])){
#     tab_t_n = tab[type == t & n == n_data, ]
#     result = list(slim = table(tab_t_n$split_slim),
#                   mob = table(tab_t_n$split_mob),
#                   ctree = table(tab_t_n$split_ctree),
#                   guide = table(tab_t_n$split_guide))
#     saveRDS(result, file = paste0(savedir, str_remove(t, "selection_bias_"), "_n", n_data, ".rds"))
# 
#     p = ggplot(stack(tab_t_n[,.(split_slim, split_mob, split_ctree, split_guide)]),
#                aes(x = values, color=ind, fill = ind)) +
#       stat_count(position = "dodge") +
#       ggtitle("Frequency of selection", subtitle = paste(str_replace_all(str_remove(t, "selection_bias_"), "_", " "), "n", n_data)) +
#       labs(x="selected variable", y="frequency", color = "surrogate", fill = "surrogate")
# 
#     ggexport(p, filename = paste0(figuredir, str_remove(t, "selection_bias_"), "_n", n_data, ".pdf"), width = 8, height = 3.8)
# 
#   }
# }

