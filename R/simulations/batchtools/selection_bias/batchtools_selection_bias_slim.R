library(batchtools)
source("R/load_packages.R")

# --- 1. SETUP REGISTRY ---
if (!dir.exists("Data/simulations/batchtools/selection_bias_slim")) dir.create("Data/simulations/batchtools/selection_bias_slim", recursive = TRUE)

reg = makeExperimentRegistry(file.dir = "Data/simulations/batchtools/selection_bias_slim/batchtools",
                             source = c("R/simulations/batchtools/simulation_setting_definition.R", "R/tree_splitting_slim.R"),
                             seed = 1)

# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

source("R/simulations/batchtools/simulation_setting_definition.R")

# add problems and setting definitions
addProblem(name = "selection_bias", fun = create_sim_data, reg = reg)
pdes = expand.grid(n = 1000, type = rep(c(
  # "selection_bias_independence_small",
  # "selection_bias_interaction",
  # "selection_bias_full_interaction",
  # "selection_bias_guide",
  "selection_bias_full_interaction_three",
  "selection_bias_interaction_binary_numeric",
  "selection_bias_interaction_categorical_numeric"), each = 1))
pdes = list("selection_bias" = pdes)


# add algorithm
source("R/simulations/batchtools/selection_bias/helper_simulation_selection_bias.R")
formals(get_sim_results_selection_bias)$tree_methods = "slim"
formals(get_sim_results_selection_bias)$get.objective = TRUE
formals(get_sim_results_selection_bias)$n.quantiles = c(NA, 100, 75, 50, 25, 10, 8, 6, 4, 2)

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
id1 = head(findExperiments(algo.name = "selection_bias"), 10)
print(id1)

testJob(id = 6)



# submit jobs
submitJobs()


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

savedir = "Data/simulations/batchtools/selection_bias_slim/results/"

if (!dir.exists(savedir)) dir.create(savedir, recursive = TRUE)

saveRDS(tab, paste0(savedir,"selection_bias_slim.rds"))

