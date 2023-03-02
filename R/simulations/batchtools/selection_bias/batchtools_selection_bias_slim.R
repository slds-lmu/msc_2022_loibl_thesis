library(batchtools)
source("R/load_packages.R")

# --- 1. SETUP REGISTRY ---
if (!dir.exists("Data/simulations/batchtools/selection_bias_slim")) dir.create("Data/simulations/batchtools/selection_bias_slim", recursive = TRUE)

reg = makeExperimentRegistry(file.dir = "Data/simulations/batchtools/selection_bias_slim/batchtools",
                             source = c("R/simulations/batchtools/simulation_setting_definition.R", "R/tree_splitting_slim.R"),
                             # conf.file = NA,
                             seed = 1)

# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

source("R/simulations/batchtools/simulation_setting_definition.R")

# add problems and setting definitions
addProblem(name = "selection_bias", fun = create_sim_data, reg = reg)
pdes = expand.grid(n = 1500, type = rep(c(
  "selection_bias_independence_small",
  "selection_bias_independence",
  "selection_bias_independence_10",
  "selection_bias_independence_25",
  "selection_bias_independence_50",
  "selection_bias_independence_100",
  "selection_bias_interaction_numerical_vs_numrical", 
  "selection_bias_interaction_numerical_vs_binary", 
  "selection_bias_interaction_numerical_vs_categorical", 
  "selection_bias_interaction_binary_vs_categorical"
  ), each = 1))
pdes = list("selection_bias" = pdes)


# add algorithm
source("R/simulations/batchtools/selection_bias/helper_simulation_selection_bias.R")

addAlgorithm(name = "get_results_selection_bias", fun = get_sim_results_selection_bias)

ades = list(get_results_selection_bias = data.frame(pruning = "none", alpha = 1,
                                                    tree_methods = "slim", mse_train = TRUE,
                                                    mse_test = TRUE))
ades$get_results_selection_bias$n.quantiles = list(c(NA, 100, 75, 50, 25, 10, 2))


# add experiments
addExperiments(
  reg = reg, 
  prob.designs = pdes,
  algo.designs = ades, 
  repls = 10L)

summarizeExperiments()
summarizeExperiments(by = c("problem", "algorithm", "n", "type"))


# test jobs
ids = getJobTable(reg = reg)[, .(job.id, problem, algorithm)]
ids[, chunk := batchtools::chunk(job.id, chunk.size = 30)]

testJob(1)

# submit jobs
submitJobs(ids = ids, list(walltime = 2700, memory = 512))



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

