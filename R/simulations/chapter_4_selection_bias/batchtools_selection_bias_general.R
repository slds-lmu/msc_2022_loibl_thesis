source("R/load_packages.R")

# --- 1. SETUP REGISTRY ---
if (!dir.exists("Data/simulations/chapter_4_selection_bias/selection_bias_general")) dir.create("Data/simulations/chapter_4_selection_bias/selection_bias_general", recursive = TRUE)

reg = makeExperimentRegistry(file.dir = "Data/simulations/chapter_4_selection_bias/selection_bias_general/batchtools",
                             source = c("R/simulations/simulation_setting_definition.R", "R/tree_splitting_slim.R",
                                        "R/simulations/mob_fitting_functions.R"),
                             conf.file = NA,
                             seed = 1)

# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---


source("R/simulations/simulation_setting_definition.R")

# add problems and setting definitions
addProblem(name = "selection_bias", fun = create_sim_data, reg = reg)
pdes = expand.grid(n = 1000, type = rep(c(
  "selection_bias_independence",
  "selection_bias_independence_small",
  "selection_bias_interaction_numerical_vs_numrical", 
  "selection_bias_interaction_numerical_vs_binary", 
  "selection_bias_interaction_numerical_vs_categorical", 
  "selection_bias_interaction_binary_vs_categorical"), each = 1))
pdes = list("selection_bias" = pdes)


# add algorithm
source("R/simulations/chapter_4_selection_bias/helper_simulation_selection_bias.R")

addAlgorithm(name = "get_results_selection_bias", fun = get_sim_results_selection_bias)

ades = list(get_results_selection_bias = data.frame(pruning = "none", alpha = 1))
ades$get_results_selection_bias$n.quantiles = list(c(NA, 100, 50, 10))
ades$get_results_selection_bias$exclude.categoricals = list(c(TRUE, FALSE))
ades$get_results_selection_bias$correct.bias = list(c(TRUE, FALSE))

# add experiments
addExperiments(
  reg = reg, 
  prob.designs = pdes,
  algo.designs = ades, 
  repls = 1000L)

summarizeExperiments()
summarizeExperiments(by = c("problem", "algorithm", "n", "type"))


# test jobs
id1 = head(findExperiments(algo.name = "selection_bias"), 1)
print(id1)

testJob(id = 101)


# submit jobs
pars = unwrap(getJobPars(reg = reg))

submitJobs(reg = reg)
submitJobs(pars[type == "selection_bias_interaction_numerical_vs_numrical",job.id])



# reduce jobs/ summarise results
reduce = function(res) res
results = unwrap(reduceResultsDataTable(fun = reduce, reg = reg))
head(results)

pars = unwrap(getJobPars(reg = reg))
tab = ijoin(pars, results)
head(tab)



savedir = "Data/simulations/chapter_4_selection_bias/selection_bias_general/results/"

if (!dir.exists(savedir)) dir.create(savedir, recursive = TRUE)
saveRDS(tab, paste0(savedir,"selection_bias_general.rds"))
