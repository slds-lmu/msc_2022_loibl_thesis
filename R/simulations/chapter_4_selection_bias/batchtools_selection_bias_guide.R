library(batchtools)
source("R/load_packages.R")

# --- 1. SETUP REGISTRY ---
if (!dir.exists("Data/simulations/chapter_4_selection_bias/selection_bias_guide")) dir.create("Data/simulations/chapter_4_selection_bias/selection_bias_guide", recursive = TRUE)

reg = makeExperimentRegistry(file.dir = "Data/simulations/chapter_4_selection_bias/selection_bias_guide/batchtools",
                             source = c("R/simulations/chapter_4_selection_bias/simulation_setting_definition.R", "R/tree_splitting_slim.R"),
                             seed = 1)

# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

source("R/simulations/simulation_setting_definition.R")

# add problems and setting definitions
addProblem(name = "selection_bias", fun = create_sim_data, reg = reg)
pdes = expand.grid(n = 1000, type = c(
  "selection_bias_guide",
  "selection_bias_guide_uniform"))
pdes = list("selection_bias" = pdes)


# add algorithm
source("R/simulations/chapter_4_selection_bias/helper_simulation_selection_bias.R")

formals(get_sim_results_selection_bias)$exclude.categoricals = c(TRUE, FALSE)
formals(get_sim_results_selection_bias)$correct.bias = c(TRUE, FALSE)
formals(get_sim_results_selection_bias)$tree_methods = "guide"

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

testJob(id = id1)



# submit jobs
submitJobs()


# reduce jobs/ summarise results
reduce = function(res) res
results = unwrap(reduceResultsDataTable(fun = reduce, reg = reg))
head(results)

pars = unwrap(getJobPars(reg = reg))
tab = ijoin(pars, results)
head(tab)






savedir = "Data/simulations/chapter_4_selection_bias/selection_bias_guide/results/"

if (!dir.exists(savedir)) dir.create(savedir, recursive = TRUE)

saveRDS(tab, paste0(savedir,"selection_bias_guide.rds"))



