library(batchtools)
source("R/load_packages.R")

# --- 1. SETUP REGISTRY ---
if (!dir.exists("Data/simulations/batchtools/lasso")) dir.create("Data/simulations/batchtools/lasso", recursive = TRUE)

reg = makeExperimentRegistry(file.dir = "Data/simulations/batchtools/lasso/batchtools",
                             source = c("R/simulations/batchtools/simulation_setting_definition.R", "R/tree_splitting_slim.R",
                                        "R/mob_fitting_functions.R",
                                        "R/simulations/batchtools/helper_simulations.R",
                                        "R/simulations/batchtools/lasso/helper_simulations_lasso.R"),
                             seed = 1
                             , conf.file = "Data/simulations/batchtools/.batchtools.conf.R"
)
reg = loadRegistry("Data/simulations/batchtools/lasso/batchtools", writeable = TRUE, conf.file = "Data/simulations/batchtools/.batchtools.conf.R")


# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

source("R/simulations/batchtools/simulation_setting_definition.R")

# add problems and setting definitions

addProblem(name = "noisy_data", fun = create_sim_data, reg = reg, seed = 100)
pdes = list("noisy_data" = data.frame(n = c(3000), type = c("linear_smooth_lasso")))


# add algorithm
source("R/simulations/batchtools/lasso/helper_simulations_lasso.R")

addAlgorithm(name = "get_sim_results_lasso", fun = get_sim_results_lasso)



# add experiments
addExperiments(
  reg = reg, 
  prob.designs = pdes,
  algo.designs = NULL, 
  repls = 250)

summarizeExperiments()
testJob(1)
submitJobs()
