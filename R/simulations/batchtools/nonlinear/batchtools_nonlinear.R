library(batchtools)
source("R/load_packages.R")

# --- 1. SETUP REGISTRY ---
if (!dir.exists("Data/simulations/batchtools/nonlinear")) dir.create("Data/simulations/batchtools/nonlinear", recursive = TRUE)

reg = makeExperimentRegistry(file.dir = "Data/simulations/batchtools/nonlinear/batchtools",
                             source = c("R/simulations/batchtools/simulation_setting_definition.R", "R/tree_splitting_slim.R",
                                        "R/mob_fitting_functions.R",
                                        "R/simulations/batchtools/nonlinear/helper_simulations_nonlinear.R"),
                             seed = 1
                             # , conf.file = "Data/simulations/batchtools/.batchtools.conf.R"
)


# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

source("R/simulations/batchtools/simulation_setting_definition.R")

# add problems and setting definitions

addProblem(name = "nonlinear_data", fun = create_sim_data, reg = reg, seed = 123)
pdes = list("nonlinear_data" = data.frame(n = c(7500), type = c("nonlinear_mixed")))


# add algorithm
source("R/simulations/batchtools/nonlinear/helper_simulations_nonlinear.R")

addAlgorithm(name = "get_sim_results_nonlinear", fun = get_sim_results_nonlinear)



# add experiments
addExperiments(
  reg = reg, 
  prob.designs = pdes,
  algo.designs = NULL, 
  repls = 50)

summarizeExperiments()
testJob(1)
submitJobs(resources = list(walltime = 9000))