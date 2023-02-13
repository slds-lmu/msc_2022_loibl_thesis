library(batchtools)
source("R/load_packages.R")

# --- 1. SETUP REGISTRY ---
if (!dir.exists("Data/simulations/batchtools/correlated_data")) dir.create("Data/simulations/batchtools/correlated_data", recursive = TRUE)

reg = makeExperimentRegistry(file.dir = "Data/simulations/batchtools/correlated_data/batchtools",
                             source = c("R/simulations/batchtools/simulation_setting_definition.R", "R/tree_splitting_slim.R",
                                        "R/mob_fitting_functions.R",
                                        "R/simulations/batchtools/helper_simulations.R",
                                        "R/simulations/batchtools/basic_scenarios/helper_simulations_basic_scenarios.R",
                                        "R/simulations/batchtools/correlated_data/helper_simulations_correlated_data.R"),
                             seed = 1
                             , conf.file = "Data/simulations/batchtools/.batchtools.conf.R"
)


# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

source("R/simulations/batchtools/simulation_setting_definition.R")

# add problems and setting definitions

addProblem(name = "correlated_data", fun = create_sim_data, reg = reg, seed = 123)
pdes = list("correlated_data" = expand.grid(n = c(1500), type = c("linear_smooth_corr"), rho = c(0.1, 0.5, 0.9)))


# add algorithm
source("R/simulations/batchtools/correlated_data/helper_simulations_correlated_data.R")

addAlgorithm(name = "get_sim_results_corr", fun = get_sim_results_corr)



# add experiments
addExperiments(
  reg = reg, 
  prob.designs = pdes,
  algo.designs = NULL, 
  repls = 250)

summarizeExperiments()
testJob(5)
submitJobs(1:750,reg = reg)


# pars = unwrap(getJobPars(reg = reg))

# test = readRDS("Data/simulations/batchtools/correlated_data/batchtools/results/100.rds")
