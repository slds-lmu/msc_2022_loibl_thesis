source("R/load_packages.R")

# --- 1. SETUP REGISTRY ---
if (!dir.exists("Data/simulations/chapter_5_simulation_study/correlated_data")) dir.create("Data/simulations/chapter_5_simulation_study/correlated_data", recursive = TRUE)

reg = makeExperimentRegistry(file.dir = "Data/simulations/chapter_5_simulation_study/correlated_data/batchtools",
                             source = c("R/simulations/simulation_setting_definition.R", "R/tree_splitting_slim.R",
                                        "R/simulations/mob_fitting_functions.R",
                                        "R/simulations/helper_simulations.R",
                                        "R/simulations/chapter_5_simulation_study/basic_scenarios/helper_simulations_basic_scenarios.R",
                                        "R/simulations/chapter_5_simulation_study/correlated_data/helper_simulations_correlated_data.R"),
                             seed = 1, conf.file = NA
)


# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

source("R/simulations/simulation_setting_definition.R")

# add problems and setting definitions

addProblem(name = "correlated_data", fun = create_sim_data, reg = reg, seed = 123)
pdes = list("correlated_data" = expand.grid(n = c(1500), type = c("linear_smooth_corr"), rho = c(0.1, 0.5, 0.9)))


# add algorithm
source("R/simulations/chapter_5_simulation_study/correlated_data/helper_simulations_correlated_data.R")

addAlgorithm(name = "get_sim_results_corr", fun = get_sim_results_corr)



# add experiments
addExperiments(
  reg = reg, 
  prob.designs = pdes,
  algo.designs = NULL, 
  repls = 250)

summarizeExperiments()
testJob(1)


submitJobs()
