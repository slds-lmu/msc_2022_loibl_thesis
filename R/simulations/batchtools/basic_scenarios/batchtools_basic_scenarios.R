library(batchtools)
source("R/load_packages.R")

# --- 1. SETUP REGISTRY ---
if (!dir.exists("Data/simulations/batchtools/basic_scenarios")) dir.create("Data/simulations/batchtools/basic_scenarios", recursive = TRUE)

reg = makeExperimentRegistry(file.dir = "Data/simulations/batchtools/basic_scenarios/batchtools",
                             source = c("R/simulations/batchtools/simulation_setting_definition.R", "R/tree_splitting_slim.R",
                                        "R/mob_fitting_functions.R",
                                        "R/simulations/batchtools/helper_simulations.R",
                                        "R/simulations/batchtools/basic_scenarios/helper_simulations_basic_scenarios.R"),
                             seed = 1
                             , conf.file = "Data/simulations/batchtools/.batchtools.conf.R"
                             )
# reg = loadRegistry("Data/simulations/batchtools/basic_scenarios/batchtools", writeable = TRUE,
#                    conf.file = "Data/simulations/batchtools/.batchtools.conf.R")

# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

source("R/simulations/batchtools/simulation_setting_definition.R")

# add problems and setting definitions
repls = 100L
set.seed(49)
data_stability = lapply(c(linear_smooth = "linear_smooth", linear_abrupt = "linear_abrupt", linear_mixed = "linear_mixed"),
                        function(t){
                          lapply(1:100, function(i){create_sim_data(job = NULL, n = 1000, type = t)$data})
                        })


addProblem(name = "basic_scenarios", data = data_stability, fun = create_sim_data, reg = reg, seed = 123)
pdes = list("basic_scenarios" = expand.grid(n = c(1500, 7500), type = c("linear_smooth", "linear_abrupt", "linear_mixed")))


# add algorithm
source("R/simulations/batchtools/basic_scenarios/helper_simulations_basic_scenarios.R")

addAlgorithm(name = "get_sim_results", fun = get_sim_results)
ades = list(get_sim_results = data.frame(alpha = c(0.001, 0.01, 0.05), impr.par = c(0.15, 0.1, 0.05)))



# add experiments
addExperiments(
  reg = reg, 
  prob.designs = pdes,
  algo.designs = ades, 
  repls = repls)

summarizeExperiments()
testJob(27)
submitJobs(447:1800)
getJobTable(1:9)

# pars = unwrap(getJobPars(reg = reg))

