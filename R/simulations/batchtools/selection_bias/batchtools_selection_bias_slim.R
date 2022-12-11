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
pdes = expand.grid(n = 1000, type = rep(c("selection_bias_independence_small",
                                          "selection_bias_full_interaction"), each = 1))
pdes = list("selection_bias" = pdes)


# add algorithm
source("R/simulations/batchtools/selection_bias/helper_simulation_selection_bias.R")
formals(get_sim_results_selection_bias)$tree_methods = "slim"
formals(get_sim_results_selection_bias)$n.quantiles = c(NA, 100, 10, 4, 2)

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

testJob(id = 20)



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

for (t in unique(tab$type)){
  for (n in unique(tab[type == t , n])){
    tab_t_n = tab[type == t & n == n, ]
    result = list(slim_exact = table(tab_t_n$split_slim_exact),
                  slim_100 = table(tab_t_n$split_slim_100),
                  slim_10 = table(tab_t_n$split_slim_100),
                  slim_4 = table(tab_t_n$split_slim_4),
                  slim_2 = table(tab_t_n$split_slim_2))
    saveRDS(result, file = paste0(savedir, str_remove(t, "selection_bias_"), "_n", n, ".rds"))
    
  }
}
