source("R/tree_splitting_slim.R")
source("R/helper_general.R")
source("R/load_packages.R")
source("R/mob_fitting_functions.R")
source("R/simulations/batchtools/simulation_setting_definition.R")

data = create_sim_data(job = NULL, n = 2000, type = "linear_smooth_lasso")$data
x = data[,colnames(data)!= "y"]
y = data[,"y"]

slim_lasso = compute_tree_slim(y, x, n.split = 3, impr.par = 0.1, min.split = 100, n.quantiles = 100,
                         approximate = FALSE, penalization = "L1", split.method = "slim", lambda = NULL, df.max = NULL)
slim_lasso_models = extract_models(slim_lasso)
slim_lasso_split = extract_split_criteria(slim_lasso)
slim_lasso_split
sum(slim_lasso_split$split.feature == "leafnode")

slim = compute_tree_slim(y, x, n.split = 5, impr.par = 0.1, min.split = 100, n.quantiles = 100,
                               approximate = FALSE)
slim_models = extract_models(slim)
slim_split = extract_split_criteria(slim)
slim_split

fm_mob = formula(paste("y ~", paste(colnames(x), collapse = "+"), "|", paste(colnames(x), collapse = "+")))

mob = lmtree(fm_mob, data = data, minsize = 50, maxdepth = 6, alpha = 0.001)
mob


ctree = suppressWarnings(partykit::ctree(fm_mob,
                                         data = data,
                                         ytrafo = fit_lm,
                                         control = partykit::ctree_control(minsplit  = 50, maxdepth = 5, 
                                                                           alpha = 0.001)))
ctree


