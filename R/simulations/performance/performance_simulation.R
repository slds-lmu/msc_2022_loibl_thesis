source("R/simulations/simulation_helper_slim.R")


linear_smooth = generate_data_sim("basic_linear_smooth", n = 1500, rep = 200, maxdepth = 4, minsize = 50, alpha = 1, pruning = "none")

linear_abrupt = generate_data_sim("basic_linear_abrupt", n = 1500, rep = 200, maxdepth = 4, minsize = 50, alpha = 1, pruning = "none")

categorical_linear = generate_data_sim("categorical_linear", n = 1500, rep = 200, maxdepth = 4, minsize = 50, alpha = 1, pruning = "none")

linear_mixed = generate_data_sim(scenario = "linear_mixed", n = 3000, rep = 200, maxdepth = 4, 
                                           minsize = 100, alpha = 1, pruning = "none")

save(linear_smooth, linear_abrupt, categorical_linear, linear_mixed, file = "Data/simulations/simulation_study/performance/performance_linear.RData")

