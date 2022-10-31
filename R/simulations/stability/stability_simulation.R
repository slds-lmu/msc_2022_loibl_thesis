# Simulation to assess stability
source("R/simulations/simulation_helper_slim.R")

linear_smooth = generate_data_sim(scenario = "basic_linear_smooth", type = "stability", n = 1000, rep = 200, maxdepth = 6,
                                                       minsize = 30, alpha = 0.1, pruning = "forward", impr = 0.1)
linear_abrupt = generate_data_sim(scenario = "basic_linear_smooth", type = "stability", n = 1000, rep = 200, maxdepth = 6,
                                                       minsize = 30, alpha = 0.1, pruning = "forward", impr = 0.1)
categorical_linear = generate_data_sim(scenario = "categorical_linear", type = "stability", n = 1000, rep = 200, maxdepth = 6,
                                                       minsize = 30, alpha = 0.1, pruning = "forward", impr = 0.1)
linear_mixed = generate_data_sim(scenario = "linear_mixed", type = "stability", n = 2000, rep = 200, maxdepth = 6, 
                                                      minsize = 50, alpha = 0.1, pruning = "forward", impr = 0.1)

save(linear_smooth, linear_abrupt, categorical_linear, linear_mixed, file = "Data/simulations/simulation_study/stability/stability_linear.RData")


