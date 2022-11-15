# Simulation to assess stability
source("R/simulations/simulation_helper_slim.R")
rep = 30
linear_smooth = generate_data_sim(scenario = "basic_linear_smooth", type = "stability", n = 1500, rep = rep, maxdepth = 6,
                                                       minsize = 50, alpha = 0.1, pruning = "forward", impr = 0.1)
linear_abrupt = generate_data_sim(scenario = "basic_linear_smooth", type = "stability", n = 1500, rep = rep, maxdepth = 6,
                                                       minsize = 50, alpha = 0.1, pruning = "forward", impr = 0.1)
categorical_linear = generate_data_sim(scenario = "categorical_linear", type = "stability", n = 1500, rep = rep, maxdepth = 6,
                                                       minsize = 50, alpha = 0.1, pruning = "forward", impr = 0.1)
linear_mixed = generate_data_sim(scenario = "linear_mixed", type = "stability", n = 3000, rep = rep, maxdepth = 6, 
                                                      minsize = 100, alpha = 0.1, pruning = "forward", impr = 0.1)
mixed_large = generate_data_sim(scenario = "mixed_large", type = "stability", n = 5000, rep = rep, maxdepth = 6, 
                                minsize = 200, alpha = 0.1, pruning = "forward", impr = 0.1)


save(linear_smooth, linear_abrupt, categorical_linear, linear_mixed, mixed_large, file = "Data/simulations/simulation_study/stability/stability_linear.RData")


linear_smooth_impr_005 = generate_data_sim(scenario = "basic_linear_smooth", type = "stability", n = 1500, rep = rep, maxdepth = 6,
                                  minsize = 50, alpha = 0.1, pruning = "forward", impr = 0.05)
linear_abrupt_impr_005 = generate_data_sim(scenario = "basic_linear_smooth", type = "stability", n = 1500, rep = rep, maxdepth = 6,
                                  minsize = 50, alpha = 0.1, pruning = "forward", impr = 0.05)
categorical_linear_impr_005 = generate_data_sim(scenario = "categorical_linear", type = "stability", n = 1500, rep = rep, maxdepth = 6,
                                       minsize = 50, alpha = 0.1, pruning = "forward", impr = 0.1)
linear_mixed_impr_005 = generate_data_sim(scenario = "linear_mixed", type = "stability", n = 3000, rep = rep, maxdepth = 6, 
                                 minsize = 100, alpha = 0.1, pruning = "forward", impr = 0.05)
mixed_large_impr_005 = generate_data_sim(scenario = "mixed_large", type = "stability", n = 5000, rep = rep, maxdepth = 6, 
                                minsize = 200, alpha = 0.1, pruning = "forward", impr = 0.05)

save(linear_smooth_impr_005, linear_abrupt_impr_005, categorical_linear_impr_005, linear_mixed_impr_005, mixed_large_impr_005, file = "Data/simulations/simulation_study/stability/stability_linear_impr_005.RData")
