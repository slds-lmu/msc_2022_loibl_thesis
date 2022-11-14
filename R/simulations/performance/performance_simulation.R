source("R/simulations/simulation_helper_slim.R")

rep = 50

# max depth 4 -> 8 terminal nodes
linear_smooth = generate_data_sim("basic_linear_smooth", n = 1500, rep = rep, maxdepth = 4, minsize = 50, alpha = 1, pruning = "none")

linear_abrupt = generate_data_sim("basic_linear_abrupt", n = 1500, rep = rep, maxdepth = 4, minsize = 50, alpha = 1, pruning = "none")

categorical_linear = generate_data_sim("categorical_linear", n = 1500, rep = rep, maxdepth = 4, minsize = 50, alpha = 1, pruning = "none")

linear_mixed = generate_data_sim(scenario = "linear_mixed", n = 3000, rep = rep, maxdepth = 4, 
                                           minsize = 100, alpha = 1, pruning = "none")

mixed_large = generate_data_sim(scenario = "mixed_large", n = 5000, rep = rep, maxdepth = 4, 
                                       minsize = 200, alpha = 1, pruning = "none")

save(linear_smooth, linear_abrupt, categorical_linear, linear_mixed, mixed_large, file = "Data/simulations/simulation_study/performance/performance_linear.RData")





# max depth 5 -> 16 terminal nodes

linear_smooth_big = generate_data_sim("basic_linear_smooth", n = 3000, rep = rep, maxdepth = 5, minsize = 50, alpha = 1, pruning = "none", approximate = TRUE)

linear_abrupt_big = generate_data_sim("basic_linear_abrupt", n = 3000, rep = rep, maxdepth = 5, minsize = 50, alpha = 1, pruning = "none", approximate = TRUE)

categorical_linear_big = generate_data_sim("categorical_linear", n = 3000, rep = rep, maxdepth = 5, minsize = 50, alpha = 1, pruning = "none", approximate = FALSE)

linear_mixed_big = generate_data_sim(scenario = "linear_mixed", n = 6000, rep = rep, maxdepth = 5, 
                                 minsize = 100, alpha = 1, pruning = "none", approximate = FALSE)
save(linear_smooth_big, linear_abrupt_big, categorical_linear_big ,
     file = "Data/simulations/simulation_study/performance/performance_linear_big.RData")
