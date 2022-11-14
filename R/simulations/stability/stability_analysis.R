library(ggpubr)
library(stringr)
source("R/simulations/stability/helper_stability.R")

load("Data/simulations/simulation_study/stability/stability_linear.RData")


stability_linear_smooth = analyse_stability(linear_smooth, 2)
stability_linear_abrupt = analyse_stability(linear_abrupt, 2)
stability_categorical_linear = analyse_stability(categorical_linear, 2)
stability_linear_mixed = analyse_stability(linear_mixed, 2)
stability_mixed_large = analyse_stability(mixed_large, 2)


simulation_list = list(linear_smooth = linear_smooth,
                       linear_abrupt = linear_abrupt,
                       categorical_linear = categorical_linear,
                       linear_mixed = linear_mixed,
                       mixed_large = mixed_large)

create_figures_stability(simulation_list)


