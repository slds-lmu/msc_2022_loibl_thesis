library(ggpubr)
library(stringr)
source("R/simulations/stability/helper_stability.R")
source("R/load_packages.R")
load("Data/simulations/simulation_study/stability/stability_linear.RData")


stability_linear_smooth = analyse_stability(linear_smooth, 2)
stability_linear_abrupt = analyse_stability(linear_abrupt, 2)
stability_categorical_linear = analyse_stability(categorical_linear, 2)
stability_linear_mixed = analyse_stability(linear_mixed, 2)
stability_mixed_large = analyse_stability(mixed_large, 2)



create_figures_stability(list(linear_smooth = linear_smooth,
                              linear_abrupt = linear_abrupt,
                              categorical_linear = categorical_linear,
                              linear_mixed = linear_mixed,
                              mixed_large = mixed_large))

load("Data/simulations/simulation_study/stability/stability_linear_impr_005.RData")


stability_linear_smooth_005 = analyse_stability(linear_smooth_impr_005, 2)
stability_linear_abrupt_005 = analyse_stability(linear_abrupt_impr_005, 2)
stability_categorical_linear_005 = analyse_stability(categorical_linear_impr_005, 2)
stability_linear_mixed_005 = analyse_stability(linear_mixed_impr_005, 2)
stability_mixed_large_005 = analyse_stability(mixed_large_impr_005, 2)

library(xtable)
print(xtable(stability_linear_mixed_005[["original"]][["slim"]] ), include.rownames = FALSE)
print(xtable(stability_linear_mixed_005[["original"]][["mob"]] ), include.rownames = FALSE)

print(xtable(stability_linear_mixed_005[["lm"]][["slim"]] ), include.rownames = FALSE)
print(xtable(stability_linear_mixed_005[["lm"]][["mob"]] ), include.rownames = FALSE)

print(xtable(stability_linear_mixed_005[["xgboost"]][["slim"]] ), include.rownames = FALSE)
print(xtable(stability_linear_mixed_005[["xgboost"]][["mob"]] ), include.rownames = FALSE)




create_figures_stability(create_figures_stability(list(linear_smooth_impr_005 = linear_smooth_impr_005,
                                                       linear_abrupt_impr_005 = linear_abrupt_impr_005,
                                                       categorical_linear_impr_005 = categorical_linear_impr_005,
                                                       linear_mixed_impr_005 = linear_mixed_impr_005,
                                                       mixed_large_impr_005 = mixed_large_impr_005)))



