# load results

resultfiles = list.files("Data/simulations/batchtools/selection_bias_results/", full.names = TRUE)

independence_small = readRDS("Data/simulations/batchtools/selection_bias_results/independence_small_n1000.rds")

library(REdaS)

freqCI(independence_small$slim, level = c(.95))
freqCI(independence_small$guide, level = c(.95))
