source("R/tree_splitting_slim.R")
source("R/helper_general.R")
source("R/load_packages.R")
source("R/simulations/batchtools/simulation_setting_definition.R")

# Frage: Warum gibt es bei SLIM und GUIDE diese beiden Gruppen bei der Trainingsperformance
res_ls_n1500_alpha001 = readRDS("Data/simulations/batchtools/basic_scenarios/results/1_res_experiments.rds")

res_ls_n1500_alpha01 = readRDS("Data/simulations/batchtools/basic_scenarios/results/2_res_experiments.rds")


overview_ls = rbind(unique(res_ls_n1500_alpha01[mbt %in% c("SLIM", "GUIDE") & surrogate == "standalone",
                                                .(n_leaves, r2_train, r2_test, mse_train, mse_test, mbt, job.id, config_id, ri, stability_same_size)]),
                    unique(res_ls_n1500_alpha001[mbt %in% c("MOB", "CTree") & surrogate == "standalone",
                                                 .(n_leaves, r2_train, r2_test, mse_train, mse_test, mbt, job.id, config_id, ri, stability_same_size)]))





ggpairs(overview_ls[!is.na(ri) & stability_same_size == TRUE ,.(n_leaves, ri, r2_train, mbt)],
        columns = 1:3,        # Columns
        aes(color = mbt,  # Color by group (cat. variable)
            alpha = 0.9)) 


result_list = list()
result_table = c()
for(i in 101:200){
  set.seed(i)
  data = create_sim_data(job = NULL, n = 1000, type = "linear_smooth")$data
  x = data[, colnames(data) != "y"]
  y = data$y
  
  slim = compute_tree_slim(y, x ,n.split = 6, 
                           impr.par = 0.1, min.split = 50, approximate = FALSE,
                           split.method = "slim")
  split = as.data.table(extract_split_criteria(slim))
  n_leaves = sum(split$split.feature == "leafnode")
  r2 = r_2(y, predict_slim(slim, x))
  result_table = rbind(result_table, data.table("i" = i, n_leaves = n_leaves, r2 = r2))
  result_list[[i]] = split
}

result_table = as.data.table(result_table)
result_subset = result_table[n_leaves >= 7 & n_leaves < 10,]
plot(result_subset$n_leaves, result_subset$r2)

result_subset[n_leaves == 8, ]
# 1:   2        8 0.9871647
# 2:   3        8 0.9874111
# 3:  93        8 0.9874552
# 4: 140        8 0.9771397
# 5: 150        8 0.9771639
# 6: 162        8 0.9775615
# 7: 168        8 0.9870838
# 8: 184        8 0.9875932
n_8 = result_subset[n_leaves == 8, i]

result_list[[184]][1:4,]

## der zwei Gruppen bei der trainings Performance kommen daher, dass z.B. n_leaves = 8 
# zum einen dadurch zustande kommen kann, dass bereits nach dem ersten Split ein node 
# nicht mehr weiter gesplittet wird (d.h. ein großer leafnode mit vergleichweise schlechter Performance und 7 kleine)
# oder aber, es werden 8 ähnlich große leafnodes erzeugt -> insgesamt bessere Performance

