source("R/tree_splitting_slim.R")
source("R/helper_general.R")
source("R/load_packages.R")
source("R/simulations/batchtools/simulation_setting_definition.R")
library(GGally)
library(data.table)
library(entropy)


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


result_list_slim = list()
result_list_mob = list()
result_table = data.table()
for(i in 1:100){
  set.seed(i)
  data = create_sim_data(job = NULL, n = 1000, type = "linear_smooth")$data
  x = data[, colnames(data) != "y"]
  y = data$y
  
  #slim
  slim = compute_tree_slim(y, x, n.split = 6, min.split = 50)
  split = as.data.table(extract_split_criteria(slim))
  
  slim_res = data.table()
  slim_res$slim_n_leaves = sum(split$split.feature == "leafnode")
  slim_res$slim_n_leaves
  slim_res$slim_depth = split[split.feature != "leafnode", max(depth)]
  slim_res$slim_max_leaf_size = split[split.feature == "leafnode", max(size)]
  slim_res$slim_min_leaf_size = split[split.feature == "leafnode", min(size)]
  slim_res$slim_sd_leaf_size = sd(split[split.feature == "leafnode", size])
  slim_res$slim_n_splitting_variables = length(unique(split$split.feature))-1 #substract "leafnode"
  slim_res$slim_r2_train = r_2(y, predict_slim(slim, x))
  
  #mob
  
  mob = lmtree(fm_mob, data = data, minsize = 50, maxdepth = 6, alpha = 0.001)
  mob_leaf_sizes = unlist(nodeapply(mob, ids = nodeids(mob, terminal = TRUE), function(nodes){info_node(nodes)$nobs}))
  mob_res = data.table()
  mob_res$mob_n_leaves = width(mob)
  mob_res$mob_n_leaves
  mob_res$mob_depth = depth(mob)
  mob_res$mob_max_leaf_size = max(mob_leaf_sizes)
  mob_res$mob_min_leaf_size = min(mob_leaf_sizes)
  mob_res$mob_sd_leaf_size = sd(mob_leaf_sizes)
  mobrule = partykit:::.list.rules.party(mob)
  mob_res$mob_n_splitting_variables = length(unique(unlist(str_extract_all(mobrule,"(x+[1-9])"))))
  
  mob_res$mob_r2_train = r_2(y, predict(mob, x))
  
  
  result_table = rbind(result_table, 
                       cbind("i" = i, slim_res, mob_res))
  result_list_slim[[i]] = split
  result_list_mob[[i]] = mob
}

result_table = as.data.table(result_table)
result_subset = result_table[slim_n_leaves >= 5 & slim_n_leaves < 11,]
plot(result_subset$slim_n_leaves, result_subset$slim_r2)

result_subset[slim_n_leaves == 8, ]
# 1:   2        8 0.9871647
# 2:   3        8 0.9874111
# 3:  93        8 0.9874552
# 4: 140        8 0.9771397
# 5: 150        8 0.9771639
# 6: 162        8 0.9775615
# 7: 168        8 0.9870838
# 8: 184        8 0.9875932


plot(result_subset[ , slim_max_leaf_size], result_subset[ , slim_r2_train])
plot(result_subset[ , slim_sd_leaf_size], result_subset[ , slim_r2_train])

result_subset[n_leaves == 9, ]
View(result_list[[102]])


## der zwei Gruppen bei der trainings Performance kommen daher, dass z.B. n_leaves = 8 
# zum einen dadurch zustande kommen kann, dass bereits nach dem ersten Split ein node 
# nicht mehr weiter gesplittet wird (d.h. ein großer leafnode mit vergleichweise schlechter Performance und 7 kleine)
# oder aber, es werden 8 ähnlich große leafnodes erzeugt -> insgesamt bessere Performance



colnames(result_table)
result_table[slim_n_leaves == 11, .(slim_max_leaf_size, slim_sd_leaf_size, slim_r2_train)]
result_table[mob_n_leaves == 11, .(mob_max_leaf_size, mob_sd_leaf_size, mob_r2_train)]


nodeapply(mob, ids = nodeids(mob), function(nodes){nrow(data_party(nodes))})
innernodes = nodeids(mob)[!(nodeids(mob) %in% nodeids(mob, terminal = TRUE))]
obs = sapply(innernodes, function(id) {
  nrow(data_party(mob[id]))
})
length(obs)

innernodes

rules = str_extract_all(partykit:::.list.rules.party(mob, i = innernodes+1),"(x+[1-9])")

split_feature = unlist(sapply(rules,function(r){
  tail(r,1)
}))
length(split_feature)


share_x2 = nrow(data_party(mob,1))
  