library(stringr)

# extract and aggregate split information for stability assesment

analyse_stability = function(sim_data, depth = 3){
  
  split_list = extract_splits_from_rules(sim_data = sim_data, depth = depth)
  
  aggregated_split_list = lapply(split_list, function(bb_model_list){
    lapply(bb_model_list, function(tree_method_list){
      data_aggr = tree_method_list %>% dplyr::group_by(depth, split_id, feature) %>% 
        dplyr::summarise(share = round(n()/30,2), split.mean = round(mean(as.numeric(value)),2),
                         split.sd = round(sd(as.numeric(value)),2)) %>%
        arrange(depth, split_id, desc(share))
      
    })
  })
  
  return(aggregated_split_list)
}




extract_splits_from_rules = function(sim_data, depth = 2){
  
  list_surr_lm = list(sim_data[["result_surrogate_lm"]][, "rule_slim"],
                      sim_data[["result_surrogate_lm"]][, "rule_slim_aov"],
                      sim_data[["result_surrogate_lm"]][, "rule_slim_r2"],
                      sim_data[["result_surrogate_lm"]][, "rule_slim_r2_adj"],
                      sim_data[["result_surrogate_lm"]][, "rule_mob"],
                      sim_data[["result_surrogate_lm"]][, "rule_ctree"])
  
  list_surr_xgboost = list(sim_data[["result_surrogate_xgboost"]][, "rule_slim"],
                           sim_data[["result_surrogate_xgboost"]][, "rule_slim_aov"],
                           sim_data[["result_surrogate_xgboost"]][, "rule_slim_r2"],
                           sim_data[["result_surrogate_xgboost"]][, "rule_slim_r2_adj"],
                           sim_data[["result_surrogate_xgboost"]][, "rule_mob"],
                           sim_data[["result_surrogate_xgboost"]][, "rule_ctree"])
  
  list_original = list(sim_data[["result_original"]][, "rule_slim"],
                       sim_data[["result_original"]][, "rule_slim_aov"],
                       sim_data[["result_original"]][, "rule_slim_r2"],
                       sim_data[["result_original"]][, "rule_slim_r2_adj"],
                       sim_data[["result_original"]][, "rule_mob"],
                       sim_data[["result_original"]][, "rule_ctree"])
  
  bb_model_list = list(original = list_original, lm = list_surr_lm, xgboost = list_surr_xgboost)
  
  datatable_list = lapply(bb_model_list, function(sim_rule_list){
    
    list_data = lapply(sim_rule_list,function(tree_model_list){
      
      split_list_sim = lapply(1:length(tree_model_list), function(sim_id){
        
        rule = tree_model_list[[sim_id]]
        rule_list = lapply(rule, function(r){
          unlist(str_split(r, " & "))[1:depth]
        })
        
        rule_data = do.call(rbind, rule_list)
        
        
        extract_split = function(rule_data, id = "1"){
          depth = length(str_split(id, "_")[[1]])
          if(all(is.na(rule_data[,1]))) {
            splitlist = list(paste0(c(depth, id, NA, NA, NA), collapse = " AND "))
            return(splitlist)
          } else{
            split = unique(str_subset(rule_data[,1], " <= | %in% "))
            split = str_subset(split, "^(?!.*!).*")[1]
            
            feature = str_split(split, " <= | %in% ")[[1]][1]
            value = str_split(split, " <= | %in% ")[[1]][2]
            splitlist = list(paste0(c(depth, id, feature, value, split), collapse = " AND "))
          }
          
          if(ncol(rule_data) > 1){
            data_1 = data.frame(rule_data[rule_data[,1] == split, -1])
            data_2 = data.frame(rule_data[rule_data[,1] != split, -1])
            
            return(list(splitlist, extract_split(data_1, id = paste0(id, "_1")), 
                        extract_split(data_2, id = paste0(id, "_2"))))
            
          } else {
            return(splitlist)
          }
          
        }
        
        split_list = unlist(extract_split(rule_data))
        split_list = str_split(split_list, " AND ")
        split_df = do.call(rbind, split_list)
        split_df = cbind(split_df, sim_id = sim_id)
        return(split_df)
      })
      split_df_sim = do.call(rbind, split_list_sim)
      colnames(split_df_sim) = c("depth", "split_id", "feature", "value", "rule", "sim_id")
      split_dt_sim = as.data.table(split_df_sim)
      for(id in unique(split_dt_sim$split_id)){
        missing = setdiff(as.character(1:30), split_dt_sim[split_id == id, sim_id])
        if(length(missing) > 0){
          for(missing_sim_id in missing){
            split_dt_sim = rbind(split_dt_sim, t(c(length(str_split(id, "_")[[1]]), id, NA, NA, NA, missing_sim_id)), use.names = FALSE)
          }
        }
      }
      split_dt_sim[, sim_id := as.numeric(sim_id)]
      setorder(split_dt_sim, depth, split_id, sim_id)   
      
      return(split_dt_sim)
    })
    
    names(list_data) = c("slim", "slim_aov", "slim_r2", "slim_r2_adj", "mob", "ctree")
    
    return(list_data)
  })
  
  return(datatable_list)
  

}


# Create barplots for rare data (e.g. number of terminal nodes)


create_figures_stability = function(simulation_list){
  if (!dir.exists("Figures")) dir.create("Figures", recursive = TRUE)
  if (!dir.exists("Figures/Stability")) dir.create("Figures/Stability", recursive = TRUE)
  
  
  for(el in names(simulation_list)){
    if (!dir.exists(paste0("Figures/Stability/", el))) dir.create(paste0("Figures/Stability/", el), recursive = TRUE)
    
    col_res_orig = colnames(simulation_list[[el]]$result_original)
    col_res_surr_lm = colnames(simulation_list[[el]]$result_surrogate_lm)
    col_res_surr_xgboost = colnames(simulation_list[[el]]$result_surrogate_xgboost)
    
    n_term_nodes_orig = simulation_list[[el]]$result_original[,str_detect(col_res_orig, "n_term_nodes")]
    colnames(n_term_nodes_orig) = str_remove(colnames(n_term_nodes_orig), "n_term_nodes_")
    
    n_term_nodes_lm = simulation_list[[el]]$result_surrogate_lm[,str_detect(col_res_surr_lm, "n_term_nodes")]
    colnames(n_term_nodes_lm) = str_remove(colnames(n_term_nodes_lm), "n_term_nodes_")
    
    p_nofnodes_lm = ggplot(stack(data.frame(orig = n_term_nodes_orig, 
                                            surr = n_term_nodes_lm)),
                           aes(x = ind, y = values)) +
      stat_boxplot(geom = "errorbar", width = 0.5) +
      ggtitle("Number of terminal nodes - surrogate for lm", subtitle = str_replace(el, "_", " ")) +
      labs(x="model", y="number of terminal nodes") +
      geom_boxplot()
    
    ggexport(p_nofnodes_lm, filename = paste0("Figures/Stability/", el, "/lm_nofnodes.pdf"), width = 12, height = 3.8)
    
    n_term_nodes_xgboost = simulation_list[[el]]$result_surrogate_xgboost[,str_detect(col_res_surr_xgboost, "n_term_nodes")]
    colnames(n_term_nodes_xgboost) = str_remove(colnames(n_term_nodes_xgboost), "n_term_nodes_")
    
    p_nofnodes_xgboost = ggplot(stack(data.frame(orig = n_term_nodes_orig, 
                                                 surr = n_term_nodes_xgboost)),
                                aes(x = ind, y = values)) +
      stat_boxplot(geom = "errorbar", width = 0.5) +
      ggtitle("Number of terminal nodes - surrogate for xgboost", subtitle = str_replace(el, "_", " ")) +
      labs(x="model", y="number of terminal nodes") +
      geom_boxplot()
    
    ggexport(p_nofnodes_xgboost, filename = paste0("Figures/Stability/", el, "/xgboost_nofnodes.pdf"), width = 12, height = 3.8)
    
    
    
    r2_train_orig = simulation_list[[el]]$result_original[,str_detect(col_res_orig,"r2_train")]
    colnames(r2_train_orig) = str_remove(colnames(r2_train_orig), "r2_train_")
    
    r2_train_surr_lm = simulation_list[[el]]$result_surrogate_lm[,str_detect(col_res_surr_lm, "r2_train")]
    colnames(r2_train_surr_lm) = str_remove(colnames(r2_train_surr_lm), "r2_train_")
    
    p_r2_train_lm = ggplot(stack(data.frame(orig = r2_train_orig,
                                            surr = r2_train_surr_lm)),
                           aes(x = ind, y = values)) +
      stat_boxplot(geom = "errorbar", width = 0.5) +
      ggtitle("R squared - Training", subtitle = str_replace(el, "_", " ")) +
      labs(x="model", y="R2") +
      geom_boxplot()
    
    ggexport(p_r2_train_lm, filename = paste0("Figures/Stability/", el, "/lm_r2_train.pdf"), width = 12, height = 3.8)
    
    r2_train_surr_xgboost = simulation_list[[el]]$result_surrogate_xgboost[,str_detect(col_res_surr_xgboost, "r2_train")]
    colnames(r2_train_surr_xgboost) = str_remove(colnames(r2_train_surr_xgboost), "r2_train_")
    
    p_r2_train_xgboost = ggplot(stack(data.frame(orig = r2_train_orig,
                                                 surr = r2_train_surr_xgboost)),
                                aes(x = ind, y = values)) +
      stat_boxplot(geom = "errorbar", width = 0.5) +
      ggtitle("R squared - Training", subtitle = str_replace(el, "_", " ")) +
      labs(x="model", y="R2") +
      geom_boxplot()
    
    ggexport(p_r2_train_xgboost, filename = paste0("Figures/Stability/", el, "/xgboost_r2_train.pdf"), width = 12, height = 3.8)
    
    
    
    p_r2_test_lm = ggplot(stack(data.frame(orig = simulation_list[[el]]$result_original[,str_detect(col_res_orig, "r2_test")],
                                           surr = simulation_list[[el]]$result_surrogate_lm[,str_detect(col_res_surr_lm, "r2_test")])),
                          aes(x = ind, y = values)) +
      stat_boxplot(geom = "errorbar", width = 0.5) +
      ggtitle("R squared - testing", subtitle = str_replace(el, "_", " ")) +
      labs(x="model", y="R2") +
      geom_boxplot()
    
    ggexport(p_r2_test_lm, filename = paste0("Figures/Stability/", el, "/lm_r2_test.pdf"), width = 12, height = 3.8)
    
    p_r2_test_xgboost = ggplot(stack(data.frame(orig = simulation_list[[el]]$result_original[,str_detect(col_res_orig, "r2_test")],
                                                surr = simulation_list[[el]]$result_surrogate_xgboost[,str_detect(col_res_surr_xgboost, "r2_test")])),
                               aes(x = ind, y = values)) +
      stat_boxplot(geom = "errorbar", width = 0.5) +
      ggtitle("R squared - testing", subtitle = str_replace(el, "_", " ")) +
      labs(x="model", y="R2") +
      geom_boxplot()
    
    ggexport(p_r2_test_xgboost, filename = paste0("Figures/Stability/", el, "/xgboost_r2_test.pdf"), width = 12, height = 3.8)
  }
}

