# reduce data simulation 
library(batchtools)
library(fossil)
source("R/helper_stability.R")


reduce_trees = function(ades, pdes, savedir, reg){
  if(!is.null(ades)){
    experiments = merge(ades, pdes, by=NULL)
  } else{
    experiments = pdes
  }
  if (!dir.exists(savedir)) dir.create(savedir, recursive = TRUE)

  res_mean = data.table()
  res_sd = data.table()
  
  for(exp in 1:nrow(experiments)){
    # find all job ids which contain repititions of the experiment
    pars = unwrap(getJobPars(reg = reg))
    toreduce = ijoin(experiments[exp,], pars)[1:100,]
    # toreduce = ijoin(toreduce$job.id, findDone(reg = reg))
    reduce = function(res) rbind(res)
    res = reduceResultsDataTable(ids = toreduce$job.id, fun = reduce, reg = reg)
    res_list = lapply(1:nrow(res), function(job){
      job.id = rep(res[job,job.id], nrow(res[job,result][[1]]))
      job.df = cbind(job.id, res[job,result][[1]])
      return(job.df)
    })
    res_df = data.table(do.call("rbind", res_list))
    
    # # nur vorübergehend! muss dann in der Simulation korrigiert werden!
    # res_df = res_df[!is.na(n_leaves), ]
    measure_cols = c("mse_train", "r2_train", "mse_test", "r2_test")    
    group_cols = c("type", "n", "alpha", "impr", "surrogate", "mbt")
    group_cols = group_cols[group_cols %in% colnames(res_df)]
    
    if("rho" %in% colnames(res_df)){
      group_cols = c("type", "n", "surrogate", "mbt", "rho")
    }
    
    cols_unwrap = colnames(res_df)[sapply(res_df, is.list) & colnames(res_df) != "stability"]
    res_df = unwrap(res_df, cols = cols_unwrap)
    setnames(res_df, paste0(cols_unwrap, ".1"),
             cols_unwrap)
    # save raw result data
    res_df[, config_id:=.GRP,by = group_cols]
    
    
    # summarize results
   
    group_cols = c(group_cols, "config_id")
    
    res_mean_exp = res_df[, lapply(.SD, function(col){mean(col, na.rm = TRUE)}), by = group_cols, .SDcols = c(measure_cols, "n_leaves")]
    res_sd_exp = res_df[, lapply(.SD, function(col){sd(col, na.rm = TRUE)}), by = group_cols, .SDcols = measure_cols]
    

    
    
    lower_bound = function(col){
      df =  length(col)-1
      t_score = qt(p=0.05/2, df = df,lower.tail=F)
      mean(col)-t_score*(sd(col)/sqrt(length(col)))
    }
    
    upper_bound = function(col){
      df =  length(col)-1
      t_score = qt(p=0.05/2, df = df,lower.tail=F)
      mean(col)+t_score*(sd(col)/sqrt(length(col)))
    }
    
    res_lower_bound_exp = res_df[, lapply(.SD, lower_bound), by = group_cols, .SDcols = measure_cols]
    setnames(res_lower_bound_exp, measure_cols, paste0(measure_cols, "_05"))
    res_upper_bound_exp = res_df[, lapply(.SD, upper_bound), by = group_cols, .SDcols = measure_cols]
    setnames(res_upper_bound_exp, measure_cols, paste0(measure_cols, "_95"))
    
    res_n_leaves = res_df[, .(n_leaves_min = min(n_leaves),
                              n_leaves_max = max(n_leaves)), by = group_cols]
    
    if("n_splitting_variables" %in% colnames(res_df)){
      res_split_feat = res_df[, .(
        n_splitting_variables = mean(n_splitting_variables),
        n_splitting_variables_min = min(n_splitting_variables),
        n_splitting_variables_max = max(n_splitting_variables)), by = group_cols]
    }

    res_int_exp = ijoin(res_lower_bound_exp, res_upper_bound_exp, by = group_cols)
    res_int_exp = ijoin(res_int_exp, res_n_leaves, by = group_cols)
    res_mean_exp = ijoin(res_mean_exp, res_int_exp, by = group_cols)
    res_mean_exp = ijoin(res_mean_exp, res_split_feat, by = group_cols)
    
    if("stability" %in% colnames(res_df)){
      # create all possible pairs of simulation repititions
      pair_ids = combn(unique(res_df$job.id), 2, simplify = FALSE)
      # set.seed(1)
      # pair_ids_subset = sample(seq_along(pair_ids), 5000)
      set_index = rep(1:100,50)
      # pair_ids = pair_ids[pair_ids_subset]
      stability_list = lapply(seq_along(pair_ids), function(p){
        pair = pair_ids[[p]]
        stability_df = data.frame(config_id = integer(), ri = double(), 
                                  job.id = integer(), evaluationset_id = integer(), stability_same_size = logical())

        for(conf in unique(res_df$config_id)){
          set1_region =  res_df[job.id == pair[[1]] & config_id == conf, stability][[1]]
          set2_region =  res_df[job.id == pair[[2]] & config_id == conf, stability][[1]]
          
          if(length(set1_region)>1){
            s1_region = set1_region[[set_index[p]]]
            s2_region = set2_region[[set_index[p]]]
            
            
            stability_same_size = (length(unique(s1_region)) == length(unique(s2_region)))

            if(stability_same_size){
              ri = rand.index(as.numeric(s1_region), as.numeric(s2_region))
              
              stability_df = rbind(stability_df, 
                                   c(config_id = conf, ri = ri, job.id = pair[[1]], 
                                     evaluationset_id = set_index[p], stability_same_size = stability_same_size),
                                   c(config_id = conf, ri = ri, job.id = pair[[2]], 
                                     evaluationset_id = set_index[p], stability_same_size = stability_same_size))
            }
            
            
          }
            
        }

        colnames(stability_df) = c("config_id", "ri", "job.id", "evaluationset_id", "stability_same_size")
        return(stability_df)
      })
      stability_df = data.table(do.call("rbind", stability_list))
      res_save = ojoin(res_df, stability_df, by = c("job.id", "config_id"))
      res_save[, ":="(stability = NULL)]
      
      saveRDS(res_save, paste0(savedir, exp, "_res_experiments.rds" ))
      
      
      stability_mean = stability_df[, .(ri = mean(ri, na.rm = TRUE),
                                           ri_05 = lower_bound(ri),
                                           ri_95 = upper_bound(ri)), by = config_id]
      

      stability_sd = stability_df[, lapply(.SD, function(col){sd(col, na.rm = TRUE)}), by = config_id, .SDcols = c("ri")]
      
      
      res_mean_exp = ljoin(res_mean_exp, stability_mean)
      res_sd_exp = ljoin(res_sd_exp, stability_sd)
      
    } else if(!("stability" %in% colnames(res_df))){
      saveRDS(res_df, paste0(savedir, exp, "_res_experiments.rds" ))
    }
    
    if("x_wrong" %in% colnames(res_df)){
      x_wrong_mean = res_df[, .(x_wrong = mean(as.numeric(x_wrong))), by = config_id]
      res_mean_exp = ijoin(res_mean_exp, x_wrong_mean)
      
    }
    
    if("share_x3" %in% colnames(res_df)){
      share_x3 = res_df[, .(share_x3 = mean(as.numeric(share_x3))), by = config_id]
      res_mean_exp = ijoin(res_mean_exp, share_x3)
      
    }

    res_mean_exp$experiment_id = exp
    res_mean_exp = cbind(experiments[exp,], res_mean_exp)
    res_sd_exp$experiment_id = exp
    res_sd_exp = cbind(experiments[exp,], res_sd_exp)
    
    
    res_mean = rbind(res_mean, res_mean_exp)
    res_sd = rbind(res_sd, res_sd_exp)
    
  }
  
  saveRDS(list(mean = res_mean, sd = res_sd), paste0(savedir, "result_summary.rds" ))
  
  return(list(mean = res_mean, sd = res_sd))
  
}

# reg_lasso = loadRegistry("Data/simulations/batchtools/lasso/batchtools/"
#                          ,conf.file = "Data/simulations/batchtools/.batchtools.conf.R")
# 
# ades_lasso = NULL
# pdes_lasso = data.frame(n = c(3000), type = c("linear_smooth_lasso"))
# 
# savedir_lasso = "Data/simulations/batchtools/lasso/results/"
# 
# result_lasso = reduce_trees(ades_lasso, pdes_lasso, savedir_lasso, reg_lasso)$mean
# 
# 
# result_lasso = readRDS("Data/simulations/batchtools/lasso/results/result_summary.rds")$mean
# 
# 
# result_lasso[,.(surrogate, mbt, x_wrong, share_x3, n_leaves, r2_train, r2_test)] %>%
#   arrange(., desc(surrogate))%>%
#   kbl(caption="Mean simulation results on 100 simulation runs as stand alone model and surrogate on lm predictions on scenario Linear Smooth - noise features with n = 1000, alpha = 0.001, impr = 0.01",
#       format="latex",
#       col.names = c("surrogate","MBT", "x wrong", "share x3", "n leaves","R2 train","R2 test"),
#       align="r",
#       digits = 4) %>%
#   kable_minimal(full_width = F)
# 


# basic scenarios
reg_basic = loadRegistry("Data/simulations/batchtools/basic_scenarios/batchtools/"
                         ,conf.file = NA
                         )

ades_basic = data.frame(alpha = c(0.001, 0.01, 0.05), impr.par = c(0.15, 0.1, 0.05))
pdes_basic = expand.grid(n = c(1500, 7500), type = c("linear_smooth", "linear_abrupt", "linear_mixed"))

savedir_basic = "Data/simulations/batchtools/basic_scenarios/results/"

result_basic = reduce_trees(ades_basic, pdes_basic, savedir_basic, reg_basic)

result_basic = readRDS("Data/simulations/batchtools/basic_scenarios/results/result_summary.rds")




# correlated data

# reg_corr = loadRegistry("Data/simulations/batchtools/correlated_data/batchtools/"
#                         ,conf.file = NA
# )
# ades_corr = NULL
# pdes_corr = expand.grid(n = c(1500), type = c("linear_smooth_corr"), rho = c(0.1, 0.5, 0.9))
#
# savedir_corr = "Data/simulations/batchtools/correlated_data/results/"
#
# result_corr = reduce_trees(ades_corr, pdes_corr, savedir_corr, reg_corr)$mean
#
#
result_corr = readRDS("Data/simulations/batchtools/correlated_data/results/result_summary.rds")
result_corr_sd = result_corr$sd
setnames(result_corr_sd, c("r2_train", "r2_test"), c("r2_train_sd", "r2_test_sd"))
result_corr_mean = cbind(result_corr$mean, result_corr_sd[,.(r2_train_sd, r2_test_sd)])



result_corr_mean[surrogate == "lm",.(mbt, rho, x_wrong, n_leaves, n_leaves_min, n_leaves_max, r2_train, r2_train_sd, r2_test, r2_test_sd)] %>%
  arrange(., rho, desc(mbt)) %>%
  kbl(caption="Mean simulation results on 250 simulation runs as stand alone model on scenario Linear Smooth - Correlated with n = 1000, alpha = 0.001, impr = 0.01",
      format="latex",
      col.names = c("MBT", "rho", "x1", "n leaves", "n leaves min", "n leaves max", "R2 train", "R2 train sd", "R2 test", "R2 test sd"),
      align="r",
      digits = 4) %>%
  kable_minimal(full_width = F)
