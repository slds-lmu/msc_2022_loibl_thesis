# reduce data simulation 
library(batchtools)
library(pdfCluster)


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
    toreduce = ijoin(experiments[exp,], pars)
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
    measure_cols = c("n_leaves", "mse_train", "r2_train", "mse_test", "r2_test")    
    group_cols = c("type", "n", "alpha", "impr", "surrogate", "mbt")
    
    if("x1" %in% colnames(res_df)){
      group_cols = c("type", "n", "surrogate", "mbt", "rho", "biased")
    }
    
    cols_unwrap = c(measure_cols, group_cols, "job.id")
    res_df = unwrap(res_df, cols = cols_unwrap)
    setnames(res_df, paste0(cols_unwrap, ".1"),
             cols_unwrap)
    # save raw result data
    res_df[, config_id:=.GRP,by = group_cols]
    
    
    # summarize results
   
    group_cols = c(group_cols, "config_id")
    
    res_mean_exp = res_df[, lapply(.SD, function(col){mean(col, na.rm = TRUE)}), by = group_cols, .SDcols = measure_cols]
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
    
    
    res_int_exp = ijoin(res_lower_bound_exp, res_upper_bound_exp, by = group_cols)
    res_mean_exp = ijoin(res_mean_exp, res_int_exp, by = group_cols)
    
    
    if("stability" %in% colnames(res_df)){
      # create all possible pairs of simulation repititions
      pair_ids = split(unique(res_df$job.id), ceiling(seq_along(unique(res_df$job.id)) / 2))
      
      # function to calculate gaussian radial basis function (to measure semantic stability)
      rbf = function(pred1, pred2, sigma = 0.01){
        rbf = exp( - sum((as.numeric(pred1) - as.numeric(pred2))^2) / 2*sigma)
        return(rbf)
      }
      
      stability_list = lapply(pair_ids, function(pair){
        stability_df = data.frame(config_id = integer(), ari = double(), rbf = double(), job.id = integer(), evaluationset_id = integer())
        for(conf in unique(res_df$config_id)){
          set1_region =  res_df[job.id == pair[[1]] & config_id == conf, stability][[1]]
          set2_region =  res_df[job.id == pair[[2]] & config_id == conf, stability][[1]]
          
          set1_sem =  res_df[job.id == pair[[1]] & config_id == conf, stability_sem][[1]]
          set2_sem =  res_df[job.id == pair[[2]] & config_id == conf, stability_sem][[1]]
          for(s in 1:length(set1_region)){
            s1_region = set1_region[[s]]
            s2_region = set2_region[[s]]
            
            s1_sem = set1_sem[[s]]
            s2_sem = set2_sem[[s]]
            
            ari = adj.rand.index(s1_region, s2_region)
            rbf = rbf(s1_sem, s2_sem)
            
            stability_df = rbind(stability_df, 
                                 c(config_id = conf, ari = ari, rbf = rbf, job.id = pair[[1]], evaluationset_id = s),
                                 c(config_id = conf, ari = ari, rbf = rbf, job.id = pair[[2]], evaluationset_id = s))
          }
          
        }
        colnames(stability_df) = c("config_id", "ari", "rbf", "job.id", "evaluationset_id")
        
        return(stability_df)
      })
      stability_df = data.table(do.call("rbind", stability_list))
      
      res_save = ijoin(res_df, stability_df, by = c("job.id", "config_id"))
      res_save[, ":="(stability = NULL, stability_sem = NULL)]
      
      saveRDS(res_save, paste0(savedir, exp, "_res_experiments.rds" ))
      
      
      stability_mean = stability_df[, lapply(.SD, function(col){mean(col, na.rm = TRUE)}), by = config_id, .SDcols = c("ari", "rbf")]
      
      stability_lower = stability_df[,  lapply(.SD, lower_bound), by = config_id, .SDcols = c("ari", "rbf")]
      setnames(stability_lower, c("ari", "rbf"), c("ari_05", "rbf_05"))
      
      stability_upper = stability_df[,  lapply(.SD, upper_bound), by = config_id, .SDcols = c("ari", "rbf")]
      setnames(stability_upper, c("ari", "rbf"), c("ari_95", "rbf_95"))
      
      stability_sd = stability_df[, lapply(.SD, function(col){sd(col, na.rm = TRUE)}), by = config_id, .SDcols = c("ari", "rbf")]
      
      
      stability_int = ijoin(stability_lower, stability_upper, by = "config_id")
      stability_mean = ijoin(stability_mean, stability_int, by = "config_id")
      res_mean_exp = ijoin(res_mean_exp, stability_mean)
      res_sd_exp = ijoin(res_sd_exp, stability_sd)
    }
    
    if("x1" %in% colnames(res_df)){
      x1_mean = res_df[, .(x1 = mean(as.numeric(x1))), by = config_id]
      res_mean_exp = ijoin(res_mean_exp, x1_mean)
      
    }

    res_mean_exp$experiment_id = exp
    res_sd_exp$experiment_id = exp
    
    
    res_mean = rbind(res_mean, res_mean_exp)
    res_sd = rbind(res_sd, res_sd_exp)
    
  }
  
  saveRDS(list(mean = res_mean, sd = res_sd), paste0(savedir, "result_summary.rds" ))
  
  return(list(mean = res_mean, sd = res_sd))
  
}
# 
# reg_basic = loadRegistry("Data/simulations/batchtools/basic_scenarios/batchtools"
#                          ,conf.file = NA
# )
# 
# ades_basic = data.frame(alpha = c(0.001, 0.01, 0.05), impr.par = c(0.15, 0.1, 0.05))
# pdes_basic = expand.grid(n = c(1500, 7500, 15000), type = c("linear_smooth", "linear_abrupt", "linear_mixed"))
# 
# savedir_basic = "Data/simulations/batchtools/basic_scenarios/results_test/"
# 
# result_basic = reduce_trees(ades_basic, pdes_basic, savedir_basic, reg_basic)


# result_basic = readRDS("Data/simulations/batchtools/basic_scenarios/results/result_summary.rds")
# View(result_basic$mean[n == 1500 & type == "linear_smooth" , .(surrogate, mbt, alpha, impr, n_leaves_05, n_leaves, n_leaves_95,
#                                                          mse_train_05, mse_train, mse_train_95,
#                                                          r2_train_05, r2_train, r2_train_95,
#                                                          ari_05, ari, ari_95,
#                                                          rbf_05, rbf, rbf_95)])
# 
# View(result_basic$mean[n == 7500 & type == "linear_smooth" , .(surrogate, mbt, alpha, impr, n_leaves_05, n_leaves, n_leaves_95,
#                                                          mse_train_05, mse_train, mse_train_95,
#                                                          r2_train_05, r2_train, r2_train_95,
#                                                          ari_05, ari, ari_95,
#                                                          rbf_05, rbf, rbf_95)])
# 
# View(result_basic$mean[n == 15000 & type == "linear_smooth" , .(surrogate, mbt, alpha, impr, n_leaves_05, n_leaves, n_leaves_95,
#                                                          mse_train_05, mse_train, mse_train_95,
#                                                          r2_train_05, r2_train, r2_train_05,
#                                                          ari_05, ari, ari_95,
#                                                          rbf_05, rbf, rbf_95)])

reg_corr = loadRegistry("Data/simulations/batchtools/correlated_data/batchtools/"
                         ,conf.file = NA
)

ades_corr = NULL
pdes_corr = expand.grid(n = c(1500), type = c("linear_smooth_corr"), rho = c(0.1, 0.5, 0.9), biased = c(FALSE))

savedir_corr = "Data/simulations/batchtools/correlated_data/results/"

result_corr = reduce_trees(ades_corr, pdes_corr, savedir_corr, reg_corr)


result_corr = readRDS("Data/simulations/batchtools/correlated_data/results/result_summary.rds")

View(result_corr[["mean"]][,.(mbt, rho, biased, x1, n_leaves, r2_train, r2_test)])

