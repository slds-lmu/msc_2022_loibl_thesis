# reduce data simulation 
library(batchtools)
library(pdfCluster)


reduce_trees = function(ades, pdes, savedir, reg){
  # 27 unique experiments
  experiments = merge(ades, pdes, by=NULL)
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
    
    measure_cols = c("n_leaves", "mse_train", "r2_train", "mse_test", "r2_test")    
    group_cols = c("job.id", "type", "n", "alpha", "impr", "surrogate", "mbt")
    cols_unwrap = c(measure_cols, group_cols)
    res_df = unwrap(res_df, cols = cols_unwrap)
    setnames(res_df, paste0(cols_unwrap, ".1"),
             cols_unwrap)
    
    # save raw result data
    saveRDS(res_df, paste0(savedir, exp, "_res_experiments.rds" ))
    
    
    # summarize results
    res_df[, config_id:=.GRP,by = list(type, n, alpha, impr, surrogate, mbt)]
   

    res_mean_exp = res_df[, lapply(.SD, mean), by = list(type, n, alpha, impr, surrogate, mbt, config_id), .SDcols = measure_cols]
    res_sd_exp = res_df[, lapply(.SD, sd), by = list(type, n, alpha, impr, surrogate, mbt, config_id), .SDcols = measure_cols]
    
    
    # create all possible pairs of simulation repititions
    pair_ids = combn(unique(res_df$job.id), 2, simplify = FALSE)

    stability_list = lapply(pair_ids, function(pair){
      stability_df = data.frame(config_id = integer(), ari = double())
      for(conf in unique(res_df$config_id)){
        set1 =  res_df[job.id == pair[[1]] & config_id == conf, stability][[1]]
        set2 =  res_df[job.id == pair[[2]] & config_id == conf, stability][[1]]
        stability_df = rbind(stability_df, c(config_id = conf, ari = adj.rand.index(set1, set2)))
      }
      colnames(stability_df) = c("config_id", "stability")
      
      return(stability_df)
    })

    stability_df = data.table(do.call("rbind", stability_list))
    stability_mean = stability_df[, .(stability = mean(stability)), by = config_id]
    stability_sd = stability_df[, .(stability = sd(stability)), by = config_id]
    res_mean_exp = ijoin(res_mean_exp, stability_mean)
    res_sd_exp = ijoin(res_sd_exp, stability_sd)
    res_mean_exp$experiment_id = exp
    res_sd_exp$experiment_id = exp
    
    
    res_mean = rbind(res_mean, res_mean_exp)
    res_sd = rbind(res_sd, res_sd_exp)
    
    
  }
  
  saveRDS(list(mean = res_mean, sd = res_sd), paste0(savedir, "result_summary.rds" ))
  
  return(list(mean = res_mean, sd = res_sd))
  
}


reg = loadRegistry("Data/simulations/batchtools/basic_scenarios/batchtools", conf.file = NA)

ades = data.frame(alpha = c(0.01, 0.05,0.1), impr.par = c(0.15, 0.1, 0.05))
pdes = expand.grid(n = c(1500, 7500, 15000), type = c("linear_smooth", "linear_abrupt", "linear_mixed"))

savedir = "Data/simulations/batchtools/basic_scenarios/results/"

result = reduce_trees(ades, pdes, savedir, reg)


