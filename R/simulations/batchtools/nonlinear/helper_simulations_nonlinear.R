get_sim_results_nonlinear = function(data, job, instance, 
                                     n.quantiles = 100, min.split = 100, n.split = 6, impr.par = 0.1, r2 = 0.99, ... ){
  
  # The data used to train the trees and evaluate their performance is re-simulated with each repetition.
  data = instance$data
  
  
  # -- standalone model 
  
  # train test split
  split_point = nrow(data)/3*2
  train = data[1:split_point,]
  test = data[(split_point+1):nrow(data),]
  x_train = train[, colnames(train) != "y"]
  x_test = test[, colnames(test) != "y"]
  
  x_train = train[, colnames(train) != "y"]
  x_test = test[, colnames(test) != "y"]
  
  y_train = train$y
  y_test = test$y
  
  # fit trees to the original data
  slim_basic = fit_slim_tree(x_train = x_train, y_train = y_train, x_test = x_test, y_test = y_test, 
                             n.split = n.split, impr.par = impr.par, r2 = r2, min.split = min.split, 
                             degree.poly = 1, penalization = NULL, fit.bsplines = FALSE, fit.gam = FALSE)
  
  slim_poly = fit_slim_tree(x_train = x_train, y_train = y_train, x_test = x_test, y_test = y_test, 
                          n.split = n.split, impr.par = impr.par, r2 = r2, min.split = min.split, 
                          degree.poly = 2, penalization = "L1", fit.bsplines = FALSE, fit.gam = FALSE)
  
  slim_bsplines = fit_slim_tree(x_train = x_train, y_train = y_train, x_test = x_test, y_test = y_test, 
                                n.split = n.split, impr.par = impr.par, r2 = r2, min.split = min.split, 
                                degree.poly = 1, penalization = NULL, fit.bsplines = TRUE, fit.gam = FALSE)
  
  slim_gam = fit_slim_tree(x_train = x_train, y_train = y_train, x_test = x_test, y_test = y_test, 
                                n.split = n.split, impr.par = impr.par, r2 = r2, min.split = min.split, 
                                degree.poly = 1, penalization = NULL, fit.bsplines = FALSE, fit.gam = TRUE)
                          
                             
  result = rbind(slim_basic, slim_poly, slim_bsplines, slim_gam)
  result$model = c("basic_lm", "penalized_poly", "bsplines", "gam")
  return(result)
  
  
}

fit_slim_tree = function(x_train, y_train, x_test, y_test , 
                         n.split, impr.par, r2, min.split,
                         degree.poly, fit.bsplines, fit.gam, penalization){
  slim_res = list()
  
  slim = compute_tree_slim(y = y_train, x = x_train , n.split = n.split, n.quantiles = n.quantiles,
                           impr.par = impr.par, r2 = r2, min.split = min.split, degree.poly = degree.poly, 
                           fit.bsplines = fit.bsplines, fit.gam = fit.gam, penalization = penalization)

  split = as.data.table(extract_split_criteria(slim))
  models = extract_models(slim)
  
  leafnode_ids = split[split.feature == "leafnode",id.node]
  
  slim_res$n_leaves = sum(split$split.feature == "leafnode")
  slim_res$n_splitting_variables = length(unique(split$split.feature))-1 #substract "leafnode"
  # Share of observations split by x2 or x3 in all split observations
  slim_res$share_main_effect_split = sum(split[split.feature %in% c("x2", "x3"), size])/sum(split[split.feature != "leafnode", size])
  
  if(!is.null(penalization)){
    if(penalization == "L1"){
      slim_res$df = mean(sapply(as.character(leafnode_ids), function(id){
        models[[id]][["model"]][["df"]]
      }))
    }
  } else if (fit.bsplines){
    slim_res$df = c(NA)
  } else if (fit.gam){
    slim_res$df = c(NA)
  } else{
    slim_res$df = mean(sapply(as.character(leafnode_ids), function(id){
      length(models[[id]][["model"]][["coefficients"]])
    }))
  }
 
  slim_res$mse_train = mean((predict_slim(slim, x_train, degree.poly = degree.poly)- y_train)^2)
  slim_res$r2_train = r_2(y_train, predict_slim(slim, x_train, degree.poly = degree.poly))
  
  slim_res$mse_test = mean((predict_slim(slim, x_test, degree.poly = degree.poly)- y_test)^2)
  slim_res$r2_test = r_2(y_test, predict_slim(slim, x_test, degree.poly = degree.poly))
  
  
  return(slim_res)
  
}


