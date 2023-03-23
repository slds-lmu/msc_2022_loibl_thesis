create_sim_data = function(job, n = 1000, type, rho = 0, ...){
  
  # basic scenarios
  if (type == "linear_smooth"){
    x1 = runif(n, -1, 1)
    x2 = runif(n, -1, 1)
    x3 = runif(n, -1, 1)

    
    formula = x1 + 4*x2 + 3*x2*x3 
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    
    data = data.frame(x1, x2, x3, y)
    fm = as.formula("y ~ x1 + x2 + x2:x3")
    lrn = lrn("regr.xgboost",
              max_depth = 5,
              eta = 0.5,
              alpha = 1,
              gamma = 2,
              nrounds = 400,
              interaction_constraints = "[[1,2]]")
    
   
  # linear categorical
  } else if(type == "linear_abrupt"){
    
    x1 = runif(n, -1, 1)
    x2 = runif(n, -1, 1)
    x3 = as.numeric(rbernoulli(n))
  
    
    formula = x1 - 8*x2 + ifelse(x3 == 0, I(16*x2),0) + ifelse(x1 > mean(x1), I(8*x2),0) 

    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    

    
    data = data.frame(x1, x2, x3, y)
    fm = as.formula("y ~ x1 + x2 + x2:x3 + ti(x1,x2)")
    lrn = lrn("regr.xgboost",
              max_depth = 3,
              eta = 0.5,
              alpha = 0.5,
              gamma = 1,
              nrounds = 350,
              interaction_constraints = "[[0,1], [1,2]]")
    
   
    
  } else if(type == "linear_mixed"){
    
    x1 = runif(n, -1, 1)
    x2 = runif(n, -1, 1)
    x3 = as.numeric(rbernoulli(n))
    x4 = as.numeric(rbernoulli(n))
    
    
    formula = 4*x2 + 2*x4 + 4*x2*x1 + ifelse(x3 == 0, 8*x2,0) + 
      ifelse(x4 == 1, 8*x1*x2, 0)
    
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    
    data = data.frame(mget(paste0("x",1:4)), y)
    fm = as.formula("y ~ x2 + x4 + x2:x1 + x2:x3 + x1:x2:x4")
    lrn = lrn("regr.xgboost",
              max_depth = 5,
              eta = 0.5,
              alpha = 2,
              gamma = 3.5,
              nrounds = 500,
              interaction_constraints = "[[0,1], [1,2], [0,1,3]]")
    
    
  } else if(type == "linear_smooth_corr"){
    
    cor_matrix = matrix(c(1,rho,
                          rho,1), nrow = 2, byrow = TRUE)
    
    
    list_distributions = list(function(n) qunif(n, -1, 1), 
                              function(n) qunif(n, -1, 1))
    vars = marginals_copula(cor_matrix, list_distributions, n = n)
    
    x1 = vars[,1]
    x2 = vars[,2]
    x3 = runif(n, -1, 1)
    
    
    
    formula = x1 + 4*x2 + 3*x2*x3 
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    
    data = data.frame(x1, x2, x3, y)
    fm = as.formula("y ~ x1 + x2 + x2:x3")
    lrn = NULL
    
    # noise features
  } else if (type == "linear_smooth_lasso"){
    x1 = runif(n, -1, 1)
    x2 = runif(n, -1, 1)
    x3 = runif(n, -1, 1)
    for(i in 4:10){
      x = runif(n, -1, 1)
      assign(paste0("x",i), x)
    }
    
    
    formula = x1 + 4*x2 + 3*x2*x3 
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    
    data = data.frame(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, y)
    fm = as.formula("y ~ x1 + x2 + x2:x3")
    lrn = NULL
    
    
  } else if(type == "nonlinear_mixed"){
    
    x1 = runif(n, -1, 1)
    x2 = runif(n, -1, 1)
    x3 = runif(n, -1, 1)
    x4 = runif(n, -1, 1)
    x5 = runif(n, -1, 1)
    x6 = as.numeric(rbernoulli(n))
    
    
    formula = x1 + 2*x2^2 + x3*log(abs(x3)) + x4*x5 + x1*x4*ifelse(x6 == 0, 1,0)
    
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    
    data = data.frame(mget(paste0("x",1:6)), y)
    fm = as.formula("y ~ x1 + x2 + x3 + ti(x2, x3) + ti(x1,x2,x4)")
    lrn = lrn("regr.xgboost",
              max_depth = 4,
              eta = 0.825,
              alpha = 0.75,
              gamma = 1,
              nrounds = 700,
              interaction_constraints = "[[3,4],[0,3,5]]")
    
  # Selection Bias
  } else if (type == "selection_bias_independence"){
    x1 = runif(n, 0, 1)
    x2 = runif(n, 0, 1)
    x3 = sample(seq(0,1,0.1), size = n, replace = TRUE)  
    x4 = as.factor(sample(1:2, n, replace = TRUE))
    x5 = as.factor(sample(1:5, n, replace = TRUE))
    x6 = as.factor(sample(1:8, n, replace = TRUE))
    
    y = rnorm(n, 0, 1)
    data = data.frame(x1, x2, x3, x4, x5, x6, y)
    fm = NULL
    lrn = NULL

  } else if (type == "selection_bias_independence_small"){
    
    x1 = runif(n, 0, 1)
    x2 = runif(n, 0, 1)
    x3 = sample(seq(0, 1, 0.1), size = n, replace = TRUE)  
    x4 = sample(seq(0, 1, 0.01), size = n, replace = TRUE)   
    
    y =  rnorm(n, 0, 1)
    data = data.frame(x1, x2, x3, x4, y)
    fm = NULL
    lrn = NULL

    
    # guide replication
  } else if (type == "selection_bias_guide"){
    
    x1 = sample(c(-3,-1,1,3), n, replace = TRUE) 
    x2 = rexp(n, 1)
    x3 = rnorm(n) 
    x4 = as.factor(sample(1:5, n, replace = TRUE))
    x5 = as.factor(sample(1:10, n, replace = TRUE))
    
    y =  rnorm(n, 0, 1)
    data = data.frame(x1, x2, x3, x4, x5, y)
    fm = NULL
    lrn = NULL

  } else if (type == "selection_bias_guide_uniform"){
    
    x1 = runif(n, 0, 1)
    x2 = runif(n, 0, 1)
    x3 = runif(n, 0, 1)
    x4 = as.factor(sample(1:5, n, replace = TRUE))
    x5 = as.factor(sample(1:10, n, replace = TRUE))
    
    y =  rnorm(n, 0, 1)
    data = data.frame(x1, x2, x3, x4, x5, y)
    fm = NULL
    lrn = NULL

  } else if (type == "selection_bias_independence_10"){

    x1 = runif(n, 0, 1)
    x2 = runif(n, 0, 1)
    x3 = sample(seq(0, 1, 0.1), size = n, replace = TRUE)  
    y =  rnorm(n, 0, 1)
    data = data.frame(x1, x2, x3, y)
    fm = NULL
    lrn = NULL
    search_space = NULL

  } else if (type == "selection_bias_independence_25"){

    x1 = runif(n, 0, 1)
    x2 = runif(n, 0, 1)
    x3 = sample(seq(0,1,length.out = 26), n, replace = TRUE)
    y =  rnorm(n, 0, 1)
    data = data.frame(x1, x2, x3, y)
    fm = NULL
    lrn = NULL
    search_space = NULL

  } else if (type == "selection_bias_independence_50"){

    x1 = runif(n, 0, 1)
    x2 = runif(n, 0, 1)
    x3 = sample(seq(0,1,length.out = 51), n, replace = TRUE)
    y =  rnorm(n, 0, 1)
    data = data.frame(x1, x2, x3, y)
    fm = NULL
    lrn = NULL
    search_space = NULL

  } else if (type == "selection_bias_independence_100"){

    x1 = runif(n, 0, 1)
    x2 = runif(n, 0, 1)
    x3 = sample(seq(0, 1, 0.01), size = n, replace = TRUE) 
    y =  rnorm(n, 0, 1)
    data = data.frame(x1, x2, x3, y)
    fm = NULL
    lrn = NULL
    search_space = NULL

    
    # selection bias / splitting strategy interactions
  } else if (type == "selection_bias_interaction_numerical_vs_numrical"){
    x1 = runif(n, 0, 1)
    x2 = runif(n, 0, 1)
    x3 = sample(seq(0, 1, 0.1), size = n, replace = TRUE) 
    x4 = runif(n, 0, 1)
    formula = ifelse(x1 <= mean(x1), x2, 0) + ifelse(x3 <= mean(x3), x4, 0)
    
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    data = data.frame(x1, x2, x3, x4, y)
    fm = NULL
    lrn = NULL
    
  } else if (type == "selection_bias_interaction_numerical_vs_binary"){
    x1 = runif(n, 0, 1)
    x2 = runif(n, 0, 1)
    x3 = as.numeric(rbernoulli(n))
    x4 = runif(n, 0, 1)
    formula = ifelse(x1 <= 0.5, x2, 0) + ifelse(x3 == 0, x4, 0)
    
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    data = data.frame(x1, x2, x3, x4, y)
    fm = NULL
    lrn = NULL
    
  } else if (type == "selection_bias_interaction_numerical_vs_categorical"){
    x1 = runif(n, 0, 1)
    x2 = runif(n, 0, 1)
    x3 = as.factor(sample(1:6, n, replace = TRUE))
    x4 = runif(n, 0, 1)
    formula = ifelse(x1 <= 0.5, x2, 0) + ifelse(x3 %in% 1:3, x4, 0)
    
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    data = data.frame(x1, x2, x3, x4, y)
    fm = NULL
    lrn = NULL
    
  } else if (type == "selection_bias_interaction_binary_vs_categorical"){
    x1 = as.numeric(rbernoulli(n))
    x2 = runif(n, 0, 1)
    x3 = as.factor(sample(1:6, n, replace = TRUE))
    x4 = runif(n, 0, 1)
    formula = ifelse(x1 == 0, x2, 0) + ifelse(x3 %in% 1:3, x4, 0)
    
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    data = data.frame(x1, x2, x3, x4, y)
    fm = NULL
    lrn = NULL
  }
  


  return(list(data = data, fm = fm, lrn = lrn))
}
