create_sim_data = function(job, n = 1000, type, ...){
  
  if (type == "basic_linear_smooth"){
    
    x1 = runif(n, -1, 1)
    x2 = runif(n, -1, 1)
    x3 = runif(n, -1, 1)
    x4 = runif(n, -1, 1)
    x5 = runif(n, -1, 1)
    x6 = rnorm(n, 0, 2)
    x7 = rnorm(n, 2, 3)
    
    formula = x1 + 4*x2 + 3*x2*x3 + 4*x4*x5 + x5
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    
    data = data.frame(x1, x2, x3, x4, x5, x6, x7, y)
    fm = as.formula("y ~ x1 + x2 + x5 + x2:x3 + x4:x5")
    lrn = lrn("regr.xgboost",
              max_depth = 8,
              eta = 1,
              alpha = 0.1,
              gamma = 5)
    
    search_space = ps(
      max_depth = p_int(lower = 4, upper = 8),
      eta = p_dbl(lower = 0.5, upper = 1),
      alpha = p_dbl(lower = 0, upper = 2),
      gamma = p_dbl(lower = 1, upper = 5)
    )
    
  } else if (type == "basic_linear_abrupt"){
    
    x1 = runif(n, -1, 1)
    x2 = runif(n, -1, 1)
    x3 = runif(n, -1, 1)
    x4 = runif(n, -1, 1)
    x5 = runif(n, -1, 1)
    x6 = rnorm(n, 0, 2)
    x7 = rnorm(n, 2, 3)
    
    formula = x1 + 4*x2 + 3*x2*x3 + 5*x2*ifelse(x4>0,1,0) + x3*x5 + x5
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    
    data = data.frame(x1, x2, x3, x4, x5, x6, x7, y)
    fm = as.formula("y ~ x1 + x2 + x2:x3 + ti(x2,x4) + ti(x3,x5) + x5")
    lrn = lrn("regr.xgboost",
              max_depth = 7,
              eta = 1,
              alpha = 0.15,
              gamma = 2)
    
    search_space = ps(
      max_depth = p_int(lower = 4, upper = 8),
      eta = p_dbl(lower = 0.5, upper = 1),
      alpha = p_dbl(lower = 0, upper = 2),
      gamma = p_dbl(lower = 1, upper = 5)
    )
    
  } else if(type == "categorical_linear"){
    
    x1 = runif(n, -1, 1)
    x2 = runif(n, -1, 1)
    x3 = as.numeric(rbernoulli(n))
    x4 = as.numeric(rbernoulli(n))
    x5 = as.numeric(rbernoulli(n))
    x6 = rnorm(n, mean = 1, sd = 5)
    
    formula = 0.2*x1 - 8*x2 + ifelse(x3 == 0, I(16*x2),0) + ifelse(x1 > mean(x1), I(8*x2),0) 
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    
    data = data.frame(x1, x2, x3, x4, x5, x6, y)
    fm = as.formula("y ~ x1 + x2 + x2:x3 + ti(x1,x2)")
    lrn = lrn("regr.xgboost"
              ,
              max_depth = 4,
              eta = 1,
              alpha = 1.5,
              gamma = 5)
    
    search_space = ps(
      max_depth = p_int(lower = 4, upper = 6),
      eta = p_dbl(lower = 0.5, upper = 1),
      alpha = p_dbl(lower = 0, upper = 2),
      gamma = p_dbl(lower = 1, upper = 5)
    )
    
  } else if(type == "linear_mixed"){
    
    x1 = runif(n, -1, 1)
    x2 = runif(n, -1, 1)
    x3 = as.numeric(rbernoulli(n))
    x4 = as.numeric(rbernoulli(n))
    x5 = as.numeric(rbernoulli(n))
    for(i in 6:20){
      x = runif(n, -1, 1)
      assign(paste0("x",i), x)
    }
    
    
    formula = 4*x2 + 2*x4 + 2*x6 + 2*x8 + 4*x2*x1 + ifelse(x3 == 0, I(8*x2),0) + 
      ifelse(x5 == 1, I(10*x2),0)*x6 + 8*x2*x7 + 3*x1*x3 + 3*x8*x10 + 3*x7*x9
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    
    data = data.frame(mget(paste0("x",1:20)), y)
    fm = as.formula("y ~ x2 + x4 + x6 + x8 + x2:x1 + x3:x2 + x5:x2:x6 + x2:x7 + x1:x3 + x8:x10 + x7:x9")
    lrn = lrn("regr.xgboost",
              max_depth = 8,
              eta = 1,
              alpha = 0.02,
              gamma = 3)
    
    search_space = ps(
      max_depth = p_int(lower = 4, upper = 10),
      eta = p_dbl(lower = 0.5, upper = 1),
      alpha = p_dbl(lower = 0, upper = 2),
      gamma = p_dbl(lower = 1, upper = 5)
    )
    
  } else if(type == "mixed_large"){
    
    # numeric
    for(i in 1:10){
      x = runif(n, -1, 1)
      assign(paste0("x",i), x)
    }
    # binary
    for(i in 11:15){
      x = as.factor(sample(c(0, 1), size = n, replace = TRUE, prob = c(0.5, 0.5)))
      assign(paste0("x",i), x)
    }
    # multiple categories
    x16 = as.factor(sample(c(1:3), size = n, replace = TRUE, prob = c(0.2, 0.4, 0.3)))
    x17 = as.factor(sample(c(1:4), size = n, replace = TRUE, prob = c(0.2, 0.2, 0.3, 0.3)))
    x18 = as.factor(sample(c(1:4), size = n, replace = TRUE, prob = c(0.1, 0.1, 0.1, 0.7)))
    x19 = as.factor(sample(c(1:8), size = n, replace = TRUE, prob = c(rep(0.1,6), 0.2, 0.2)))
    
    
    formula = 4*x2 - 4*x4 + 4*x6 - 4*x8 + 4*x10 + 3*x2*x1 + 5*x2*x5 + 7*x2*x8 + ifelse(x13 == 0, I(8*x2),0) + 
      ifelse(x16 == 1, I(4*x2),0) + ifelse(x16 == 2, I(6*x2),0) + ifelse(x19 %in% c(1,2,3), I(4*x2),0) + ifelse(x19 %in% c(4:6), I(6*x2),0) + 
      3*x1*x3 + 3*x8*x10 + 3*x7*x9
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    
    data = data.frame(mget(paste0("x",1:19)), y)
    fm = as.formula("y ~ x2 + x4 + x6 + x8 + x10 + x2:x1 + x2:x5 + x2:x8 + x13:x2 + x16:x4 + x16:x2 + x19:x2 + x9:x2 +
                    x1:x3 + x8:x10 + x7:x9")
    
    task = as_task_regr(x = data, target = "y")
    
    lrn =  lrn("regr.xgboost",
               max_depth = 9,
               eta = 1,
               alpha = 0.3,
               gamma = 3.5)           
    
    fencoder = po("encode", method = "treatment", affect_columns = selector_type("factor"))
    fencoder$train(list(task))
    graph = fencoder %>>% lrn
    lrn = as_learner(graph)
    
    
    search_space = ps(
      regr.xgboost.max_depth = p_int(lower = 4, upper = 12),
      regr.xgboost.eta = p_dbl(lower = 0.5, upper = 1),
      regr.xgboost.alpha = p_dbl(lower = 0, upper = 2),
      regr.xgboost.gamma = p_dbl(lower = 1, upper = 5)
    )
    
  } else if(type == "nonlinear"){ 
    
    for(i in 1:10){
      x = runif(n, -1, 1)
      assign(paste0("x",i), x)
    }
    
    formula = 6*x1 + x2^2 - (pi)^(x3) + exp(-2*(x4)^2) + 1/(2+abs(x5)) + x6*log(abs(x6)) + 
      2*ifelse(x1 > 0, 1,0)*ifelse(x2 > 0, 1,0)*x3 + 2*ifelse(x4 > 0, 1,0)*x2 + 4*(x2*ifelse(x2 > 0, 1,0))^(abs(x6)) + abs(x2 + x8)
    eps = rnorm(n, 0, 0.5)
    y =  formula + eps
    
    data = data.frame(mget(paste0("x",1:10)), y)
    fm = NULL
    lrn = NULL
    search_space = NULL
  } else   if (type == "slim2"){
    for(i in 1:10){
      x = runif(n, -1, 1)
      assign(paste0("x",i), x)
    }
    formula = 3*x1 + x2^3 - pi^x3 + exp(-2*x4^2) + 1/(2+abs(x5)) + x6 * log(abs(x6)) + sqrt(2*abs(x7)) + max(0, x7) + x8^4 + 2*cos(pi*x8) +
      2*ifelse(x1>0, 1, 0)*ifelse(x2>0, 1, 0)*x3 + 2*ifelse(x1>0, 1, 0)*x4 + 4*(x5*ifelse(x5>0, 1, 0))^(abs(x6)) + abs(x7 + x8)
    eps = rnorm(n, 0, 0.5)
    y =  formula + eps
    
    data = data.frame(mget(paste0("x",1:10)), y)
    fm = NULL
    lrn = NULL
    search_space = NULL
    
  } else if (type == "selection_bias_independence"){
    x1 = runif(n, 0, 1)
    x2 = runif(n, 0, 1)
    x3 = round(runif(n, 0, 1), 1)  
    x4 = as.factor(sample(1:2, n, replace = TRUE))
    x5 = as.factor(sample(1:5, n, replace = TRUE))
    x6 = as.factor(sample(1:8, n, replace = TRUE))
    
    y = rnorm(n, 0, 1)
    data = data.frame(x1, x2, x3, x4, x5, x6, y)
    fm = NULL
    lrn = NULL
    search_space = NULL
    
  } else if (type == "selection_bias_independence_small"){
    
    x1 = runif(n, 0, 1)
    x2 = runif(n, 0, 1)
    x3 = round(runif(n, 0, 1), 1)  
    x4 = round(runif(n, 0, 1), 2) 
    
    y =  rnorm(n, 0, 1)
    data = data.frame(x1, x2, x3, x4, y)
    fm = NULL
    lrn = NULL
    search_space = NULL
    
  } else if (type == "selection_bias_guide"){
    
    x1 = runif(n, 1, 3)
    x2 = rexp(n, 1)
    x3 = rnorm(n)  
    x4 = as.factor(sample(1:5, n, replace = TRUE))
    x5 = as.factor(sample(1:8, n, replace = TRUE))
    
    y =  rnorm(n, 0, 1)
    data = data.frame(x1, x2, x3, x4, x5, y)
    fm = NULL
    lrn = NULL
    search_space = NULL
    
  } else if (type == "selection_bias_interaction"){
    
    x1 = runif(n, 0, 1)
    x2 = runif(n, 0, 1)
    x3 = round(runif(n, 0, 1), 1)  
    x4 = round(runif(n, 0, 1), 2) 
    formula = x1 + x2 + x3 + x4 + x1*x2 + x3*x4
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    data = data.frame(x1, x2, x3, x4, y)
    fm = NULL
    lrn = NULL
    search_space = NULL
    
  } else if (type == "selection_bias_full_interaction"){
    
    x1 = runif(n, 0, 1)
    x2 = runif(n, 0, 1)
    x3 = round(runif(n, 0, 1), 1)  
    x4 = round(runif(n, 0, 1), 2) 
    formula = x1 + x2 + x3 + x4 + x1*x2 + x1*x3+ x1*x4 + x2*x3 + x2*x4 + x3*x4
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    data = data.frame(x1, x2, x3, x4, y)
    fm = NULL
    lrn = NULL
    search_space = NULL
    
    
  } else if (type == "selection_bias_interaction_binary"){
    
    x1 = runif(n, 0, 1)
    x2 = as.factor(rbinom(n, 1, 0.5))
    formula = x1 + as.numeric(x2) + ifelse(x2 == 1, x1, 0)
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    data = data.frame(x1, x2, y)
    fm = NULL
    lrn = NULL
    search_space = NULL
    
    
  } else if (type == "selection_bias_interaction_categorical"){
    
    x1 = runif(n, 0, 1)
    x2 = as.factor(sample(1:4, n, replace = TRUE))
    formula = x1 + ifelse(x2 %in% c(2,3), 1, 0)  + ifelse(x2 == 2, x1 ,0) 
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    data = data.frame(x1, x2, y)
    fm = NULL
    lrn = NULL
    search_space = NULL
    
    
  } else if (type == "selection_bias_interaction_binary_categorical"){
    
    x1 = as.factor(sample(1:2, n, replace = TRUE))
    x2 = as.factor(sample(1:4, n, replace = TRUE))
    
    # formula = ifelse(x1 == 2, 1,0) + ifelse(x2 %in% c(2,3), 1, 0)  + ifelse(x1 == 2 & x2 == 2, 2 ,
    #                                                                         ifelse(x1 == 2 & x2 == 3, 3 ,
    #                                                                                ifelse(x1 == 2 & x2 == 4, 4 ,0))) 
    formula = ifelse(x1 == 2, 1,0) + ifelse(x2 %in% c(2,3), 1, 0)  + ifelse(x1 == 2 & x2 == 2, 5 ,0) + ifelse(x1 == 1 & x2 == 2, 7 ,0) 
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    data = data.frame(x1, x2, y)
    fm = NULL
    lrn = NULL
    search_space = NULL
    
    
  }

  return(list(data = data, fm = fm, lrn = lrn, search_space = search_space))
}
