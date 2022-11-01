# define different simulation settings for slim

create_sim_data_slim = function(n, type, ...){
  
  if (type == "basic_linear_smooth"){
    
    x1 = runif(n, -1, 1)
    x2 = runif(n, -1, 1)
    x3 = runif(n, -1, 1)
    x4 = runif(n, -1, 1)
    x5 = runif(n, -1, 1)
    x6 = rnorm(n, 0, 2)
    x7 = rnorm(n, 2, 3)
    
    formula = x1 + 4*x2 + 3*x2*x3 + 5*x2*x4 + x5
    eps = rnorm(n, 0, sd(formula)*0.1)
    y =  formula + eps
    
    data = data.frame(x1, x2, x3, x4, x5, x6, x7, y)
    fm = as.formula("y ~ x1 + x2 + x2:x3 + x2:x4 + x5")
    
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
    fm = as.formula("y ~ x2 + x4 + x6 + x8 + x2:x1 + x3*x2 + x5*x2*x6 + x2*x7 + x1*x3 + x8*x10 + x7*x9")
    
  } else if (type == "selection_bias"){
    
    x1 = runif(n, 0, 1)
    x2 = runif(n, 0, 1)
    x3 = round(runif(n, 0, 1), 1)  
    x4 = as.factor(sample(1:2, n, replace = TRUE))
    x5 = as.factor(sample(1:5, n, replace = TRUE))
    x6 = as.factor(sample(1:8, n, replace = TRUE))
    
    y = rnorm(n, 0, 1)
    data = data.frame(x1, x2, x3, x4, x5, x6, y)
    fm = NULL
  }
  return(list(data = data, fm = fm))
}
