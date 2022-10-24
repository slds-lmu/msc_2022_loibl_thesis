# Lasso with MOB
source("R/load_packages.R")
source("R/tree_splitting_slim.R")
source("R/helper_general.R")

n = 1000
x1 = runif(n, -1, 1)
x2 = runif(n, -1, 1)
x3 = rnorm(n)
x4 = rnorm(n)
x5 = rnorm(n)

eps = rnorm(n)

# Data generation process enthält einen Interaktionsterm

# y = 1 + x1 + 2*x2 + 2*ifelse(x1<0, x2,0) + eps
y = 1 + x1 + 2*x2 + x2*x2 + 2*ifelse(x1<0, x2,0) + eps


x = data.frame(x1,x2,x3,x4,x5)
data = as.data.frame(cbind(y,x))




fit_lasso = function(y, x, start = NULL, weights, offset, estfun = TRUE, object = FALSE, ...) {
  if (is.null(weights)) weights <- rep(1, NROW(y))
  n = nrow(x)
  y = unlist(y)
  x = as.matrix(x)
  
  # lambda = cv.glmnet(x, y, alpha = 1, weights = weights)$lambda.min

  fit<-glmnet(x,y,nlambda=100)
  RSS<-deviance(fit)
  BIC <- n*log(RSS/n) + log(n)*fit$df
  lambda<-fit$lambda[which.min(BIC)]
  
  # lambda_perm<-rep(NA,100)
  # for(i in 1:100){
  #   lambda_perm[i]<- (1/n)* max( abs( t(x)%*%sample(y) ) )
  # }
  # lambda = median(lambda_perm)
  
  model = glmnet(x, y, alpha = 1, lambda = lambda, weights = weights)
  
  y_hat = predict.glmnet(model, newx = x, s = lambda)
  residuals = as.vector(y - y_hat)
  coef = coef(model)
  obj = sum(residuals^2) + lambda * glmnet:::pen_function(coef[-1], 1, 1)
  
  # Use a subgradient as score
  
  s = residuals*cbind(1,x) - lambda*matrix(rep(c(0,sign(coef[-1])), length(residuals)), nrow = length(residuals))
  # s = s[, !(as.vector(coef)) == 0]
  
  # For MOB scores must fluctuate around 0 -> substract means
  s = apply(s, 2, function(col){
    score = col - mean(col)
  })
  
  estfun <- matrix(0, nrow = length(weights), ncol = NCOL(s))
  estfun[weights > 0, ] <- s
  
  return(list(estfun = estfun,
              coefficients = coef,
              objfun = obj,
              object = model))
  
  # return(list(estfun = estfun,
  #             converged = TRUE))
}


mob = mob(y ~ 0 + x1 + x2 + x3 + x4 + x5 |x1 + x2 + x3 + x4 + x5,
          data = data, fit = fit_lasso, mob_control = mob_control(maxdepth = 2))

ct = ctree(y ~ x1 + x2 + x3 + x5 + x5 |x1 + x2 + x3 + x5 + x5,
           data = data, ytrafo = fit_lasso)

slim = compute_tree_slim(y, x, n.split = 2, impr.par = 0.05, penalization = "L1", min.split = n/50, pruning = "forward",
                         approximate = FALSE)
extract_split_criteria(slim)
