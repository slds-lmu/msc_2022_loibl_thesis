# fitting/trafo functions for mob and ctree


fit_lm = function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
  lm(y ~ x , ...)
}

fit_spline = function(y, x, start = NULL, weights = NULL, offset = NULL, degree = 1, df = 5,  ..., estfun = TRUE, object = TRUE) {
  term = c()
  for (n in colnames(x)[-1]){
    if (is.numeric(x[,n])){
      newterm = paste0("bs(", n, ", df =", df, ", degree =",  degree, ")")
    } else {
      newterm = n
    }
    term = c(term, newterm)
  }
  
  fm_spline = as.formula(paste("y~", paste(term, collapse = "+")))
  
  model = lm(fm_spline, data = data.frame(y,x), weights = weights, offset = offset)
  return(list(coefficients = coef(model),
              objfun = sum(model$residuals**2),
              estfun = sandwich::estfun(model),
              object = model))
}


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
