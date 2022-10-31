library(partykit)
library(splines)

# generate nonlinear data
n = 1000

for(i in 1:5){
  x = runif(n, -1, 1)
  assign(paste0("x",i), x)
}
formula = 3*x1 + x2^3 - pi^x3 + exp(-2*x4^2) +
  2*ifelse(x2>0, 1, 0)*x3 + 2*ifelse(x1>0, 1, 0)*x4 
eps = rnorm(n, 0, 0.5)
y =  formula + eps

x = data.frame(mget(paste0("x",1:5)))
data = data.frame(x, y)




# create tree with lmtree
## define formula
df = 5
term = c()
for (n in names(x)){
  if (is.numeric(x[,n])){
    newterm = paste0("bs(", n, ", df = ", df, ", degree = 1)")
  } else {
    newterm = n
  }
  term = c(term, newterm)
}

fm_spline = as.formula(paste("y~", paste(term, collapse = "+"), "|", paste(names(x), collapse = "+")))

lmtree_spline = lmtree(fm_spline, data = data, maxdepth = 2, alpha = 0.5)



# create tree with mob
## define fit function
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

fm_mob_spline = as.formula(paste("y~", paste(names(x), collapse = "+"), "|", paste(names(x), collapse = "+")))

mobtree = partykit::mob(fm_mob_spline, data = data, fit = fit_spline, control = mob_control(maxdepth = 2, alpha = 0.5), degree = 1, df = 5)

