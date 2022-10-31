# build trees based on data from slim 3.2
load(file = "Data/simulations/xgboost_slim.RData")

source("R/load_packages.R")
source("R/tree_splitting_slim.R")
source("R/helper_general.R")
n = nrow(X_slim)
data_slim_pred = cbind(y_pred_slim, X_slim)


#### Splines #####
# Vergleiche MOB, CTree und SLIM für LM mit Basissplines 
tree_slim_bspline = compute_tree_slim(y_pred_slim, X_slim, n.split = 3, fit.bsplines = TRUE, impr.par = 0.05, min.split = n/100, pruning = "forward",
                               approximate = FALSE, df.spline = 15)
splitting = extract_split_criteria(tree_slim_bspline)
models = extract_models(tree_slim_bspline)
x_slim_pred = predict_slim(tree_slim_bspline, newdata = X_slim)
View(cbind(x_slim_pred$y_hat,y_pred_slim))
rss = sum((x_slim_pred$y_hat - y_pred_slim)^2)

tree_slim_bspline_approx = compute_tree_slim(y_pred_slim, X_slim, n.split = 3, fit.bsplines = TRUE, impr.par = 0.05, min.split = n/100, pruning = "forward",
                                      approximate = TRUE, df.spline = 15)

# Findet der approximative splitting algorithmus die selben Splits?
res_slim_bspline = extract_split_criteria(tree_slim_bspline)
res_slim_bspline_approx = extract_split_criteria(tree_slim_bspline_approx)


-# MOB
model_lm = function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
  lm(y ~ x , ...)
}

# create formula
fm_bspline = function(x, targetname, df){
  term = c()
  for (n in names(x)){
    if (is.numeric(x[,n])){
      newterm = paste0("bs(", n, ", df = ", df, ", degree = 1)")
    } else {
      newterm = n
    }
    term = c(term, newterm)
  }
  fm = paste(targetname,"~", paste(term, collapse = "+"))
  return(fm)
}


fm_slim_bspline = as.formula(paste(fm_bspline(X_slim, "y_pred_slim", df = 15), "|", paste(colnames(X_slim), collapse= "+")))

# run MOB
mob_slim_bspline = mob(fm_slim_bspline, data = data_slim_pred, fit = model_lm, control = mob_control(maxdepth = 4))

library("strucchange")
sctest(mob_slim_bspline, node = 5)


# run CTree
ctree_slim_bspline = partykit::ctree(fm_slim_bspline, 
                                     data = data_slim_pred, 
                                     ytrafo = model_lm,
                                     control = partykit::ctree_control(maxdepth = 3))
sctest(ctree_slim_bspline, node = 9)

save(ctree_slim_bspline, mob_slim_bspline, tree_slim_bspline, file = "Data/simulations/MOB_CTree_SLIM/bsplines_slim.RData")



##### Polynomial #####
tree_slim_poly = compute_tree_slim(y_pred_slim, X_slim, n.split = 2, degree.poly = 3, impr.par = 0.05, min.split = n/100, pruning = "forward",
                                   approximate = FALSE)
res_slim_poly = extract_split_criteria(tree_slim_poly)



fm_poly = function(x, targetname, degree.poly){
  poly = c()
  numeric.names = c()
  if (degree.poly > 1) {
    numeric.names = names(x)[sapply(x,function(xval)(is.numeric(xval) & !is.integer(xval)))]
    poly = paste0("poly(",numeric.names, ", degree =", degree.poly,")")
  }
  fm = paste(targetname, "~", paste(c(names(x)[!(names(x) %in% numeric.names)], poly), collapse = "+"))
}

fm_slim_poly = as.formula(paste(fm_poly(X_slim, "y_pred_slim", degree.poly = 3), "|", paste(colnames(X_slim), collapse= "+")))



# run MOB
start = Sys.time()
mob_slim_poly = mob(fm_slim_poly, data = data_slim_pred, fit = model_lm, control = mob_control(maxdepth = 3))
print(paste("mob_slim:", Sys.time() - start))

# run CTree
ctree_slim_poly = partykit::ctree(fm_slim_poly, 
                                  data = data_slim_pred, 
                                  ytrafo = model_lm,
                                  control = partykit::ctree_control(maxdepth = 3))
save(ctree_slim_poly, mob_slim_poly, tree_slim_poly, file = "Data/simulations/MOB_CTree_SLIM/poly_slim.RData")


###### GAM #######

# Vergleiche MOB, CTree und SLIM für LM mit Basissplines 

tree_slim_gam = compute_tree_slim(y_pred_slim, X_slim, n.split = 2, fit.gam = TRUE, degree.poly = 1, impr.par = 0.05, min.split = round(n/100))
res_slim_gam = extract_split_criteria(tree_slim_gam)

# MOB using package gamtree
library("devtools")
install_github("marjoleinF/gamtree")
library("gamtree")

gamtree(as.formula(paste("y_pred_slim ~", paste(colnames(x_slim), collapse= "+"), "|", paste(colnames(x_slim), collapse= "+"))))


# MOB
model_gam = function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
  print(colnames(x))
  features_numeric = c("mnth", "hr", "temp", "atemp", "hum", "windspeed")
  x_numeric = x[, colnames(x) %in% features_numeric]
  x_factor = x[, !(colnames(x) %in% features_numeric)]
  
  spline.matrix = c()
  
  for(feature in colnames(x_numeric)){
    new.spline.matrix = s(x_numeric[,feature])
    print(head(new.spline.matrix))
    # colnames(new.spline.matrix) = paste0(feature, "_s_", colnames(new.spline.matrix))
    spline.matrix = cbind(spline.matrix,new.spline.matrix)
  }
  x = cbind(x_factor,spline.matrix)
  gam(y ~ x , ...)
}

# create formula
fm_gam = function(x, targetname){
  term = c()
  for (n in names(x)){
    # if (is.numeric(x[,n])){
    #   newterm = paste0("s(", n, ")")
    # } else {
    newterm = n
    # }
    term = c(term, newterm)
  }
  fm = paste(targetname,"~", paste(term, collapse = "+"))
  return(fm)
}

data_slim_pred = cbind(y_pred_slim, x_slim)

fm_slim_gam = as.formula(paste(fm_gam(x_slim, "y_pred_slim"), "|", paste(colnames(x_slim), collapse= "+")))

# run MOB
mob_slim_gam = mob(fm_slim_gam, data = data_slim_pred, fit = model_gam, control = mob_control(maxdepth = 3))

# run CTree
ctree_slim = partykit::ctree(fm_slim_gam, 
                             data = data_slim_pred, 
                             ytrafo = model_gam,
                             control = partykit::ctree_control(maxdepth = 3))






