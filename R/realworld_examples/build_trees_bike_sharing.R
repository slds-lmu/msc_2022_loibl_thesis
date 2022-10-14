# build trees based on data from slim 3.2
load(file = "Data/realworld_examples/bikesharing/xgboost_bikesharing.RData")

source("R/load_packages.R")
source("R/tree_splitting_slim.R")
source("R/helper_general.R")
library(strucchange)
n = nrow(x_bike)
data_bike_pred = cbind(y_pred_bike, x_bike)


#### Splines #####
# Vergleiche MOB, CTree und SLIM für LM mit Basissplines 
tree_bike_bspline = compute_tree_slim(y_pred_bike, x_bike, n.split = 2, fit.bspline = TRUE, degree.poly = 1, impr.par = 0.05, min.split = round(n/100), pruning = "forward", df.spline = 25)
res_bike_bspline = extract_split_criteria(tree_bike_bspline)
models = extract_models(tree_bike_bspline)

bike_pred = predict_slim(tree_bike_bspline, newdata = x_bike)
View(cbind(bike_pred$y_hat, y_pred_bike))
rss = sum((bike_pred$y_hat - y_pred_bike)^2)



tree_bike_bspline_approx = compute_tree_slim(y_pred_bike, x_bike, n.split = 2, fit.bspline = TRUE, approximate = TRUE, impr.par = 0.05, min.split = round(n/100), pruning = "forward", df.spline = 25)
res_bike_bspline_approx = extract_split_criteria(tree_bike_bspline_approx)




# MOB
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


fm_bike_bspline = as.formula(paste(fm_bspline(x_bike, "y_pred_bike", df = 25), "|", paste(colnames(x_bike), collapse= "+")))

# run MOB
mob_bike_bspline = mob(fm_bike_bspline, data = data_bike_pred, fit = model_lm, control = mob_control(maxdepth = 3))
p_mob_bspline = sctest(mob_bike_bspline, node = 1)


# run CTree
ctree_bike_bspline = partykit::ctree(fm_bike_bspline, 
                             data = data_bike_pred, 
                             ytrafo = model_lm,
                             control = partykit::ctree_control(maxdepth = 2,
                                                               teststat = "maximum"))
p_ctree_bspline = sctest(ctree_bike_bspline, node = 1)

p_bspline = rbind(p_mob_bspline[2,], p_ctree_bspline[2,])


save(ctree_bike_bspline, mob_bike_bspline, tree_bike_bspline, file = "Data/realworld_examples/bikesharing/bsplines_bike.RData")


##### Polynomial #####

tree_bike_poly = compute_tree_slim(y_pred_bike, x_bike, n.split = 2, degree.poly = 3, impr.par = 0.05, min.split = round(n/100), pruning = "forward",
                                   approximate = FALSE)
res_bike_poly = extract_split_criteria(tree_bike_poly)



fm_poly = function(x, targetname, degree.poly){
  poly = c()
  numeric.names = c()
  if (degree.poly > 1) {
    numeric.names = names(x)[sapply(x,function(xval)(is.numeric(xval) & !is.integer(xval)))]
    poly = paste0("poly(",numeric.names, ", degree =", degree.poly,")")
  }
  fm = paste(targetname, "~", paste(c(names(x)[!(names(x) %in% numeric.names)], poly), collapse = "+"))
}

fm_bike_poly = as.formula(paste(fm_poly(x_bike, "y_pred_bike", degree.poly = 3), "|", paste(colnames(x_bike), collapse= "+")))

# run MOB
start = Sys.time()
mob_bike_poly = mob(fm_bike_poly, data = data_bike_pred, fit = model_lm, control = mob_control(maxdepth = 3))
print(paste("mob_bike:", Sys.time() - start))

# run CTree
ctree_bike_poly = partykit::ctree(fm_bike_poly, 
                                  data = data_bike_pred, 
                                  ytrafo = model_lm,
                                  control = partykit::ctree_control(maxdepth = 2))



###### GAM #######

# Vergleiche MOB, CTree und SLIM für LM mit Basissplines 
x_bike_gam = x_bike
# x_bike_gam$mnth = as.factor(x_bike$mnth)
# x_bike_gam$hr = as.factor(x_bike$hr)
tree_bike_gam = compute_tree_slim(y_pred_bike, x_bike_gam, n.split = 2, fit.gam = TRUE, degree.poly = 1, impr.par = 0.05, min.split = round(n/100))
res_bike_gam = extract_split_criteria(tree_bike_gam)

# MOB using package gamtree
library("devtools")
install_github("marjoleinF/gamtree")
library("gamtree")

gamtree(as.formula(paste("y_pred_bike ~", paste(colnames(x_bike), collapse= "+"), "|", paste(colnames(x_bike), collapse= "+"))))


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

data_bike_pred = cbind(y_pred_bike, x_bike)

fm_bike_gam = as.formula(paste(fm_gam(x_bike, "y_pred_bike"), "|", paste(colnames(x_bike), collapse= "+")))

# run MOB
mob_bike_gam = mob(fm_bike_gam, data = data_bike_pred, fit = model_gam, control = mob_control(maxdepth = 3))

# run CTree
ctree_bike = partykit::ctree(fm_bike_gam, 
                             data = data_bike_pred, 
                             ytrafo = model_gam,
                             control = partykit::ctree_control(maxdepth = 3))





