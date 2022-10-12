source("R/load_packages.R")
source("R/tree_splitting_slim.R")
source("R/helper_general.R")
source("R/simulations/simulation_setting_definition.R")

# Vergleich von MOB und SLIM
# Simulationsdaten aus dem SLIM Paper

n = 100000

data_slim = create_sim_data(nob = NULL, n = n, type = "slim2")
x = data_slim[which(names(data_slim)!="y")]
y = data_slim$y 

# Polynomiale Regression

# degree 1
degree.poly = 3
poly = c()
if (degree.poly > 1) {
  numeric.names = names(x)[sapply(x,function(xval)(is.numeric(xval) & !is.integer(xval)))]
  for(d in 2:degree.poly){
    poly = c(poly, paste0(numeric.names, "^", degree.poly))
  }
}
fm_poly = as.formula(paste("y ~", paste(c(names(x), poly), collapse = "+"), "|", paste(names(x), collapse = "+")))

start.time = Sys.time()
lmtree_poly = lmtree(fm_poly, data = cbind(x,y), maxdepth = 4, verbose = TRUE, restart = FALSE)
end.time = Sys.time()
print(end.time - start.time)
print(lmtree_poly)

slimtree_poly = compute_tree_slim(y, x, n.split = 3, use.bsplines = FALSE, degree.poly = 3, impr.par = 0.1, min.split = max(n/100,20), pruning = "forward")
res_slim_poly = extract_split_criteria(slimtree_poly)

save(slimtree_poly, lmtree_poly, file = "R/simulation_results/slim_poly_mob.RData")


df = 15
term = c()
for (n in names(x)){
  if (is.numeric(x[,n])){
    newterm = paste0("bs(", n, ", df = ", df, ", degree = 2)")
  } else {
    newterm = n
  }
  term = c(term, newterm)
}

fm_spline = as.formula(paste("y~", paste(term, collapse = "+"), "|", paste(names(x), collapse = "+")))

start.time = Sys.time()
lmtree_spline = lmtree(fm_spline, data = cbind(x,y), maxdepth = 4)
end.time = Sys.time()
print(end.time - start.time)


tree_slim_splines = compute_tree_slim(y_slim, X_slim, n.split = 3, use.bsplines = TRUE, df.spline = 15, impr.par = 0.1, min.split = max(n/100,20), pruning = "forward")
res_slim_splines = extract_split_criteria(tree_slim_splines)
models_slim_splines = extract_models(tree_slim_splines)

# save(lmtree_spline, tree_slim_splines, file = "R/simulation_results/slim_spline_mob.RData")
load("R/simulation_results/slim_spline_mob.RData")
load("R/simulation_results/slim_poly_mob.RData")
