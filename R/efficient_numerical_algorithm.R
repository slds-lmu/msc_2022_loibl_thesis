# Test efficient numerical alogorithm

source("R/load_packages.R")
source("R/tree_splitting_slim.R")
source("R/helper_general.R")
source("R/simulations/simulation_setting_definition.R")

set.seed(1234)

n = 100000
data_linear = create_sim_data(job = NULL, n = n, type = "numeric_linear")
X_linear = data_linear[which(names(data_linear)!="y")]
y_linear = data_linear$y 


tree_approx = compute_tree_slim(y_linear, X_linear, n.split = 3, fit.bsplines = FALSE, impr.par = 0.05, min.split = n/100, pruning = "forward",
                                approximate = TRUE, df.spline = 15)
res_approx = extract_split_criteria(tree_approx)

tree_exact = compute_tree_slim(y_linear, X_linear, n.split = 3, fit.bsplines = FALSE, impr.par = 0.05, min.split = n/100, pruning = "forward",
                               approximate = FALSE, df.spline = 15)
res_exact = extract_split_criteria(tree_exact)

test_efficient_algorithm = function(x, y, xval, n.quantiles = 100, fit.bsplines = FALSE, min.node.size = 200, penalization = NULL, df.spline = 15){
  # calculate sse through closed form
  gram.list = create_gramlist(x = x, y = y, bin = rep(1, nrow(x)), fit.bsplines = fit.bsplines, penalization = penalization, df.spline = df.spline)
  sse.closed.form = perform_gram_splits(gram.list, x = x, min.node.size = min.node.size, penalization = penalization)
  q = generate_split_candidates(xval, n.quantiles = n.quantiles, min.node.size = min.node.size)
  node.number = findInterval(x = xval, vec = q, rightmost.closed = TRUE) + 1
  gram.list = create_gramlist(x = x, y = y, bin = node.number, fit.bsplines = fit.bsplines, penalization = penalization, df.spline = df.spline)
  sse.gram = perform_gram_splits(gram.list, x = x, min.node.size = min.node.size, penalization = NULL, lambda = 0, include.parent.sse = TRUE)
  
  return(list(sse.closed.form = sse.closed.form, sse.parent.gram = sse.gram$parent.sse, sse.gram.min = min(sse.gram$splits)))
}

test.gram.bsplines = test_efficient_algorithm(x = X_slim, y = y_pred_slim, xval = X_slim$x4, fit.bsplines = TRUE, n.quantiles = 2, df.spline = 2)
# -> Wenn keine invalid Spalten vorhanden sind funktioniert b spline generell
lm_sse = get_objective_bspline(x = X_slim, y = y_pred_slim)
