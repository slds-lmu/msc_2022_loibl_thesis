# Lasso with MOB
source("R/load_packages.R")
source("R/tree_splitting_slim.R")
source("R/helper_general.R")
source("R/mob_fitting_functions.R")

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


mob = mob(y ~ 0 + x1 + x2 + x3 + x4 + x5 |x1 + x2 + x3 + x4 + x5,
          data = data, fit = fit_lasso, mob_control = mob_control(maxdepth = 2))

ct = ctree(y ~ x1 + x2 + x3 + x5 + x5 |x1 + x2 + x3 + x5 + x5,
           data = data, ytrafo = fit_lasso)

slim = compute_tree_slim(y, x, n.split = 2, impr.par = 0.05, penalization = "L1", min.split = n/50, pruning = "forward",
                         approximate = FALSE)
extract_split_criteria(slim)
