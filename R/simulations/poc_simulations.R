source("R/load_packages.R")
source("R/tree_splitting_slim.R")
source("R/helper_general.R")
source("R/simulations/simulation_setting_definition.R")

set.seed(1234)
n = 1000

# numeric linear
# y = x1 + 4*x2 + 3*x2*x3 + 5*x2*x4 + 7*x2*x5 + eps

data_linear = create_sim_data(job = NULL, n = n, type = "numeric_linear")
X_linear = data_linear[which(names(data_linear)!="y")]
y_linear = data_linear$y 

tree_linear = compute_tree_slim(y_linear, X_linear, n.split = 7, penalization = NULL, fit.bsplines = TRUE, impr.par = 0.1, min.split = n/100, pruning = "forward",
                                split.method = "slim")
res_linear = extract_split_criteria(tree_linear)

tree_linear_anova = compute_tree_slim(y_linear, X_linear, n.split = 2, penalization = NULL, degree.poly = 1, impr.par = 0.1, min.split = n/100, pruning = "forward",
                                split.method = "anova")

res_linear_anova = extract_split_criteria(tree_linear_anova)

save(tree_linear, res_linear, models_linear, file = "R/simulation_results/linear.RData")

tree_linear_mae = compute_tree_slim(y_linear, X_linear, objective = "MAE",  n.split = 3, penalization = NULL, degree.poly = 1, impr.par = 0, min.split = n/100, pruning = "none")
res_linear_mae = extract_split_criteria(tree_linear_mae)
models_linear_mae = extract_models(tree_linear_mae)

save(tree_linear, res_linear, models_linear, file = "R/simulation_results/linear.RData")

# categorical_linear
# y = 0.2*x1 - 8*x2 + ifelse(x3 == 0, I(16*x2),0) + ifelse(x1 > mean(x1), I(8*x2),0) + eps
data_categorical = create_sim_data(job = NULL, n = n, type = "categorical_linear")
X_categorical = data_categorical[which(names(data_categorical)!="y")]

X_categorical$x3 = as.factor(X_categorical$x3)
X_categorical$x4 = as.factor(X_categorical$x4)
X_categorical$x5 = as.factor(X_categorical$x5)

y_categorical = data_categorical$y 

tree_categorical = compute_tree_slim(y_categorical, X_categorical, n.split = 3, penalization = NULL, degree.poly = 1, impr.par = 0.01, min.split = n/100, pruning = "forward")
res_categorical = extract_split_criteria(tree_categorical)
models_categorical = extract_models(tree_categorical)

tree_categorical_anova = compute_tree_slim(y_categorical, X_categorical, n.split = 3, impr.par = 0.01, min.split = n/100, 
                                           pruning = "forward", split.method = "anova")
res_categorical_anova = extract_split_criteria(tree_categorical_anova)

X_categorical_new = X_categorical
X_categorical_new$x3 = as.factor(X_categorical_new$x3)
X_categorical_new$x4 = as.factor(X_categorical_new$x4)
X_categorical_new$x5 = as.factor(X_categorical_new$x5)

tree_categorical_new = compute_tree_slim(y_categorical, X_categorical_new, n.split = 4, penalization = NULL, degree.poly = 1, impr.par = 0.01, min.split = n/100, pruning = "forward")
res_categorical_new = extract_split_criteria(tree_categorical_new)
models_categorical_new = extract_models(tree_categorical_new)



save(tree_categorical, res_categorical, models_categorical, file = "R/simulation_results/categorical.RData")



# linear mixed
# y = 4*x2 + 2*x4 + 2*x6 + 2*x8 + 4*x2*x1 + ifelse(x3 == 0, I(8*x2),0) + ifelse(x5 == 1, I(10*x2),0)*x6 + 8*x2*x7 + 3*x1*x3 + 3*x8*x10 + 3*x7*x9 + eps
data_linear_mixed = create_sim_data(job = NULL, n = n, type = "linear_mixed")
X_linear_mixed = data_linear_mixed[which(names(data_linear_mixed)!="y")]
y_linear_mixed = data_linear_mixed$y 

tree_linear_mixed = compute_tree_slim(y_linear_mixed, X_linear_mixed, n.split = 4, penalization = NULL, degree.poly = 1, impr.par = 0.1, min.split = n/100, pruning = "forward")
res_linear_mixed = extract_split_criteria(tree_linear_mixed)
models_linear_mixed = extract_models(tree_linear_mixed)

save(tree_linear_mixed, res_linear_mixed, models_linear_mixed, file = "R/simulation_results/linear_mixed.RData")



# mixed large 
# y = 4*x2 - 4*x4 + 4*x6 - 4*x8 + 4*x10 + 3*x2*x1 + 5*x2*x5 + 7*x2*x8 + ifelse(x13 == 0, I(8*x2),0) + 
# ifelse(x16 == 1, I(4*x2),0) + ifelse(x16 == 2, I(6*x2),0) + ifelse(x19 %in% c(1,2,3), I(4*x2),0) + ifelse(x19 %in% c(4:6), I(6*x2),0) + 
#  3*x1*x3 + 3*x8*x10 + 3*x7*x9 + ifelse(x20 == 16, I(4*x5),0) + ifelse(x20 %in% c(1,3,5,7), I(6*x5),0)
data_mixed_large = create_sim_data(job = NULL, n = n, type = "mixed_large")
X_mixed_large = data_mixed_large[which(names(data_mixed_large)!="y")]
y_mixed_large = data_mixed_large$y 


# task_mixed_large = mlr3::as_task_regr(data_mixed_large, target = "y")
# 
# at_mixed_large = auto_tuner(
#   method = "random_search",
#   learner = lrn("regr.xgboost", nrounds=to_tune(1, 150)),
#   resampling = rsmp("cv", folds = 3),
#   measure = msr("regr.mse"),
#   term_evals = 10,
#   batch_size = 5
# )
# 
# 
# # tune hyperparameters and fit final model on the complete data set in one go
# at_mixed_large$train(task_mixed_large)
# 
# prediction_mixed_large = at_mixed_large$predict(task_mixed_large)

tree_mixed_large = compute_tree_slim(y_mixed_large, X_mixed_large, n.split = 4, penalization = NULL, degree.poly = 1, impr.par = 0.1, min.split = n/100, pruning = "forward")
res_mixed_large = extract_split_criteria(tree_mixed_large)
models_mixed_large = extract_models(tree_mixed_large)
save(tree_mixed_large, res_mixed_large, models_mixed_large, file = "R/simulation_results/mixed_large.RData")


# nonlinear
# y = 6*x1 + x2^2 - (pi)^(x3) + exp(-2*(x4)^2) + 1/(2+abs(x5)) + x6*log(abs(x6)) + 
# 2*ifelse(x1 > 0, 1,0)*ifelse(x2 > 0, 1,0)*x3 + 2*ifelse(x4 > 0, 1,0)*x2 + 4*(x2*ifelse(x2 > 0, 1,0))^(abs(x6)) + abs(x2 + x8) +eps
n = 1000
data_nonlinear = create_sim_data(job = NULL, n = n, type = "nonlinear")
X_nonlinear = data_nonlinear[which(names(data_nonlinear)!="y")]
y_nonlinear = data_nonlinear$y 

mod_nonlinear = gam(y~s(x1,x2,x3)+s(x4,x2)+s(x6,x2)+s(x8,x2)+
            s(x1)+s(x2)+s(x3)+s(x4)+s(x5)+s(x6)+s(x7)+s(x8)+s(x9)+s(x10),data=data_nonlinear, method="REML")
y_nonlinear_pred = predict.gam(mod_nonlinear, X_nonlinear)

tree_nonlinear_gam = compute_tree_slim(y_nonlinear_pred, X_nonlinear, n.split = 3, use.bsplines = TRUE, impr.par = 0.1, min.split = 100, pruning = "forward")
res_nonlinear_gam = extract_split_criteria(tree_nonlinear_gam)
models_nonlinear = extract_models(tree_nonlinear)

tree_nonlinear_lasso = compute_tree_slim(y_nonlinear_pred, X_nonlinear, n.split = 2, degree.poly = 3, penalization = "L1", impr.par = 0.001, min.split = 100, pruning = "forward")
res_nonlinear_lasso = extract_split_criteria(tree_nonlinear_lasso)


save(tree_nonlinear, res_nonlinear, models_nonlinear, file = "R/simulation_results/nonlinear.RData")




# slim 
n = 10000
data_slim = create_sim_data(nob = NULL, n = n, type = "slim2")
X_slim = data_slim[which(names(data_slim)!="y")]
y_slim = data_slim$y 

tree_slim_splines = compute_tree_slim(y_slim, X_slim, n.split = 3, use.bsplines = TRUE, impr.par = 0.1, min.split = max(n/100,20), pruning = "forward")
res_slim_splines = extract_split_criteria(tree_slim_splines)
models_slim_splines = extract_models(tree_slim_splines)

tree_slim_lasso = compute_tree_slim(y_slim, X_slim, n.split = 3, use.bsplines = FALSE, penalization = "L1", degree.poly = 3, impr.par = 0.1, min.split = max(n/100,20), pruning = "forward")
res_slim_lasso = extract_split_criteria(tree_slim_lasso)
models_lasso = extract_models(tree_slim_lasso)

tree_slim_lm = compute_tree_slim(y_slim, X_slim, n.split = 3, use.bsplines = FALSE, degree.poly = 3, impr.par = 0.1, min.split = max(n/100,20), pruning = "forward")
res_slim_lm = extract_split_criteria(tree_slim_lm)
models_lm = extract_models(tree_slim_lm)

save(tree_slim_splines, tree_slim_lasso, tree_slim_lm, file = "R/simulation_results/splines_vs_poly.RData")
load("R/simulation_results/splines_vs_poly.RData")
# pdp plots für slim 1

partial(models_slim1[["1_rootnode_0"]][["model"]], pred.var = "x4",  train = data_slim1, plot = TRUE)
partial(models_slim1[["2_0_1"]][["model"]], pred.var = "x4",  train = data_slim1, plot = TRUE)
partial(models_slim1[["2_0_2"]][["model"]], pred.var = "x4",  train = data_slim1, plot = TRUE)

partial(models_slim1[["1_rootnode_0"]][["model"]], pred.var = "x3",  train = data_slim1, plot = TRUE)
partial(models_slim1[["2_0_1"]][["model"]], pred.var = "x3",  train = data_slim1, plot = TRUE)
partial(models_slim1[["2_0_2"]][["model"]], pred.var = "x3",  train = data_slim1, plot = TRUE)

