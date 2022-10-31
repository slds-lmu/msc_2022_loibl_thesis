# Replication xgboost predictions for SLIM 3.2 Case 2 (page 8)
# Two-way and Three-way Interactions

source("R/simulations/simulation_setting_definition.R")

n = 100000
data_slim = create_sim_data(nob = NULL, n = n, type = "slim2")
X_slim = data_slim[which(names(data_slim)!="y")]
y_slim = data_slim$y

library(mlr3)
library(mlr3viz)
library(mlr3learners)
library(mlr3tuning)
library(mlr3pipelines)

task_slim = as_task_regr(x = data_slim, target = "y")

learner = lrn("regr.xgboost")
learner$param_set
fencoder = po("encode", method = "treatment", affect_columns = selector_type("factor"))
fencoder$train(list(task_slim))
graph = fencoder %>>% learner
learner_slim = as_learner(graph)                                                                      

search_space = ps(
  regr.xgboost.max_depth = p_int(lower = 3, upper = 15),
  regr.xgboost.eta = p_dbl(lower = 0, upper = 1),
  regr.xgboost.alpha = p_dbl(lower = 0, upper = 3),
  regr.xgboost.gamma = p_dbl(lower = 0, upper = 10),
  regr.xgboost.colsample_bynode = p_dbl(lower = 0.4, upper = 1),
  regr.xgboost.colsample_bytree = p_dbl(lower = 0.4, upper = 1)
)

stop_time = as.POSIXct("2022-10-10 17:00:00")
terminator = trm("clock_time", stop_time = stop_time)

at_slim = AutoTuner$new(
  learner = learner_slim,
  resampling = rsmp("holdout"),
  measure = msr("regr.mse"),
  search_space = search_space,
  terminator = terminator,
  tuner = tnr("random_search")
)
at_slim
at_slim$train(task_slim)
at_slim$model


prediction = at_slim$predict(task_slim)
y_pred_slim = as.data.table(prediction)$response

save(at_slim, y_pred_slim, y_slim, X_slim, file = "Data/simulations/xgboost_slim_100000.RData")
