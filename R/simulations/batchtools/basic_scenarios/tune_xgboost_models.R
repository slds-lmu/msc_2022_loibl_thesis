source("R/simulations/batchtools/simulation_setting_definition.R")
source("R/load_packages.R")
library(mlr3)
library(mlr3viz)
library(mlr3learners)
library(mlr3tuning)
library(mlr3pipelines)
library(paradox)
library(iml)

# Linear smooth
data_linear_smooth = create_sim_data(n = 3000, type = "linear_smooth")$data

# Replication xgboost predictions for bike sharing example

task_linear_smooth = as_task_regr(x = data_linear_smooth, target = "y")

learner_linear_smooth = lrn("regr.xgboost",
                            max_depth = 5,
                            eta = 0.5,
                            alpha = 1,
                            gamma = 2,
                            nrounds = 400,
                            interaction_constraints = "[[1,2]]")
search_space = ps(
  max_depth = p_int(lower = 2, upper = 8),
  eta = p_dbl(lower = 0.5, upper = 1),
  alpha = p_dbl(lower = 0, upper = 2),
  gamma = p_dbl(lower = 1, upper = 5),
  nrounds = p_int(lower = 200, upper = 1000)
)

terminator = trm("evals", n_evals = 500)

at = AutoTuner$new(
  learner = learner_linear_smooth,
  resampling = rsmp("holdout"),
  measure = msr("regr.mse"),
  search_space = search_space,
  terminator = terminator,
  tuner = tnr("random_search")
)

at$train(task_linear_smooth)
tuning_linear_smooth = at$tuning_result


learner_linear_smooth$train(task_linear_smooth)

prediction_linear_smooth = at$predict(task_linear_smooth)

prediction_linear_smooth$score(list(msr("regr.mse"), msr("regr.rsq")))
y_pred_linear_smooth = as.data.table(prediction_linear_smooth)$response

summary(lm(y ~ x1 + x2 + x2:x3 , data = data_linear_smooth))

x_linear_smooth = data_linear_smooth[which(names(data_linear_smooth) != "y")]

predictor_linear_smooth = Predictor$new(at, data = x_linear_smooth, y = data_linear_smooth$y)
interact_linear_smooth = Interaction$new(predictor_linear_smooth)
plot(interact_linear_smooth)





# Linear abrupt
data_linear_abrupt = create_sim_data(n = 3000, type = "linear_abrupt")$data

# Replication xgboost predictions for bike sharing example

task_linear_abrupt = as_task_regr(x = data_linear_abrupt, target = "y")



learner_linear_abrupt = lrn("regr.xgboost",
                            max_depth = 3,
                            eta = 0.5,
                            alpha = 0.5,
                            gamma = 1,
                            nrounds = 350,
                            interaction_constraints = "[[0,1], [1,2]]"
                            )

learner_linear_abrupt$param_set


# search_space = ps(
#   # max_depth = p_int(lower = 2, upper = 8),
#   eta = p_dbl(lower = 0.5, upper = 1),
#   alpha = p_dbl(lower = 0, upper = 2),
#   gamma = p_dbl(lower = 1, upper = 5)
#   #,
#   # nrounds = p_int(lower = 5, upper = 1000)
# )
# 
# terminator = trm("evals", n_evals = 500)
# 
# at = AutoTuner$new(
#   learner = learner_linear_abrupt,
#   resampling = rsmp("holdout"),
#   measure = msr("regr.mse"),
#   search_space = search_space,
#   terminator = terminator,
#   tuner = tnr("random_search")
# )
# 
# at$train(task_linear_abrupt)
# tuning_linear_abrupt = at$tuning_result


learner_linear_abrupt$train(task_linear_abrupt)

prediction_linear_abrupt = learner_linear_abrupt$predict(task_linear_abrupt)

prediction_linear_abrupt$score(list(msr("regr.mse"),
                                    msr("regr.rsq")))
y_pred_linear_abrupt = as.data.table(prediction_linear_abrupt)$response

summary(gam(y ~ x1 + x2 + x2:x3 + x1:x2, data = data_linear_abrupt))

x_linear_abrupt = data_linear_abrupt[which(names(data_linear_abrupt) != "y")]

predictor_linear_abrupt = Predictor$new(learner_linear_abrupt, data = x_linear_abrupt, y = data_linear_abrupt$y)
interact_linear_abrupt = Interaction$new(predictor_linear_abrupt)
plot(interact_linear_abrupt)





# linear_mixed
data_linear_mixed = create_sim_data(n = 3000, type = "linear_mixed")$data

task_linear_mixed = as_task_regr(x = data_linear_mixed, target = "y")

learner_linear_mixed = lrn("regr.xgboost",
                           max_depth = 5,
                           eta = 0.5,
                           alpha = 2,
                           gamma = 3.5,
                           nrounds = 500,
                           interaction_constraints = "[[0,1], [1,2], [0,1,3]]")

summary(gam(y ~ x2 + x4 + x2:x1 + x2:x3 + x1:x2:x4, data = data_linear_mixed))


search_space = ps(
  max_depth = p_int(lower = 5, upper = 15),
  eta = p_dbl(lower = 0.5, upper = 1),
  alpha = p_dbl(lower = 0, upper = 2),
  gamma = p_dbl(lower = 1, upper = 5),
  nrounds = p_int(lower = 100, upper = 1000),
  colsample_bytree = p_dbl(lower = 0.6, upper = 1)
)

terminator = trm("evals", n_evals = 500)

at = AutoTuner$new(
  learner = learner_linear_mixed,
  resampling = rsmp("holdout"),
  measure = msr("regr.mse"),
  search_space = search_space,
  terminator = terminator,
  tuner = tnr("random_search")
)

at$train(task_linear_mixed)
tuning_linear_mixed = at$tuning_result

learner_linear_mixed$train(task_linear_mixed)

prediction_linear_mixed = learner_linear_mixed$predict(task_linear_mixed)

prediction_linear_mixed$score(list(msr("regr.mse"),
                                         msr("regr.rsq")))
y_pred_linear_mixed = as.data.table(prediction_linear_mixed)$response

x_linear_mixed = data_linear_mixed[which(names(data_linear_mixed) != "y")]

predictor_linear_mixed = Predictor$new(learner_linear_mixed, data = x_linear_mixed, y = data_linear_mixed$y)
interact_linear_mixed = Interaction$new(predictor_linear_mixed)
plot(interact_linear_mixed)



# mixed_large
data_mixed_large = create_sim_data_slim(n = 5000, type = "mixed_large")$data

task_mixed_large = as_task_regr(x = data_mixed_large, target = "y")

learner_mixed_large = lrn("regr.xgboost"
                          ,
                           max_depth = 9,
                           eta = 1,
                           alpha = 0.3,
                           gamma = 3.5
)

fencoder = po("encode", method = "treatment", affect_columns = selector_type("factor"))
fencoder$train(list(task_mixed_large))
graph = fencoder %>>% learner_mixed_large
learner_mixed_large = as_learner(graph)

# search_space = ps(
#   regr.xgboost.max_depth = p_int(lower = 4, upper = 12),
#   regr.xgboost.eta = p_dbl(lower = 0.5, upper = 1),
#   regr.xgboost.alpha = p_dbl(lower = 0, upper = 2),
#   regr.xgboost.gamma = p_dbl(lower = 1, upper = 5)
# )
# terminator = trm("evals", n_evals = 1000)
# 
# at = AutoTuner$new(
#   learner = learner_mixed_large,
#   resampling = rsmp("holdout"),
#   measure = msr("regr.mse"),
#   search_space = search_space,
#   terminator = terminator,
#   tuner = tnr("random_search")
# )
# 
# at$train(task_mixed_large)
# tuning_mixed_large = at$tuning_result

learner_mixed_large$train(task_mixed_large)

prediction_mixed_large = learner_mixed_large$predict(task_mixed_large)

prediction_mixed_large$score(list(msr("regr.mse"),
                                   msr("regr.rsq")))
y_pred_mixed_large = as.data.table(prediction_mixed_large)$response



