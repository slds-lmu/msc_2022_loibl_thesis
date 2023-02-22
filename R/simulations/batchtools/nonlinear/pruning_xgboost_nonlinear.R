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
data_nonlinear = create_sim_data(n = 45000, type = "nonlinear_mixed")$data

# Replication xgboost predictions for bike sharing example

task_nonlinear = as_task_regr(x = data_nonlinear, target = "y")

learner_nonlinear = lrn("regr.xgboost",
                            max_depth = 4,
                            eta = 0.825,
                            alpha = 0.75,
                            gamma = 1,
                            nrounds = 700,
                            interaction_constraints = "[[3,4],[0,3,5]]")
search_space = ps(
  max_depth = p_int(lower = 2, upper = 8),
  eta = p_dbl(lower = 0.5, upper = 1),
  alpha = p_dbl(lower = 0, upper = 2),
  gamma = p_dbl(lower = 1, upper = 5),
  nrounds = p_int(lower = 200, upper = 1000)
)

terminator = trm("evals", n_evals = 500)

at = AutoTuner$new(
  learner = learner_nonlinear,
  resampling = rsmp("holdout"),
  measure = msr("regr.mse"),
  search_space = search_space,
  terminator = terminator,
  tuner = tnr("random_search")
)

at$train(task_nonlinear)
tuning_nonlinear = at$tuning_result


learner_nonlinear$train(task_nonlinear)

prediction_nonlinear = learner_nonlinear$predict(task_nonlinear)

prediction_nonlinear$score(list(msr("regr.mse"), msr("regr.rsq")))
y_pred_nonlinear = as.data.table(prediction_nonlinear)$response


x_nonlinear = data_nonlinear[which(names(data_nonlinear) != "y")]

predictor_nonlinear = Predictor$new(learner_nonlinear, data = x_nonlinear, y = data_nonlinear$y)
interact_nonlinear = Interaction$new(predictor_nonlinear)
plot(interact_nonlinear)

