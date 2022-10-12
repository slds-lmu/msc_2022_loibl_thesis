# Replication xgboost predictions for bike sharing example

library(mlr3)
library(mlr3viz)
library(mlr3learners)
library(mlr3tuning)
library(mlr3pipelines)
library(paradox)
library(iml)


data_bike = read.csv("Data/realworld_examples/bikesharing/Bike-Sharing-Dataset/hour.csv")
data_bike = data_bike[!(names(data_bike) %in% c("instant", "dteday", "yr", "casual", "registered"))]
# x_bike = data_bike[names(data_bike) != "cnt"]
data_bike$season = as.factor(data_bike$season)
data_bike$workingday = as.factor(data_bike$workingday)
data_bike$weekday = as.factor(data_bike$weekday)
data_bike$weathersit = as.factor(data_bike$weathersit)
data_bike$holiday = as.factor(data_bike$holiday)
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
data_bike$temp = min_max_norm(data_bike$temp)
data_bike$atemp = min_max_norm(data_bike$atemp)
data_bike$cnt = log(data_bike$cnt)


task_bike = as_task_regr(x = data_bike, target = "cnt")

learner = lrn("regr.xgboost")
learner$param_set
fencoder = po("encode", method = "treatment", affect_columns = selector_type("factor"))
fencoder$train(list(task_bike))
graph = fencoder %>>% learner
learner_bike = as_learner(graph)

search_space = ps(
  regr.xgboost.max_depth = p_int(lower = 3, upper = 15),
  regr.xgboost.eta = p_dbl(lower = 0, upper = 1),
  regr.xgboost.alpha = p_dbl(lower = 0, upper = 3),
  regr.xgboost.gamma = p_dbl(lower = 0, upper = 10),
  regr.xgboost.colsample_bynode = p_dbl(lower = 0.4, upper = 1),
  regr.xgboost.colsample_bytree = p_dbl(lower = 0.4, upper = 1)
)

stop_time = as.POSIXct("2022-09-23 11:00:00")
terminator = trm("clock_time", stop_time = stop_time)

at = AutoTuner$new(
  learner = learner_bike,
  resampling = rsmp("holdout"),
  measure = msr("regr.mse"),
  search_space = search_space,
  terminator = terminator,
  tuner = tnr("random_search")
)

at$train(task_bike)
at$model


prediction = at$predict(task_bike)
y_pred_bike = as.data.table(prediction)$response


x_bike = data_bike[names(data_bike) != "cnt"]
n = nrow(x_bike)
y_bike = data_bike[,"cnt"]

save(at, x_bike, y_pred_bike, y_bike, file = "Data/realworld_examples/bikesharing/xgboost_bikesharing.RData")

# model = Predictor$new(at, data = x_bike, y = data_bike$cnt)
# featimp = FeatureImp$new(model,loss="mse")
# plot(featimp)
# effect = FeatureEffects$new(model, method = "pdp", features = colnames(x_bike))
# plot(effect)
# 
# interaction_workingday = FeatureEffect$new(model, method = "pdp", feature = c("hr", "workingday"))
# plot(interaction_workingday)
# 
# interaction_weekday = FeatureEffect$new(model, method = "pdp", feature = c("hr", "weekday"))
# plot(interaction_weekday)
# 
# interaction_workingday_temp = FeatureEffect$new(model, method = "pdp", feature = c("temp", "workingday"))
# plot(interaction_workingday_temp)
# 
# 
# interact_global <- Interaction$new(model)
# plot(interact_global)
# 
# h_workingday <- Interaction$new(model, feature = "workingday")
# plot(h_workingday)




