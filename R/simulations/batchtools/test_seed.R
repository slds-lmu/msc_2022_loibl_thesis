if (!dir.exists("Data/simulations/batchtools/test")) dir.create("Data/simulations/batchtools/test", recursive = TRUE)

reg = makeExperimentRegistry(file.dir = "Data/simulations/batchtools/test/batchtools",
                             seed = 1
                             , conf.file = NA
)


# --- 2. ADD PROBLEMS, ALGORITHMS, EXPERIMENTS ---

set.seed(10)
# add problems and setting definitions
repls = 10L
# set.seed(49)
data_test = c(1,2)


addProblem(name = "test_problem", data = data_test, fun = function(job, n,...){
  data = sample(1:10,size = n, replace = TRUE)
  return(data)
  }, reg = reg, seed = 2)
pdes = list("test_problem" = expand.grid(n = c(10)))


# add algorithm

addAlgorithm(name = "get_test_results", fun = function(data, job, instance, type){
  if(type == "sum1"){
    res = sum(c(data,instance))
  } else if(type == "sum2"){
    res = sum(c(data,instance))
  }
  return(res)
})
ades = list(get_test_results = data.frame(type = c("sum1", "sum2")))



# add experiments
addExperiments(
  reg = reg, 
  prob.designs = pdes,
  algo.designs = ades, 
  repls = repls)

summarizeExperiments()
testJob(1)
submitJobs()

reduce = function(res) res
results = unwrap(reduceResultsDataTable(fun = reduce, reg = reg))
head(results)

pars = unwrap(getJobPars(reg = reg))
tab = ijoin(pars, results)
tab
