source("R/load_packages.R")
source("R/helper_general.R")

library(R6)
library(data.table)
library(BBmisc)

#' @title Performs a single tree with main effect models in the leaf nodes
#'
#' @description
#' Uses functions in customtrees.r for splitting according to defined objective.
#'
#' @param y target vector
#' @param x dataset to use for splitting (data.frame with features in columns)
#' @param n.split maximum number of splits to be performed
#' @param impr.par minimum required percentage improvement of the objective by a split compared to the previous split
#' @param min.split minimum number of observations per node
#' @param n.quantiles number of quantile splits to be evaluated per splitting variable
#' @param approximate should approximate splitting algorithm be used
#' @param split.method how should splits be performed? "slim" for exhaustive search, "anova" for zwo step approach with modelcomparison via F-Test. 
#' "R2" for Model comparison with r squared an "R2_adj" for model comparison with adjusted r squared
#' @param pruning select pruning method ('forward', 'none')
#' @param objective character string with objective function to use ('MSE', 'MAE')
#' @param family a description of the error distribution and link function to be used in the model. This can be a character string naming a family function, a family function or the result of a call to a family function
#' @param degree.poly degree of included polynomials
#' @param fit.bsplines should bsplines be fitted
#' @param fit.gam should a gam be fitted
#' @param df.spline number of bspline knots
#' @param penalization "L1" for LASSO "L2" for Ridge and NULL for no penalization

  


# Definition of Class Node
Node <- R6Class("Node", list(
  id = NULL,
  
  # on which depth is the node
  depth = NULL,
  
  # ids of the instances of data that are in this node
  subset.idx = NULL,
  
  # objective value in a node
  objective.value = NULL, 
  objective.value.parent = NULL,
  term.predictions.parent = NULL,
  term.predictions = NULL,
  
  # Parent information
  id.parent = NULL, 
  child.type = NULL, # left or right type
  
  # Split information (if splitting has already taken place)
  split.feature = NULL,
  split.value = NULL,
  split.type = NULL,
  
  # Append the children of this node
  children = list(),
  
  stop.criterion.met = FALSE,
  improvement.met = NULL,
  
  intImp = NULL,
  
  # Model information
  model.fit = NULL,
  split.effect = NULL,
  variable.importance = NULL,

  
  
  initialize = function(id, depth = NULL, subset.idx, id.parent = NULL, child.type = NULL, objective.value.parent = NULL, objective.value = NULL, improvement.met, intImp, model.fit = NULL, term.predictions.parent = NULL, variable.importance = NULL) {
    assert_numeric(id, len = 1)
    assert_numeric(depth, len = 1, null.ok = TRUE)
    
    assert_numeric(subset.idx, min.len = 1)
    assert_numeric(id.parent, len = 1, null.ok = TRUE)
    assert_character(child.type, null.ok = TRUE)
    
    self$id = id
    self$depth = depth
    self$subset.idx = subset.idx
    self$id.parent = id.parent
    self$child.type = child.type
    self$intImp = intImp
    #self$rsqrt = rsqrt
    self$objective.value.parent = objective.value.parent
    self$objective.value = objective.value
    self$improvement.met = improvement.met
    
    self$term.predictions.parent = term.predictions.parent
    self$stop.criterion.met = FALSE
    self$model.fit = model.fit
    self$variable.importance = variable.importance
  },
  
  computeSplit = function(X, Y, objective, fit, impr.par, optimizer, min.split = 10, pruning, n.quantiles, penalization, fit.bsplines, df.spline, split.method) {
    if (length(self$subset.idx) < (2*min.split + 1) | (self$improvement.met == TRUE & pruning == "forward")) {
      self$stop.criterion.met = TRUE
      self$objective.value.parent = objective(y = Y[self$subset.idx, , drop = FALSE], x = X[self$subset.idx, ])
      self$objective.value = NULL
    } else {
      self$objective.value.parent = objective(y = Y, x = X)
      self$objective.value = objective(y = Y[self$subset.idx, ,drop = FALSE], x = X[self$subset.idx, ])
      tryCatch({
        split = split_parent_node(Y = Y[self$subset.idx, ,drop = FALSE], X = X[self$subset.idx, ], 
                                  objective = objective, optimizer = optimizer, 
                                  fit = fit,
                                  min.node.size = min.split, n.quantiles = n.quantiles,
                                  penalization = penalization, 
                                  fit.bsplines = fit.bsplines, df.spline = df.spline,
                                  split.method = split.method)

        if(is.null(self$intImp)) {
          #self$rsqrt = 0 
          self$intImp = 0
        }
        intImp = (self$objective.value - split$objective.value[split$best.split][1]) / self$objective.value.parent
        if(self$intImp == 0){
          if ( intImp < impr.par & pruning == "forward"){
            self$improvement.met = TRUE
            self$objective.value.parent = objective(y = Y[self$subset.idx, , drop = FALSE], x = X[self$subset.idx, ])
            self$objective.value = NULL
          } else{
            self$split.feature = split$feature[1]
            self$split.type = split$split.type
            if (self$split.type == "categorical"){
              self$split.value = str_split(split$split.points, ",")[[1]]
            } else if (self$split.type == "numerical"){
              self$split.value = split$split.points
            }
            self$intImp = intImp
            self$objective.value.parent = objective(y = Y[self$subset.idx, ,drop = FALSE], x = X[self$subset.idx, ])
            self$objective.value = split$objective.value[1]
            
          }
        } else {
          if ( intImp < self$intImp*impr.par & pruning == "forward"){
            self$improvement.met = TRUE
            self$objective.value.parent = objective(y = Y[self$subset.idx, , drop = FALSE], x = X[self$subset.idx, ])
            self$objective.value = NULL
          } else{
            self$split.feature = split$feature
            self$split.type = split$split.type
            if (self$split.type == "categorical"){
              self$split.value = str_split(split$split.points, ",")[[1]]
            } else if (self$split.type == "numerical") {
              self$split.value = split$split.points
            }
            self$intImp = intImp
            self$objective.value.parent = objective(y = Y[self$subset.idx, , drop = FALSE], x = X[self$subset.idx, ])
            self$objective.value = split$objective.value[1]
            
          }
        }
      },
      error = function(cond) {
        browser()
        # message(paste0("Min.node.size is reached in node ", self$id))
        self$stop.criterion.met = TRUE
      })
    }
  },
  
  computeChildren = function(X, Y, objective, fit, predict_response, pruning) {

    if (self$stop.criterion.met|(self$improvement.met & pruning == "forward")) {
      # no further split is performed
      self$children = list("left.child" = NULL, "right.child" = NULL)
    } else {
      if(is.null(self$split.feature))
        stop("Please compute the split first via computeSplit().")

      if (self$split.type == "categorical"){
        idx.left = which(as.character(X[self$subset.idx, self$split.feature]) %in% self$split.value)
        idx.right = which(!(as.character(X[self$subset.idx, self$split.feature]) %in% self$split.value))
      } else if (self$split.type == "numerical"){
        idx.left = which(X[self$subset.idx, self$split.feature] <= self$split.value)
        idx.right = which(X[self$subset.idx, self$split.feature] > self$split.value)
      }
      idx.left = self$subset.idx[idx.left]
      if(length(idx.left)==0) idx.left = 0
      idx.right = self$subset.idx[idx.right]
      if(length(idx.right)==0) idx.right = 0
      
      obj.left = objective(y = Y[idx.left, ,drop = FALSE], x = X[idx.left, ])
      obj.right = objective(y = Y[idx.right, ,drop = FALSE], x = X[idx.right, ])
      obj.parent = objective(y = Y[self$subset.idx, ,drop = FALSE], x = X[self$subset.idx, ])
      
      model.left = fit(y = Y[idx.left, ,drop = FALSE], x = X[idx.left, ])
      model.right = fit(y = Y[idx.right, ,drop = FALSE], x = X[idx.right, ])
      term.predictions.left = predict_response(model.left, x = X[idx.left, ])
      term.predictions.right = predict_response(model.right, x = X[idx.right, ])
      intersection = intersect(colnames(term.predictions.left), colnames(term.predictions.right))
      
      predictions = rbind(term.predictions.left[,intersection, drop = FALSE], term.predictions.right[, intersection, drop = FALSE])

      self$term.predictions = predictions[order(as.numeric(rownames(predictions))),, drop = FALSE]
      self$split.effect = calculate_split_effects(self$term.predictions.parent, self$term.predictions, exclude = self$split.feature)
      
      variable.importance.left = round(apply(term.predictions.left, MARGIN = 2, var), 4)
      variable.importance.right = round(apply(term.predictions.right, MARGIN = 2, var), 4)
      
      
      child.type.left = paste(self$split.feature, ifelse(self$split.type == "numerical", paste("<=", self$split.value), ifelse(self$split.type == "categorical", paste0("%in% c(", paste0(self$split.value, collapse = ","), ")"), NULL)))
      child.type.right =  paste(ifelse(self$split.type == "numerical", paste(self$split.feature, ">", self$split.value), ifelse(self$split.type == "categorical", paste0("!(",self$split.feature, " %in% c(", paste0(self$split.value, collapse = ","), "))"), NULL)))
      if(!is.null(self$child.type)){
        child.type.left = paste(self$child.type, child.type.left, sep = " & ")
        child.type.right = paste(self$child.type, child.type.right, sep = " & ")
      }
      left.child = Node$new(id = 1, depth = self$depth + 1, subset.idx = idx.left, id.parent = self$id, child.type = child.type.left,  improvement.met = self$improvement.met, 
                            intImp = self$intImp, model.fit = model.left, term.predictions.parent = term.predictions.left, objective.value.parent = obj.left, variable.importance = variable.importance.left)
      right.child = Node$new(id = 2, depth = self$depth + 1, subset.idx = idx.right, id.parent = self$id, child.type = child.type.right,  improvement.met = self$improvement.met, 
                             intImp = self$intImp, model.fit = model.right, term.predictions.parent = term.predictions.right, objective.value.parent = obj.right, variable.importance = variable.importance.right)
      
      self$children = list("left.child" = left.child, "right.child" = right.child)
    }
  }
)
)




# compute single tree based on Class 'Node' 
compute_tree_slim = function(y,
                             x,
                             n.split, 
                             impr.par = 0.1,
                             min.split = 10,
                             n.quantiles = 100,
                             approximate = FALSE,
                             split.method = "slim",
                             pruning = "forward", 
                             objective = "MSE",
                             family = "gaussian",
                             degree.poly = 1,
                             fit.bsplines = FALSE,
                             fit.gam = FALSE,
                             df.spline = 15,
                             penalization = NULL) {
  time.start = Sys.time()
  input.data = list(X=as.data.frame(x), Y=as.data.frame(y))

  alpha = 0
  if (objective == "MSE"){
    
    if (fit.gam){
      split.objective = get_objective_gam
      fit.model = get_model_gam
      predict_response = get_prediction_gam
      
    } else {
      if (is.null(penalization)){
        split.objective = get_objective_lm
        fit.model = get_model_lm
        predict_response = get_prediction_lm
        
      } else if (penalization %in% c("L1", "L2")){
        alpha = ifelse(penalization == "L1", 1, 0)
        split.objective = get_objective_glmnet
        formals(split.objective)$.alpha = alpha
        fit.model = get_model_glmnet
        formals(fit.model)$.alpha = alpha
        predict_response = get_prediction_glmnet
        
      } else {
        stop(paste("penalization", penalization, "is not supported."))
      }
      
      
    }
  } else if (objective == "MAE"){
    split.objective = get_objective_lad
    fit.model = get_model_lad
  }
  else {
    stop(paste("objective", objective, "is not supported."))
  } 
  
  # set arguments of objective and splitting function
  formals(split.objective) = list(y = data.frame(), x = data.frame(), 
                                  .degree.poly = degree.poly,
                                  .df.spline = df.spline,
                                  .fit.bsplines = fit.bsplines,
                                  .family = family,
                                  .alpha = alpha)
  
  formals(fit.model) = list(y = data.frame(), x = data.frame(), 
                            .degree.poly = degree.poly,
                            .df.spline = df.spline,
                            .fit.bsplines = fit.bsplines,
                            .family = family,
                            .alpha = alpha)

  # Initialize the parent node of the tree
  model.parent = fit.model(y = input.data$Y, x = input.data$X)
  term.predictions.parent = predict_response(model.parent, input.data$X)
  parent = Node$new(id = 0, depth = 1, subset.idx = seq_len(nrow(input.data$X)), improvement.met = FALSE, intImp = 0, model.fit = model.parent, 
                    term.predictions.parent = term.predictions.parent, variable.importance = round(apply(term.predictions.parent, MARGIN = 2, var), 4))
  
  # Perform splitting for the parent
  tree = list(list(parent))

  for (depth in seq_len(n.split)) {
    leaves = tree[[depth]]
    
    tree[[depth + 1]] = list()
    
    for (node.idx in seq_along(leaves)) {
      node.to.split = leaves[[node.idx]]
      
      if (!is.null(node.to.split)) {
        node.to.split$computeSplit(X = input.data$X, Y = input.data$Y, objective = split.objective,
                                   fit = fit.model, 
                                   impr.par = impr.par, optimizer = ifelse(approximate == FALSE, find_best_binary_split, find_best_binary_split_approx), 
                                   min.split = min.split, pruning = pruning, n.quantiles = n.quantiles,
                                   penalization = penalization, fit.bsplines = fit.bsplines, df.spline = df.spline,
                                   split.method = split.method)
        node.to.split$computeChildren(input.data$X, input.data$Y, objective = split.objective,
                                      fit = fit.model, predict_response = predict_response, 
                                      pruning = pruning)
        tree[[depth + 1]] = c(tree[[depth + 1]], node.to.split$children)    

      } else {
        tree[[depth + 1]] = c(tree[[depth + 1]], list(NULL,NULL))                
      }
    }

  }
  time.end = Sys.time()
  # print(time.end-time.start)
  return(tree)
}




