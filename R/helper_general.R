# Helper functions

predict_slim = function(tree, newdata, type = "response", degree.poly = 1){
  if(type == "response"){
    models = extract_models(tree)
  }
  nodes = extract_split_criteria(tree)
  nodes = nodes[nodes$split.feature == "leafnode",c("child.type", "id.node")]
  if (degree.poly > 1) {
    features = names(newdata)
    for(f in features){
      if (is.numeric(newdata[[f]])){
        for(d in 2:degree.poly){
          newdata = cbind(newdata, "new" = newdata[[f]]^d)
          colnames(newdata)[which(names(newdata) == "new")] = paste0(f,"_",d)
        }
      }
    }
  }
  newdata = as.data.table(newdata)
  newdata$row_id = 1:nrow(newdata)
  
  predictions = c()
  for (n in 1:nrow(nodes)){
    node_data = as.data.frame(newdata[eval(parse(text = nodes[n, "child.type"])),])
    node_id = as.character(nodes[n,"id.node"])
    if (nrow(node_data) > 0){
      if (type == "response"){
        newdata_n = subset(node_data, select = -c(row_id))
        if(class(models[[node_id]]$model)[1] %in% c("elnet", "glmnet")){
          newdata_n = newdata_n[, colnames(newdata_n) %in% rownames(models[[node_id]]$model$beta)]
          newdata_n = as.matrix(newdata_n)
        }
        y_hat = predict(models[[node_id]]$model, newdata_n)
      }
      else if (type == "node"){
        y_hat = node_id
      }
      
      predictions = rbind(predictions, cbind(node_data, y_hat = as.vector(y_hat)))
    }
  }
  predictions = predictions[order(predictions$row_id),]
  rownames(predictions) = predictions$row_id
  return(predictions$y_hat)
}


extract_split_criteria = function(tree){
  list.split.criteria = lapply(tree, function(depth){
    lapply(depth, function(node){
      if(is.null(node)){
        df = data.frame("depth" = NA, "id" = NA, 
                        "id.parent" = NA,
                        "objective.value" = NA, 
                        "objective.value.parent" = NA,
                        "r2" = NA,
                        "intImp" = NA, 
                        "split.feature" = "final", 
                        "split.value" = NA,
                        "child.type" = "final",
                        "size" = NA,
                        "guide.test" = NA)
      } else{
        df = data.frame("depth" = node$depth, "id" = node$id,
                        "id.parent" = ifelse(is.null(node$id.parent), 0, node$id.parent),
                        "objective.value" = ifelse(is.null(node$objective.value), NA, node$objective.value),
                        "objective.value.parent" = ifelse(is.null(node$objective.value.parent), NA, node$objective.value.parent),
                        "r2" = ifelse(is.null(node$r2), NA, node$r2),
                        "intImp" = ifelse(is.null(node$intImp), NA, node$intImp),
                        "split.feature" = ifelse(is.null(node$split.feature), "leafnode", node$split.feature),
                        "split.value" = ifelse(is.null(node$split.value), NA, node$split.value),
                        "child.type" = ifelse(is.null(node$child.type), "rootnode", node$child.type),
                        "size" = length(node$subset.idx), 
                        "guide.test" = ifelse(is.null(node$test.type), NA, node$test.type))
      }
      df
    })
  })
  
  list.split.criteria = list.clean(list.split.criteria, function(x) length(x) == 0L, TRUE)
  df.split.criteria = unlist(list.split.criteria, recursive = FALSE)
  df.split.criteria = as.data.frame(do.call(rbind, df.split.criteria))
  n.final = length(which(df.split.criteria$child.type == "final"))
  df.split.criteria = df.split.criteria[df.split.criteria$child.type!="final",]
  df.split.criteria$id.node = 0:(nrow(df.split.criteria)-1)
  row.names(df.split.criteria) = df.split.criteria$id.node
  df.split.criteria[] = lapply(df.split.criteria, unlist)
  # print(paste0("RSS:",(sum(unlist(df.split.criteria[df.split.criteria$split.feature=="leafnode",]$objective.value.parent)))))
  return(df.split.criteria)
}

extract_models = function(tree){
  list.split.criteria = lapply(tree, function(depth){
    lapply(depth, function(node){
      if(is.null(node)) l = NULL
      else{
        l = list("depth" = node$depth, "id" = node$id,
                 "id.parent" = ifelse(is.null(node$id.parent), "rootnode", node$id.parent),
                 "child.type" = ifelse(is.null(node$child.type), "leafnode", node$child.type),
                 "model" = node[["model.fit"]])
        
      }
      l
    })
  })

  list.split.criteria = unlist(list.split.criteria, recursive = FALSE)
  list.split.criteria = list.split.criteria[!sapply(list.split.criteria,is.null)]
  names(list.split.criteria) = 0:(length(list.split.criteria) - 1)
  return(list.split.criteria)
}


# helper functions for tree splitting

# performs one split

split_parent_node = function(Y, X, n.splits = 1, min.node.size = 10, optimizer,
                             objective, fit, approximate, n.quantiles, penalization, 
                             fit.bsplines, df.spline, split.method, correction.factor, ...) {
  if(split.method == "slim"){
    z = colnames(X)
  } else if (split.method %in% c("anova", "R2", "R2_adj")){
    X = X %>% dplyr::select(where(~ n_distinct(.) > 1))
    interaction_models = find_split_variable_anova(Y = Y, X = X,
                                                   objective = objective, 
                                                   fit = fit,
                                                   split.method = split.method,
                                                   penalization = penalization, 
                                                   fit.bsplines = fit.bsplines,
                                                   df.spline = df.spline)
    z = interaction_models$z

  } else if(split.method == "guide"){
    optimizer = find_best_binary_split
    X = X %>% dplyr::select(where(~ n_distinct(.) > 1))
    z_guide = find_split_variable_guide(Y = Y, X = X,
                                        objective = objective, 
                                        fit = fit,
                                        optimizer = optimizer, 
                                        correction.factor = correction.factor)
    z = z_guide$z
    guide_type = z_guide$type
  }
  # browser()
  split_point = find_split_point(Y = Y, X = X, z = z, n.splits = n.splits,
                                 min.node.size = min.node.size, 
                                 optimizer = optimizer,
                                 objective = objective, 
                                 fit = fit,
                                 approximate = approximate, 
                                 n.quantiles = n.quantiles, 
                                 penalization = penalization, 
                                 fit.bsplines = fit.bsplines,
                                 df.spline = df.spline)
  # browser()
  if(split.method == "guide"){
    split_point$test.type = guide_type
  }
  
  return(split_point)
  
  
}

find_split_variable_guide = function(Y, X, objective, fit, optimizer, correction.factor, ...){
  model = fit(y = Y, x = X)
  residuals = Y - predict(model, X)
  z_guide = guide_test(y = Y, x = X, residuals = residuals, xgroups = NULL, 
                       optimizer = optimizer, objective = objective, correction.factor = correction.factor)
  return(z_guide)
}



find_split_variable_anova = function(Y, X, objective, fit, split.method, penalization, 
                               fit.bsplines, df.spline, ...) {
  main_effect_model = fit(y = Y, x = X)
  aov_interaction = lapply(colnames(X), function(z){

      fm = paste("y ~",paste(paste0(z, "*", colnames(X)[colnames(X)!=z]), collapse = " + "))
      x_new = as.data.frame(model.matrix(as.formula(fm), data = cbind(Y,X)))
      interaction_model = fit(y = Y, x = x_new)
      aov = anova(main_effect_model, interaction_model, test = "F")
      p = aov[["Pr(>F)"]][2]
      R2_adj = summary(interaction_model)$adj.r.squared
      R2 = summary(interaction_model)$r.squared
      

    return(c(p_value = p, R2_adj = R2_adj, R2 = R2))
  })
  aov_interaction = as.data.frame(do.call(cbind, aov_interaction))
  z = colnames(X)
  if(split.method == "anova"){
    min_p = aov_interaction["p_value", ] == min(aov_interaction["p_value", ], na.rm = TRUE)
    min_p[is.na(min_p)] = FALSE
    aov_best = aov_interaction[, min_p]
    z = z[min_p]
    if (length(z)>1){
      max_r2 = aov_best["R2", ] == max(aov_best["R2", ], na.rm = TRUE)
      max_r2[is.na(max_r2)] = FALSE
      aov_best = aov_best[, max_r2]
      z = z[max_r2]
    }
  } else if(split.method == "R2"){
    max_r2 = aov_interaction["R2", ] == max(aov_interaction["R2", ], na.rm = TRUE)
    max_r2[is.na(max_r2)] = FALSE
    z = z[max_r2]
  } else if(split.method == "R2_adj"){
    max_r2_adj = aov_interaction["R2_adj", ] == max(aov_interaction["R2_adj", ], na.rm = TRUE)
    max_r2_adj[is.na(max_r2_adj)] = FALSE
    z = z[max_r2_adj]
  }

  return(list(aov_interaction = aov_interaction, z = z))
}



find_split_point = function(Y, X, z, n.splits = 1, min.node.size = 10, optimizer,
                            objective, fit, approximate = FALSE, n.quantiles, splitpoints = "quantiles", penalization = NULL, 
                            fit.bsplines = FALSE, df.spline = NULL, ...) {
  # browser()
# find best split point per splitting feature z
  opt.feature = lapply(z, function(feat) {
    t1 = proc.time()
    res = optimizer(xval = X[,feat], x = X, y = Y, n.splits = n.splits, min.node.size = min.node.size,
                    objective = objective, fit = fit, n.quantiles = n.quantiles, splitpoints = splitpoints, penalization = penalization, 
                    fit.bsplines = fit.bsplines, df.spline = df.spline, ...)
    t2 = proc.time()
    res$runtime = (t2 - t1)[[3]]
    return(res)
  })
  # browser()
  names(opt.feature) = z
  result = data.table::rbindlist(lapply(opt.feature, as.data.frame), idcol = "feature")
  result$best.split = result$objective.value == min(result$objective.value)
  result = result[best.split == TRUE]
  if (result$split.type == "numerical" & is.factor(result$split.points)){
    result$split.points = as.numeric(levels(result$split.points))[result$split.points]
  } else if (result$split.type == "numerical" & !is.factor(result$split.points)){
    result$split.points = as.numeric(result$split.points)
  } else if (result$split.type == "categorical"){
    result$split.points = as.character(result$split.points)
  }
  
  return(result)
}



generate_node_index = function(Y, X, result) {
  assert_data_table(result)
  # TODO: fix bug if more than one feature have the same best objective
  feature = unique(result$feature[result$best.split])
  split.points = unlist(result$split.points[result$best.split])
  if (is.vector(X))
    xval = X else
      xval = X[, feature]
  
  cuts = c(min(xval), split.points, max(xval))
  sp = cut(xval, breaks = unique(cuts), include.lowest = TRUE)
  #levels(sp) = paste0(feature, " in ", levels(sp))
  
  return(list(class = sp, index = split(seq_along(xval), sp)))
}



# performs and evaluates binary splits
find_best_binary_split = function(xval, x, y, n.splits = 1, min.node.size = 10, objective, fit, n.quantiles, splitpoints = "quantiles", ...) {
  if(length(unique(xval)) == 1){
    return(list(split.points = NA, objective.value = Inf, split.type = "categorical"))
  }
  assert_choice(n.splits, choices = 1)

  if(splitpoints == "quantiles"){
    # use different split candidates to perform split
    if (is.null(n.quantiles) & !is.factor(xval)){
      q = unique(xval)
    } else{
      q = generate_split_candidates(xval, n.quantiles = n.quantiles, min.node.size = min.node.size)
    }
  } else if(splitpoints == "mean"){
    q = mean(xval)
  }

  splits = vapply(q, FUN = function(i) {
    perform_split(i, xval = xval, x = x, y = y, min.node.size = min.node.size,
                  objective = objective, ...)
  }, FUN.VALUE = NA_real_, USE.NAMES = FALSE)
  # select the split point yielding the minimal objective
  best = which.min(splits)
  if (is.list(q[best])){
    split.points = paste(q[best][[1]][[1]], collapse = ",")
    split.type = "categorical"
  } else {
    split.points = q[best]
    split.type = "numerical"
    
  }
  return(list(split.points = split.points, objective.value = splits[best], split.type = split.type))
}

# performs and evaluates binary splits with approximative efficient numerical algorithm (vgl. slim paper)
find_best_binary_split_approx = function(xval, x, y, n.splits = 1, min.node.size = 10, 
                                         objective, fit, n.quantiles, penalization, 
                                         fit.bsplines, df.spline, ...) {
  if(length(unique(xval)) == 1){
    return(list(split.points = NA, objective.value = Inf, split.type = "categorical"))
  }
  assert_choice(n.splits, choices = 1)

  
  # use different split candidates to perform split
  q = generate_split_candidates(xval, n.quantiles = n.quantiles, min.node.size = min.node.size)
  
  # assign intervalnr. according to split points
  if (is.numeric(xval)){
    node.number = findInterval(x = xval, vec = q, rightmost.closed = FALSE, left.open = TRUE)
    
  } else if (is.factor(xval)){
    node.number = xval
  }
  # create with gram matrices for all bins
  gram.list = create_gramlist(x = x, y = y, bin = node.number, fit.bsplines = fit.bsplines, penalization = penalization, df.spline = df.spline)
  
  if (is.numeric(xval)){
    splits = perform_gram_splits_numeric(gram.list, x = x, min.node.size = min.node.size, penalization = penalization)
  } else if (is.factor(xval)){
    splits = perform_gram_splits_factor(gram.list, q = q, min.node.size = min.node.size, penalization = penalization)
  }
  
  
  # select the split point yielding the minimal objective
  best = which.min(splits)
  if (is.list(q[best])){
    split.points = paste(q[best][[1]][[1]], collapse = ",")
    split.type = "categorical"
  } else {
    split.points = q[best]
    split.type = "numerical"
  }
  return(list(split.points = split.points, objective.value = splits[best], split.type = split.type))
}

generate_split_candidates = function(xval, n.quantiles = NULL, min.node.size = 10) {
  assert_integerish(min.node.size, upper = floor((length(xval) - 1)/2))
  if (is.factor(xval)){
    xlevels = unique(xval)
    q = rapply(listParts(length(xlevels)), function(v) xlevels[v], how = "replace")
    q = q[lengths(q) == 2]
    
  } else {
    xval = sort.int(xval)
    # try to ensure min.node.size between points (is not guaranteed)
    # chunk.ind = seq.int(min.node.size + 1, length(xval) - min.node.size, by = min.node.size)
    # #xadj = unique(quantile(xval, prob = chunk.ind/length(xval), type = 1))
    # xadj = xval[chunk.ind]
    xadj = xval[-c(1:min.node.size, (length(xval)-min.node.size+1):length(xval))]
    if (!is.null(n.quantiles)) {
      if (n.quantiles < 2){
        print(paste0("Minimum value for n.quantiles is 2"))
        n.quantiles = 2
      }
      # to speedup we use only quantile values as possible split points
      # qprobs = seq(1/n.quantiles, (n.quantiles - 1)/n.quantiles, by = 1/n.quantiles)
      qprobs = seq(0, 1, by = 1/n.quantiles)
      qprobs = qprobs[c(-1,-length(qprobs))]
      q = unique(quantile(xadj, qprobs, type = 1))
    } else {
      q = unique(xadj)
    }
    
    # use a value between two subsequent points
    q = adjust_split_point(q, xval)
    q = sort.int(q)
    q = get_closest_point(q, xval, min.node.size)
  }
  
  return(q)
}


create_gramlist = function(x, y, bin = node.number, fit.bsplines, penalization, df.spline){
  # create matrix of spline transformations of the features and use it as x
  if (fit.bsplines){
    spline.matrix = c()
    for(feature in colnames(x)){
      if (is.numeric(x[, feature])){
        new.spline.matrix = bs(x[,feature], df = (df.spline)*2, degree = 1)
        colnames(new.spline.matrix) = paste0(feature, "_bs_", colnames(new.spline.matrix))
        spline.matrix = cbind(spline.matrix,new.spline.matrix)
      } else {
        spline.matrix = cbind(spline.matrix, x[,feature])
        
      }
     
    }
    x = spline.matrix
  }
  data = as.data.frame(cbind(y, x)) 
  if(length(unique(bin)) == 1){
    data.list = list(data)
  } else {
    data.list = split(data, bin, drop = TRUE)
  }
  
  gram.list = lapply(data.list, function(bin){
    x = bin[!(colnames(bin) %in% c("y"))] 
    x = as.matrix(x)
    if (!is.null(penalization)){
      if (penalization == "L2"){
        # standardization of features for penalized regression
        x = apply(x, 2, function(col){
          x.z = (col - mean(col))/sd(col)
        })
      }
    } 
    # add intercept column
    x = cbind(1,x)
    y = bin$y
    
    # create gram matrices
    xtx = t(x)%*%x
    xty = t(x)%*%y
    yty = t(y)%*%y
    
    n.bin = nrow(x)
    
    return(list(xtx = xtx, xty = xty, yty = yty, n.bin = n.bin))
  })
  
  return(gram.list)
}

# Performs a single split and measures the objective
perform_split = function(split.points, xval, x, y, min.node.size, objective, ...) {
  if (is.factor(xval)){
    node.number = ifelse(xval %in% split.points[[1]], 1L, 2L)
  } else {
    # assign intervalnr. according to split points
    node.number = findInterval(x = xval, split.points, rightmost.closed = TRUE) + 1
  }
  
  # compute size of each childnode
  node.size = tabulate(node.number)
  # if minimum node size is violated, return Inf
  # TODO: instead of returning Inf try to avoid that this happens by fixing split points
  if (min(node.size) < min.node.size)
    return(Inf)
  # compute objective in each interval and sum it up
  y.list = split(y, node.number)
  x.list = split(x, node.number) 
  
  res = vapply(seq_along(y.list), FUN = function(i) {
    objective(y = y.list[[i]], x = x.list[[i]])
  }, FUN.VALUE = NA_real_, USE.NAMES = FALSE)
  
  return(sum(res))
}

# perform and evaluate splits based on gram matrices
perform_gram_splits_numeric = function(gram.list,
                                       x,
                                       min.node.size, 
                                       penalization, 
                                       include.parent.sse = FALSE){
  n.bins = length(gram.list)
  
  # for test purposes
  if (n.bins == 1){
    xtx = gram.list[[1]]$xtx
    xty = gram.list[[1]]$xty
    yty = gram.list[[1]]$yty
    sse.total = calculate_sse_closed(xtx, xty, yty, nrow(x), min.node.size, penalization)
    splits = sse.total
  } else {
    # calculate aggregated gram matrices 
    cxtx.t = gram.list[[1]]$xtx
    cxty.t = gram.list[[1]]$xty
    cyty.t = gram.list[[1]]$yty
    for (b in 2:n.bins){
      cxtx.t = cxtx.t + gram.list[[b]]$xtx
      cxty.t = cxty.t + gram.list[[b]]$xty
      cyty.t = cyty.t + gram.list[[b]]$yty
    }
    sse.parent = calculate_sse_closed(cxtx.t, cxty.t, cyty.t, nrow(x), min.node.size, penalization)
   
    # calculate aggregated gram matrices for the first split (i.e. first bin in left node, rest in right node)
    cxtx.l = gram.list[[1]]$xtx
    cxtx.r = cxtx.t - cxtx.l 
    
    cxty.l = gram.list[[1]]$xty
    cxty.r = cxty.t - cxty.l
    
    cyty.l = gram.list[[1]]$yty
    cyty.r = cyty.t - cyty.l
    
    n.bin.l = gram.list[[1]]$n.bin
    n.bin.r = nrow(x) - n.bin.l
    sse.l = calculate_sse_closed(cxtx.l, cxty.l, cyty.l, n.bin.l, min.node.size, penalization)
    sse.r = calculate_sse_closed(cxtx.r, cxty.r, cyty.r, n.bin.r, min.node.size, penalization)
      
    sse.total = sse.l + sse.r

    splits = c(sse.total)
    
    
    # calculate the gram matrices and models of the remaining splits in a for loop
    if (n.bins > 2){
      for (s in 2:n.bins){
        cxtx.l = cxtx.l + gram.list[[s]]$xtx
        cxtx.r = cxtx.r - gram.list[[s]]$xtx
        cxty.l = cxty.l + gram.list[[s]]$xty
        cxty.r = cxty.r - gram.list[[s]]$xty
        cyty.l = cyty.l + gram.list[[s]]$yty
        cyty.r = cyty.r - gram.list[[s]]$yty
        
        n.bin.l = n.bin.l + gram.list[[s]]$n.bin
        n.bin.r = nrow(x) - n.bin.l
        
        sse.l = calculate_sse_closed(cxtx.l, cxty.l, cyty.l, n.bin.l, min.node.size, penalization)
        sse.r = calculate_sse_closed(cxtx.r, cxty.r, cyty.r, n.bin.r, min.node.size, penalization)
          
        sse.total = sse.l + sse.r
     
        splits = c(splits, sse.total)
      }
    }
  }
  
  # include parent.sse nur für Testzwecke
  if (include.parent.sse){
    return(list(parent.sse = sse.parent, splits = splits))
  } else {
    return(splits)
  }

}


perform_gram_splits_factor = function(gram.list,
                                      q,
                                      min.node.size, 
                                      penalization, 
                                      include.parent.sse = FALSE){
  
  splits = c()
  for(split in 1:length(q)){
    bins.l = as.character(q[[split]][[1]])
    bins.r = as.character(q[[split]][[2]])
    cxtx.l = gram.list[[bins.l[1]]]$xtx
    cxty.l = gram.list[[bins.l[1]]]$xty
    cyty.l = gram.list[[bins.l[1]]]$yty
    
    n.bin.l = gram.list[[bins.l[1]]]$n.bin
    
    if(length(bins.l)>1){
      for(b in 2:length(bins.l)){
        cxtx.l = cxtx.l + gram.list[[bins.l[b]]]$xtx
        cxty.l = cxty.l + gram.list[[bins.l[b]]]$xty
        cyty.l = cyty.l + gram.list[[bins.l[b]]]$yty
        n.bin.l = n.bin.l + gram.list[[bins.l[b]]]$n.bin
        
      }
    }
    
    cxtx.r = gram.list[[bins.r[1]]]$xtx
    cxty.r = gram.list[[bins.r[1]]]$xty
    cyty.r = gram.list[[bins.r[1]]]$yty
    
    n.bin.r = gram.list[[bins.r[1]]]$n.bin
    
    if(length(bins.r)>1){
      for(b in 2:length(bins.r)){
        cxtx.r = cxtx.r + gram.list[[bins.r[b]]]$xtx
        cxty.r = cxty.r + gram.list[[bins.r[b]]]$xty
        cyty.r = cyty.r + gram.list[[bins.r[b]]]$yty
        n.bin.r = n.bin.r + gram.list[[bins.r[b]]]$n.bin
      }
    }
    
    sse.l = calculate_sse_closed(cxtx.l, cxty.l, cyty.l, n.bin.l, min.node.size, penalization)
    sse.r = calculate_sse_closed(cxtx.r, cxty.r, cyty.r, n.bin.r, min.node.size, penalization)
    
    sse.total = sse.l + sse.r
    
    splits = c(splits, sse.total)
    
  }
  return(splits)
}




calculate_sse_closed = function(xtx, xty, yty, n.bin, min.node.size, penalization){
  if (n.bin >= min.node.size){
    if (is.null(penalization)) {
      try({beta = solve(xtx) %*% xty}, silent = TRUE)
        
      if (!is.matrix(beta)) {
        try({beta = as.matrix(MASS::ginv(xtx) %*% xty )}, silent = TRUE)
        if (!is.matrix(beta)) {
          return(Inf)
        }
      }
        
    } else if (penalization == "L2") {
      # Todo: CV to select optimal lambda
      diag.lambda = diag(nrow(xtx)) * lambda
      diag.lambda[1,1] = 0
      beta = solve(xtx + diag.lambda) %*% xty
    }
    
    sse = yty - 2 * t(beta) %*% xty + t(beta) %*% xtx %*% beta
  } else {
    sse = Inf
  }
  return(sse)
}


# helper functions for splitting

adjust_nsplits = function(xval, n.splits) {
  # max. number of splits to be performed must be unique.x-1
  unique.x = length(unique(xval))
  if (n.splits >= unique.x)
    n.splits = unique.x - 1
  return(n.splits)
}



# replace split.points with closest value from xval taking into account min.node.size
get_closest_point = function(split.points, xval, min.node.size = 10) {
  xval = sort.int(xval)
  # try to ensure min.node.size between points (is not guaranteed if many duplicated values exist)
  chunk.ind = seq.int(min.node.size + 1, length(xval) - min.node.size, by = min.node.size)
  # xadj = unique(xval[chunk.ind]) # unique(quantile(xval, prob = chunk.ind/length(xval), type = 1))
  xadj = unique(xval[-c(1:min.node.size, (length(xval)-min.node.size+1):length(xval))])
  
  # xval = xval[-c(1, length(xval))]
  split.adj = numeric(length(split.points))
  for (i in seq_along(split.adj)) {
    d = xadj - split.points[i]
    ind.closest = which.min(abs(d))
    split.adj[i] = xadj[ind.closest]
    xadj = xadj[-ind.closest] # remove already chosen value
  }
  
  return(sort.int(split.adj))
}

adjust_split_point = function(split.points, xval) {
  # use a value between two subsequent points
  q = split.points
  x.unique = sort.int(unique(xval))
  ind = which(x.unique %in% q)
  ind = ind[ind < length(x.unique)]
  if (length(ind) != length(q)) {
    eps = min(diff(x.unique))/2
  } else {
    eps = (x.unique[ind + 1] - x.unique[ind])/2
  }
  q = q + eps #+ c(diff(q)/2, 0)
  q[q < x.unique[2]] = mean(x.unique[1:2])
  q[q > x.unique[length(x.unique) - 1]] = mean(x.unique[(length(x.unique) - 1):length(x.unique)])
  #q = q[q <= max(xval) & q >= min(xval)]
  return(q)
}


# SLIM specific functions
# according to the slim paper
calculate_split_effects = function(term.predictions.parent, term.predictions, exclude = NULL){
  intersection = intersect(colnames(term.predictions.parent), colnames(term.predictions))
  if (!is.null(exclude)){
    intersection = intersection[intersection != exclude]
  } 
  if (length(intersection) == 1 | is.null(interaction)){
    return(intersection)
  } else{
    d = term.predictions.parent[,intersection] - term.predictions[,intersection]
    c = apply(d, MARGIN = 2, var)
    p = round(c/sum(c), 4)
    return(p)
  }
 
}

# objectives and fitting functions

get_model_lm = function(y, x, .family, .degree.poly, .fit.bsplines, .df.spline,
                        .exclude.categoricals, .type = "model", ...) {
  # browser()
  x = x %>% dplyr::select(where(~ n_distinct(.) > 1))
  
  if (.exclude.categoricals){
    x = x %>% dplyr::select(where(~ !is.factor(.)))
  }
  numeric.names = c()
  poly = c()
  splines = c()

  if (.degree.poly > 1) {
    numeric.names = names(x)[sapply(x,function(xval)(is.numeric(xval) & length(unique(xval)) > .degree.poly+1))]
    poly = paste0("poly(",numeric.names, ", degree =", .degree.poly,")")
  } else if (.fit.bsplines) {
    numeric.names = names(x)[sapply(x,function(xval)(is.numeric(xval)))]
    if(length(numeric.names)>0){
      splines = paste0("bs(", numeric.names, ", df = ", .df.spline, ", degree = 1)")  
    }
  }
  fm = as.formula(paste("y ~", paste(c(names(x)[!(names(x) %in% numeric.names)], poly, splines), collapse = "+")))
  data = cbind(y,x)
  model = lm(fm, data)
  if(.type == "fitted.values"){
    predictions = model$fitted.values
    return(predictions)
  } else{
    return(model)
  }
}

get_objective_lm = function(y, x, .family, .degree.poly, .fit.bsplines = FALSE, .df.spline = 15,
                            .exclude.categoricals, ...) {
  if (.exclude.categoricals){
    x = x %>% select(where(~ !is.factor(.)))
  }
  model = get_model_lm(y, x, .degree.poly = .degree.poly, 
                       .fit.bsplines = .fit.bsplines, .df.spline = .df.spline, .exclude.categoricals = .exclude.categoricals, .type = "model")
  sse = crossprod(model$residuals)
  return(sse)
}

get_prediction_lm= function(model, x, .exclude.categoricals, ...) {
  if (.exclude.categoricals){
    x = x %>% select(where(~ !is.factor(.)))
  }
  prediction = predict.lm(model, x, type = "terms")
  return(prediction)
}

get_model_glmnet = function(y, x, .family, .alpha, .degree.poly = 1, .exclude.categoricals, .lambda, .df.max, .type = "model", ...) {
  y = unlist(y)
  x = x %>% dplyr::select(where(~ n_distinct(.) > 1))
  if (.exclude.categoricals){
    x = x %>% select(where(~ !is.factor(.)))
  }
  if (.degree.poly > 1) {
    features = names(x)
    for(f in features){
      if (is.numeric(x[[f]]) & (length(unique(x[[f]])) > .degree.poly+1)){
        for(d in 2:.degree.poly){
          x = cbind(x, "new" = x[[f]]^d)
          colnames(x)[which(names(x) == "new")] = paste0(f,"_",d)
        }
      }
    }
  }
  factor.names = names(x)[sapply(x,class) == "factor"]
  if (length(factor.names) > 0){
    xfactors = model.matrix(as.formula(paste("y ~", paste(factor.names, collapse = "+"))), data = cbind(y,x))[, -1]
    x = as.matrix(data.frame(x[which(!(names(x) %in% factor.names))], xfactors))
  } else {
    x = as.matrix(x)
  }  
  if(is.null(.lambda)){
    fit = glmnet(x, y, nlambda=100)
    rss = deviance(fit)
    n = nrow(x)
    bic = n*log(rss/n) + log(n)*fit$df
    if(!is.null(.df.max)){
      # allow only lambda values which yield to df <= .df.max
      bic[fit$df > .df.max] = Inf
    } 
    lambda = fit$lambda[which.min(bic)]

  } else{
    if(!is.null(.df.max)){
      warning("df.max is ignored because lambda is not NULL. ")
    }
    lambda = .lambda
  }
  

  # cv.model = cv.glmnet(x, y, alpha = .alpha, family = .family)
  model = glmnet(x, y, alpha = .alpha, family = .family, lambda = lambda)
  if(.type == "fitted.values"){
    predictions = predict.glmnet(model, newx = x, s = lambda)
    return(predictions)
  } else{
    return(model)
  }
}

get_objective_glmnet = function(y, x, .family , .alpha, .degree.poly = 1, .exclude.categoricals, .lambda, .df.max, ...) {
  model = get_model_glmnet(y, x, .family = .family , .alpha = .alpha,
                           .degree.poly = .degree.poly, .exclude.categoricals, .exclude.categoricals = .exclude.categoricals,
                           .lambda = .lambda,
                           .df.max = .df.max, .type = "model")
  y = unlist(y)
  x = x %>% dplyr::select(where(~ n_distinct(.) > 1))
  if (.exclude.categoricals){
    x = x %>% select(where(~ !is.factor(.)))
  }
  if (.degree.poly > 1) {
    features = names(x)
    for(f in features){
      if (is.numeric(x[[f]]) & (length(unique(x[[f]])) > .degree.poly+1)){
        for(d in 2:.degree.poly){
          x = cbind(x, "new" = x[[f]]^d)
          colnames(x)[which(names(x) == "new")] = paste0(f,"_",d)
        }
      }
    }
  }
  factor.names = names(x)[sapply(x,class) == "factor"]
  if (length(factor.names) > 0){
    xfactors = model.matrix(as.formula(paste("y ~", paste(factor.names, collapse = "+"))), data = cbind(y,x))[, -1]
    x = as.matrix(data.frame(x[which(!(names(x) %in% factor.names))], xfactors))
  } else {
    x = as.matrix(x)
  } 
  
  predictions = predict.glmnet(model, newx = x, s = model$lambda)
  sse = sum((predictions - y)^2)
  return(sse)
}

get_prediction_glmnet = function(model, x, .exclude.categoricals, ...) {
  if (.exclude.categoricals){
    x = x %>% select(where(~ !is.factor(.)))
  }
  factor.names = names(x)[sapply(x,class) == "factor"]
  if (length(factor.names) > 0){
    xfactors = model.matrix(as.formula(paste("y ~", paste(factor.names, collapse = "+"))), data = cbind(y,x))[, -1]
    x = as.matrix(data.frame(x[which(!(names(x) %in% factor.names))], xfactors))
  } else {
    x = as.matrix(x)
  }  
  prediction = t(t(x)*as.vector(model$beta))
  return(prediction)
}

get_model_lad = function(y, x, .exclude.categoricals, .type = "model", ...){
  x = x %>% dplyr::select(where(~ n_distinct(.) > 1))
  if (.exclude.categoricals){
    x = x %>% dplyr::select(where(~ !is.factor(.)))
  }
  fm = as.formula(paste("y~", paste(names(x), collapse = "+")))
  data = cbind(y,x)
  model = rq(fm, data = data, tau = 0.5)
  if(.type == "fitted.values"){
    predictions = model$fitted.values
    return(predictions)
  } else{
    return(model)
  }
}

get_objective_lad = function(y, x, .exclude.categoricals,  ...){
  if (.exclude.categoricals){
    x = x %>% dplyr::select(where(~ !is.factor(.)))
  }
  model = get_model_lad(y, x, .exclude.categoricals = .exclude.categoricals, .type = "model")
  sae = sum(abs(model$residuals))
  return(sae)
}



get_model_gam = function(y, x, .family, .df.spline, .exclude.categoricals, .type = "model", ...) {
  x = x %>% dplyr::select(where(~ n_distinct(.) > 1))
  term = c()
  for (n in names(x)){
    newterm = n
    if (is.numeric(x[,n]) & length(unique(x[,n])) > 10){
      newterm = paste0("s(", n, ")")    
    }
    term = c(term, newterm)
  }
  fm = as.formula(paste("y ~", paste(term, collapse = "+")))
  data = cbind(y,x)
  model = gam(formula = fm, data = data, family = .family, method = "REML")
  if(.type == "fitted.values"){
    predictions = model$fitted.values
    return(predictions)
  } else{
    return(model)
  }
}

get_objective_gam = function(y, x, .family, .df.spline, .exclude.categoricals,  ...) {
  model = get_model_gam(y, x, .family = .family, .df.spline = .df.spline, .exclude.categoricals = .exclude.categoricals, .type = "model")
  sse = crossprod(residuals(model))
  return(sse)
}

get_prediction_gam = function(model, x, .exclude.categoricals, ...) {
  prediction = predict.gam(model, x, type = "terms")
  return(prediction)
}

# calculate r squared
r_2 = function(y_true, y_hat){
  y_true = unlist(y_true)
  y_hat = unlist(y_hat)
  rss = sum((y_hat - y_true) ^ 2)  ## residual sum of squares
  tss = sum((y_true - mean(y_true)) ^ 2)  ## total sum of squares
  r_2 = 1 - rss/tss
  return(r_2)
}


